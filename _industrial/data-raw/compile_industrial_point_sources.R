######### script for pulling industrial point source emissions from NEI ####
source("R/_load_pkgs.R")

cprg_county <- readRDS("_meta/data/cprg_county.RDS")

# base URL
req_base <- httr2::request("https://data.epa.gov/efservice")
# supplementary tables
# fetch sectors
sectors <- req_base %>%
  httr2::req_url_path_append("SECTORS/CSV") %>%
  httr2::req_method("GET") %>%
  httr2::req_perform() %>%
  httr2::resp_body_string(encoding = "UTF-8") %>%
  readr::read_delim(
    delim = ",",
    show_col_types = FALSE
  )

### The below code should work according to the nei-model, but doesn't. Unable to find source meta data for point source
# sources <- req_base %>%
#   httr2::req_url_path_append("BRS_SOURCE_NAME/CSV") %>%
#   httr2::req_method("GET") %>%
#   httr2::req_perform() %>%
#   httr2::resp_body_string(encoding = "UTF-8") %>%
#   readr::read_delim(
#     delim = ",",
#     show_col_types = FALSE
#   )

scc_metadata <- req_base %>%
  httr2::req_url_path_append("SCC/CSV") %>%
  httr2::req_method("GET") %>%
  httr2::req_perform() %>%
  httr2::resp_body_string(encoding = "UTF-8") %>%
  readr::read_delim(
    delim = ",",
    show_col_types = FALSE
  )

industrial_sector <- sectors %>% 
  filter(grepl("Industrial", ei_sector))

fetch_nei_point <- function(year, state, county) {
  req_base %>%
    # county sector summary table, all rows
    httr2::req_url_path_append("FACILITY_SUMMARY/ROWS/") %>%
    # Minnesota only
    httr2::req_url_path_append(paste0("STATE/", state)) %>%
    # county
    httr2::req_url_path_append(paste0("COUNTY/", county)) %>%
    # year 2020 inventory only
    httr2::req_url_path_append("INVENTORY_YEAR/", year, "/") %>%
    # in CSV format
    httr2::req_url_path_append("CSV") %>%
    # Go!
    httr2::req_perform() %>%
    # read response as CSV
    httr2::resp_body_string(encoding = "UTF-8") %>%
    readr::read_delim(
      delim = ",",
      show_col_types = FALSE
    )
}

multi_year_industrial_point <- purrr::map_dfr(
    2023,
    fetch_nei_point,
    state = "MN",
    county = "Dakota"
  )
### unclear if we can breakdown point sources to further categories, i.e. separate electricity and natural gas from further emissions
### API for source codes is broken and can't find what these mean
unique(multi_year_industrial_point$naics_desc)
unique(multi_year_industrial_point$source_code)

multi_year_industrial_point %>% distinct(site_name,source_code)
