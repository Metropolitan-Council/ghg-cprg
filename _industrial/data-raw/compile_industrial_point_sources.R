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

industrial_sector <- sectors %>% 
  filter(grepl("Industrial", ei_sector))

fetch_nei_county <- function(year, state) {
  req_base %>%
    # county sector summary table, all rows
    httr2::req_url_path_append("COUNTY_SECTOR_SUMMARY/ROWS/") %>%
    # state
    httr2::req_url_path_append(paste0("STATE_NAME/", state)) %>%
    # year
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

multi_year_industrial_county <-
  bind_rows(
    purrr::map_dfr(
      c(2023,
        2020,
        2017,
        2014,
        2011,
        2008
      ),
      fetch_nei_county,
      state = "Minnesota"
    ),
    purrr::map_dfr(
      c(2023,
        2020,
        2017,
        2014,
        2011,
        2008
      ),
      fetch_nei_county,
      state = "Wisconsin"
    )
  ) %>% 
  filter(sector_code %in% industrial_sector$sector_code) %>% 
  right_join(., cprg_county %>% 
               select(state_name,county_name,geoid) %>% 
               st_drop_geometry(),
             by = c("county_name","state_name"))
      
multi_year_industrial_point <- purrr::map_dfr(
    2020,
    fetch_nei_point,
    state = "MN",
    county = "Dakota"
  )

unique(multi_year_industrial_point$naics_desc)
