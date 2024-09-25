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


multi_year_industrial_county_ghg <- multi_year_industrial_county %>% 
  filter(pollutant_type == "GHG", emissions != 0) %>% 
  left_join(sectors)

unique(multi_year_industrial_county_ghg$ei_sector)

multi_year_industrial_county_ghg %>% 
  filter(inventory_year == 2020, pollutant_code == "CO2") %>% 
  group_by(county_name) %>% 
  summarize(tons_co2 = sum(emissions))

multi_year_industrial_county_ghg %>% 
  filter(inventory_year == 2020, pollutant_code == "CO2",
         county_name == "Washington") %>% 
  group_by(ei_sector, sector_code) %>% 
  summarize(tons_co2 = sum(emissions))
#### NEC can, and almost certainly does for Sherburne, include electricity generation
