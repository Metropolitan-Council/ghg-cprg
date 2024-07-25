# Fetch data from the 2020 national emissions inventory
# NEI data prior to 2008 are unavailable
# 
# This script does not save any outputs, but is intended to be 
# called through source() within another modules and resulting datasets
# (nei_county, nei_state, nei_state_multi_year, nei_county_multi_year) 
# can filtered as needed. 


# API graphic model here: https://www.epa.gov/enviro/nei-model
source("R/_load_pkgs.R")
library(httr2)
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
    col_types =  "c",
    show_col_types = FALSE
  )

fetch_nei <- function(year, state) {
  req_base %>%
    # county sector summary table, all rows
    httr2::req_url_path_append("COUNTY_SECTOR_SUMMARY/ROWS/") %>%
    # Minnesota only
    httr2::req_url_path_append(paste0("STATE_NAME/", state)) %>%
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

# pull count-level data for all available years
multi_year <-
  bind_rows(
    purrr::map_dfr(
      c(
        2020,
        2017,
        2014,
        2011,
        2008
      ),
      fetch_nei,
      state = "Minnesota"
    ),
    purrr::map_dfr(
      c(
        2020,
        2017,
        2014,
        2011,
        2008
      ),
      fetch_nei,
      state = "Wisconsin"
    )
  )

# county level aggregations ----------
nei_county_multi_year <- multi_year %>%
  mutate(GEOID = paste0(state_fips, county_fips),
         cprg_area = ifelse(GEOID %in% cprg_county$GEOID, TRUE, FALSE),
         nei_inventory_year = inventory_year) %>%
  group_by(
    state_name, nei_inventory_year,
    state_fips, county_fips, GEOID, pollutant_type, uom, emissions,
    sector_code, pollutant_code, st_abbrv, cprg_area
  ) %>%
  summarise(emissions = sum(emissions), .groups = "keep") %>%
  filter(pollutant_type == "GHG") %>%
  left_join(sectors, by = c("sector_code")) %>%
  mutate(emissions_grams = emissions %>%
           units::as_units("ton") %>% # short tons/US tons
           units::set_units("metric_ton") %>% 
           units::set_units("gram") %>% # convert to grams
           as.numeric()) %>%
  rowwise()

nei_county <- nei_county_multi_year %>% 
  filter(nei_inventory_year == max(nei_inventory_year),
         cprg_area == TRUE)


# state level aggregations -------------
nei_state_multi_year <- multi_year %>%
  mutate(nei_inventory_year = inventory_year) %>% 
  group_by(
    state_name, nei_inventory_year,
    state_fips, pollutant_type, uom, emissions,
    sector_code, pollutant_code, st_abbrv
  ) %>%
  summarise(emissions = sum(emissions), .groups = "keep") %>%
  filter(pollutant_type == "GHG") %>%
  left_join(sectors, by = c("sector_code")) %>%
  mutate(emissions_grams = emissions %>%
           units::as_units("ton") %>% # short tons/US tons
           units::set_units("metric_ton") %>% 
           units::set_units("gram") %>% # convert to grams
           as.numeric()) %>%
  rowwise()


# state aggregation, most recent year only
nei_state <- nei_state_multi_year %>%
  ungroup() %>% 
  filter(nei_inventory_year == max(nei_inventory_year))
