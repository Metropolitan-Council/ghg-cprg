# get data from the 2020 national emissions inventory
# graphic model here: https://www.epa.gov/enviro/nei-model
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
    show_col_types = FALSE
  )


fetch_nei <- function(year, state){
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

multi_year <- 
  bind_rows(
    purrr::map_dfr(
      c(2020,
        2017,
        2014,
        2011,
        2008),
      fetch_nei,
      state = "Minnesota"),
    purrr::map_dfr(
      c(2020,
        2017,
        2014,
        2011,
        2008),
      fetch_nei,
      state = "Wisconsin")
  ) 

nei_county_multi_year <- multi_year %>% 
  group_by(
    state_name, inventory_year,
    state_fips, county_fips, pollutant_type, uom, emissions,
    sector_code, pollutant_code, st_abbrv
  ) %>%
  summarise(emissions = sum(emissions), .groups = "keep") %>%
  filter(pollutant_type == "GHG") %>%
  left_join(sectors, by = c("sector_code")) %>%
  mutate(emissions_grams = emissions %>%
           units::as_units("ton") %>% # short tons/US tons
           units::set_units("metric_ton") %>% # convert to grams
           as.numeric()) %>% 
  mutate(GEOID = paste0(state_fips, county_fips)) %>%
  filter(
    GEOID %in% cprg_county$GEOID
  ) %>%
  rowwise()

nei_state_multi_year <- multi_year %>% 
  group_by(
    state_name, inventory_year,
    state_fips, pollutant_type, uom, emissions,
    sector_code, pollutant_code, st_abbrv
  ) %>%
  summarise(emissions = sum(emissions), .groups = "keep") %>%
  filter(pollutant_type == "GHG") %>%
  left_join(sectors, by = c("sector_code")) %>%
  mutate(emissions_grams = emissions %>%
           units::as_units("ton") %>% # short tons/US tons
           units::set_units("metric_ton") %>% # convert to grams
           as.numeric()) %>% 
  rowwise()


# state aggregation, most recent year
nei_state <- bind_rows(
  fetch_nei(2020, "Minnesota"),
  fetch_nei(2020, "Wisconsin")
) %>%
  group_by(
    state_name, inventory_year,
    state_fips, pollutant_type, uom, emissions,
    sector_code, pollutant_code, st_abbrv
  ) %>%
  summarise(emissions = sum(emissions), .groups = "keep") %>%
  filter(pollutant_type == "GHG") %>%
  left_join(sectors, by = c("sector_code")) %>%
  mutate(emissions_grams = emissions %>%
           units::as_units("ton") %>% # short tons/US tons
           units::set_units("metric_ton") %>% # convert to grams
           as.numeric())




# combine MN and WI, counties
# filter to only CPRG counties
nei_county <- bind_rows(
  fetch_nei(2020, "Minnesota"),
  fetch_nei(2020, "Wisconsin")
) %>%
  mutate(GEOID = paste0(state_fips, county_fips)) %>%
  filter(
    GEOID %in% cprg_county$GEOID,
    pollutant_type == "GHG"
  ) %>%
  left_join(sectors, by = c("sector_code")) %>%
  rowwise()
