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
    col_types = "c",
    show_col_types = FALSE
  )

# fetch emissions data -----
fetch_nei_county_sector <- function(year, state) {
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
      col_types =
      # specify each column type
        list(
          col_character(), # "state_name",
          col_number(), # "inventory_year",
          col_character(), # "state_fips",
          col_character(), # "pollutant_type",
          col_character(), # "uom",
          col_character(), # "county_name",
          col_character(), # "county_fips",
          col_number(), # "emissions",
          col_character(), # "sector_code",
          col_character(), # "pollutant_code",
          col_character() # "st_abbrv"
        ),
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
      fetch_nei_county_sector,
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
      fetch_nei_county_sector,
      state = "Wisconsin"
    )
  )

id_cols <- multi_year %>%
  mutate(
    geoid = paste0(state_fips, county_fips),
    cprg_area = ifelse(geoid %in% cprg_county$geoid, TRUE, FALSE),
    nei_inventory_year = inventory_year
  ) %>%
  select(
    state_name, nei_inventory_year,
    state_fips, county_fips, geoid, pollutant_type, uom,
    sector_code, pollutant_code, st_abbrv, cprg_area
  ) %>%
  unique()

# county level aggregations ----------
nei_county_multi_year <- multi_year %>%
  mutate(
    geoid = paste0(state_fips, county_fips),
    cprg_area = ifelse(geoid %in% cprg_county$geoid, TRUE, FALSE),
    nei_inventory_year = inventory_year
  ) %>%
  group_by(
    nei_inventory_year,
    geoid, pollutant_type, uom,
    sector_code, pollutant_code
  ) %>%
  summarise(emissions = sum(emissions), .groups = "keep") %>%
  ungroup() %>%
  left_join(sectors, by = c("sector_code")) %>%
  left_join(id_cols, by = join_by(
    nei_inventory_year, geoid,
    pollutant_type, uom, sector_code,
    pollutant_code
  )) %>%
  mutate(emissions_grams = emissions %>%
    units::as_units("ton") %>% # short tons/US tons
    units::set_units("metric_ton") %>%
    units::set_units("gram") %>% # convert to grams
    as.numeric())

nei_county <- nei_county_multi_year %>%
  filter(
    nei_inventory_year == max(nei_inventory_year),
    cprg_area == TRUE
  )


# state level aggregations -------------
nei_state_multi_year <- multi_year %>%
  mutate(nei_inventory_year = inventory_year) %>%
  group_by(
    state_name, nei_inventory_year,
    pollutant_type, uom, emissions,
    sector_code, pollutant_code
  ) %>%
  summarise(emissions = sum(emissions), .groups = "keep") %>%
  ungroup() %>%
  left_join(sectors, by = c("sector_code")) %>%
  left_join(
    id_cols %>%
      select(-geoid, -county_fips, -cprg_area) %>%
      unique(),
    by = join_by(
      state_name, nei_inventory_year,
      pollutant_type, uom, sector_code,
      pollutant_code
    )
  ) %>%
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


# # Tier summaries -----
# # Another option is to pull tier level summaries
# # sectors work for what we need to do for now,
# # but this is how you would go about pulling these directly
#
# tiers <- req_base %>%
#   httr2::req_url_path_append("TIERS/CSV") %>%
#   httr2::req_method("GET") %>%
#   httr2::req_perform() %>%
#   httr2::resp_body_string(encoding = "UTF-8") %>%
#   readr::read_delim(
#     delim = ",",
#     col_types =  "c",
#     show_col_types = FALSE
#   )
#
#
# fetch_nei_county_tier <- function(year, state) {
#   req_base %>%
#     # county sector summary table, all rows
#     httr2::req_url_path_append("EMISSIONS_TIER/ROWS/") %>%
#     # Minnesota only
#     httr2::req_url_path_append(paste0("STATE_NAME/", state)) %>%
#     # year 2020 inventory only
#     httr2::req_url_path_append("INVENTORY_YEAR/", year, "/") %>%
#     # in CSV format
#     httr2::req_url_path_append("CSV") %>%
#     # Go!
#     httr2::req_perform() %>%
#     # read response as CSV
#     httr2::resp_body_string(encoding = "UTF-8") %>%
#     readr::read_delim(
#       delim = ",",
#       # col_types =
#       #   # specify each column type
#       #   list(col_character(), # "state_name",
#       #        col_number(),    # "inventory_year",
#       #        col_character(), # "state_fips",
#       #        col_character(), # "pollutant_type",
#       #        col_character(), # "uom",
#       #        col_character(), # "county_name",
#       #        col_character(), # "county_fips",
#       #        col_number(),    # "emissions",
#       #        col_character(), # "sector_code",
#       #        col_character(), # "pollutant_code",
#       #        col_character()  # "st_abbrv"
#       #   ),
#       show_col_types = FALSE
#     )
# }
#
