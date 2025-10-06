#### Script to read in and process EPA GHG FLIGHT data
source("R/_load_pkgs.R")

cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_ctu <- readRDS("_meta/data/cprg_ctu.RDS")

### download flight data: https://ghgdata.epa.gov/ghgp/main.do
ghgrp_files <- list.files(file.path(here::here(), "_industrial/data-raw/ghgrp"))


#### There is no EPA flight records for Pierce or St. Croix counties in WI

ghgrp <- lapply(as.character(2011:2023), function(y) {
  read_excel(
    file.path(
      here::here(),
      paste0(
        "_industrial/data-raw/ghgrp/ghgp_data_",
        y,
        ".xlsx"
      )
    ),
    sheet = 1,
    skip = 3
  ) %>%
    clean_names() %>%
    filter(state %in% c("MN", "WI")) %>%
    select(-(last_col(offset = 2):last_col())) %>%
    mutate(county_name = str_remove(str_to_title(county), " County")) %>%
    filter(county_name %in% cprg_county$county_name) %>%
    mutate(
      unit_emissions = "Metric tons of CO2e",
      inventory_year = y
    )
}) %>%
  bind_rows()


### split out by emissions source (mt co2e)
ghgrp_source <- ghgrp %>%
  select(-c(14:26)) %>%
  pivot_longer(
    cols = 14:50,
    names_to = "emission_source",
    values_to = "value_emissions",
    values_transform = list(value_emissions = as.numeric)
  ) %>%
  filter(!is.na(value_emissions)) %>%
  mutate(
    doublecount = if_else(emission_source %in% c(
      "electricity_generation",
      "municipal_landfills"
    ),
    "Yes", "No"
    ),
    city_name = str_to_sentence(city),
    inventory_year = as.numeric(inventory_year),
    category = case_when(
      emission_source == "stationary_combustion" ~ "stationary_combustion",
      emission_source == "electricity_generation" ~ "electricity_generation",
      emission_source == "municipal_landfills" ~ "municipal_landfills",
      TRUE ~ "industrial_processes"
    )
  )

cprg_ghgrp_out <- ghgrp_source %>%
  select(inventory_year,
    facility_id,
    facility_name,
    latitude,
    longitude,
    primary_naics_code,
    city_name,
    county_name,
    state,
    value_emissions,
    doublecount,
    category,
    source = emission_source
  ) %>%
  mutate(
    unit_emissions = "Metric tons CO2e",
    sector = "Industrial",
    data_source = "EPA GHGRP",
    factor_source = "EPA GHGRP (no activity data reported)"
  )


cprg_ghgrp_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "inventory_year", class(cprg_ghgrp_out$inventory_year), "Year of survey",
    "city_name", class(cprg_ghgrp_out$city_name), "City name",
    "county_name", class(cprg_ghgrp_out$city_name), "County name",
    "state", class(cprg_ghgrp_out$state), "State name",
    "facility_id", class(cprg_ghgrp_out$facility_id), "Facility ID in GHG Reporting Program",
    "facility_name", class(cprg_ghgrp_out$facility_name), "Facility name",
    "primary_naics_code", class(cprg_ghgrp_out$primary_naics_code), "NAICS code for facility",
    "latitude", class(cprg_ghgrp_out$latitude), "Latitude of industrial source",
    "longitude", class(cprg_ghgrp_out$longitude), "Longitude of industrial source",
    "sector", class(cprg_ghgrp_out$sector), "Emissions sector",
    "category", class(cprg_ghgrp_out$category), "Category of emissions within given sector",
    "source", class(cprg_ghgrp_out$source), "Source of emissions. Most detailed sub-category in this table",
    "data_source", class(cprg_ghgrp_out$data_source), "Activity data source",
    "factor_source", class(cprg_ghgrp_out$factor_source), "Emissions factor data source",
    "value_emissions", class(cprg_ghgrp_out$value_emissions), "Numerical value of emissions",
    "unit_emissions", class(cprg_ghgrp_out$unit_emissions), "Units and gas type of emissions",
    "doublecount", class(cprg_ghgrp_out$doublecount), "Is this emission counted in another sector?"
  )

saveRDS(cprg_ghgrp_out, "./_industrial/data/ghgrp_industrial_point_sources_ctu.rds")
saveRDS(cprg_ghgrp_meta, "./_industrial/data/ghgrp_industrial_point_sources_ctu_meta.rds")
