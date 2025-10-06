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

### split out emissions by gas type
## start by pulling out sources with only fuel combustion as we'll get them in subpart c analysis

gas_cols <- colnames(ghgrp)[15:26]
source_cols <- colnames(ghgrp)[27:63]

refinery_gas <- ghgrp %>% 
  filter(grepl("Y", industry_type_subparts)) %>%  #refinery subpart
  pivot_longer(
    cols = all_of(gas_cols),
    names_to = "gas_type",
    values_to = "value_emissions"
  ) %>% 
  filter(!is.na(value_emissions)) %>% 
  select(inventory_year,
         facility_id,
         facility_name,
         primary_naics_code,
         industry_type_sectors,
         city_name = city,
         county_name = county,
         value_emissions,
         gas_type
  )

industrial_waste_gas <- ghgrp %>% 
  filter(!is.na(industrial_waste_landfills)) %>%  
  pivot_longer(
    cols = all_of(gas_cols),
    names_to = "gas_type",
    values_to = "value_emissions"
  ) %>% 
  filter(!is.na(value_emissions)) %>% 
  select(inventory_year,
         facility_id,
         facility_name,
         primary_naics_code,
         industry_type_sectors,
         city_name = city,
         county_name = county,
         value_emissions,
         gas_type
  )

# look at names of fluorinated gas emitters
fc_emitters <- ghgrp_gas %>%
  filter(gas_type %in% c("nf3", "hfc", "pfc", "sf6", "other_fully_fluorinated_ghg")) %>%
  select(inventory_year,
         facility_id,
         facility_name,
         primary_naics_code,
         industry_type_sectors,
         city_name = city,
         county_name = county,
         value_emissions,
         gas_type
  ) %>%
  mutate(
    unit_emissions = "Metric tons CO2e",
    county_name = str_to_title(county_name),
    city_name = str_to_title(city_name)
  )

ng_transmission <- ghgrp %>% 
  filter(facility_name == "Farmington Compressor Station (Farmington, MN)")


lead_production  <- ghgrp %>% 
  filter(facility_name == "GOPHER RESOURCE LLC")
  
  ghgrp_gas <- ghgrp %>%
  select(1:26, 66) %>%
  pivot_longer(
    cols = 14:26,
    names_to = "gas_type",
    values_to = "value_emissions"
  ) %>%
  filter(!is.na(value_emissions)) %>%
  mutate(gas_type = str_replace_all(gas_type, "_emissions", ""))
# not doing anything with this for now as it can't clearly be broken down by source
# potentially interesting for later
# March 2025 update: Going to recover gas emissions from process emissions here (refineries, other)
# Gas emissions from fuel combustion are recovered in 'compile_fuel_combustion.R'
# October 2025 update: new attempt to breakout gas types by various industrial subsectors
# need to be separated by fuel combustion, so utility double count can be filtered out.

fh <- ghgrp %>% filter(inventory_year == 2022, facility_name == "Flint Hills Resources Pine Bend Refinery")

tapply(ghgrp_gas$value_emissions, ghgrp_gas$gas_type, sum)

fc_emitters %>% distinct(facility_name)
# 7 total, 3m and superconductor manufacturers

# saving fluorinated gas data


