#### Script to read in and process EPA GHG FLIGHT data
source("R/_load_pkgs.R")
source("R/global_warming_potential.R")

cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_ctu <- readRDS("_meta/data/cprg_ctu.RDS")

## industrial factor hub
industrial_hub <- readRDS("_meta/data/epa_ghg_factor_hub.RDS") %>%
  extract2("industrial_combustion") %>%
  clean_names()

### download flight data: https://ghgdata.epa.gov/ghgp/main.do
ghgrp_files <- list.files(file.path(here::here(), "_industrial/data-raw/ghgrp"))

#subpart_c emissions
ind_fuel_combustion <- readRDS("./_industrial/data/fuel_combustion_emissions_by_gas.rds")

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
  bind_rows() %>% 
  # remove sources/gases where there is only NAs
  select(where(~ !all(is.na(.)))) %>% 
  mutate(petroleum_and_natural_gas_systems_transmission_compression = 
           as.numeric(petroleum_and_natural_gas_systems_transmission_compression))

### split out emissions by gas type
## start by pulling out sources with only fuel combustion as we'll get them in subpart c analysis

gas_cols <- colnames(ghgrp)[15:26]
source_cols <- colnames(ghgrp)[27:39]

## look at crosstabs of emissions sources (i.e. does everything also have stationary combustion?)
emission_sources <- ghgrp %>%
  select(stationary_combustion:industrial_waste_landfills) %>% 
  mutate(across(everything(), ~ as.integer(!is.na(.) & . > 0))) %>% 
  as.matrix()

cross_counts <- t(emission_sources) %*% emission_sources
cross_counts[1:10, 1:10] 
### everything has stationary combustion which will need to be subtracted away to deal with
### utility natural gas delivery

## pulling out refinery emissions

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

#### non-refinery industrial ####

ghgrp_other <- ghgrp %>% 
  filter(!grepl("Y", industry_type_subparts), #remove refineries
         !grepl("D", industry_type_subparts), #remove power plants
         !(grepl("Waste", industry_type_sectors) & is.na(industrial_waste_landfills))) %>%   #remove municipal waste
  # remove facilities with only stationary combustion
  filter(
    rowSums(
      across(electricity_generation:industrial_waste_landfills,
             ~ as.integer(!is.na(.) & . > 0))
    ) > 0)
  
## convert ghgrp_other to mt of gas from mt_co2e
ghgrp_gas <- ghgrp_other %>%
  select(1:5,11:26, 40:42) %>%
  pivot_longer(
    cols = all_of(gas_cols),
    names_to = "gas_type",
    values_to = "value_emissions"
  ) %>%
  filter(!is.na(value_emissions)) %>%
  mutate(gas_type = str_replace_all(gas_type, "_emissions", ""),  
  mt_gas = case_when(
    gas_type == "co2_non_biogenic" ~ value_emissions / gwp$co2,
    gas_type == "biogenic_co2_metric_tons" ~ value_emissions / gwp$co2,
    gas_type == "methane_ch4" ~ value_emissions / 25, # they used IPCC 4th assessment values, so can't use stored values
    gas_type == "nitrous_oxide_n2o" ~ value_emissions /298, # they used IPCC 4th assessment values, so can't use stored values
    gas_type == "sf6" ~ value_emissions / gwp$sf6,
    gas_type == "nf3" ~ value_emissions / gwp$nf3,
    gas_type == "hfc" ~ value_emissions / gwp$hfc,
    gas_type == "pfc" ~ value_emissions / gwp$cf4,
    gas_type == "other_fully_fluorinated_ghg" ~ value_emissions / ((gwp$cf4 + gwp$nf3) / 2), # best guess for what these might be based on NAICS codes
    gas_type == "hfe" ~ value_emissions / 200, # ChatGPT estimate based on HFE-7xxx compounds, typically used in electronics manufacture
    TRUE ~ NA # unID'ed compounds - nothing to be done if we don't know what they are
    ))

### subtract away combustion emissions

ghgrp_process <- left_join(
  ghgrp_gas %>% 
    group_by(facility_id, county_name, city, inventory_year, gas_type ) %>% 
    summarize(mt_gas = sum(mt_gas)) %>% 
    ungroup(),
  ind_fuel_combustion %>% 
    group_by(facility_id, county_name, city_name, reporting_year, units_emissions) %>% 
    summarize(mt_gas = sum(values_emissions)) %>% 
    ungroup(),
  by
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

mutate(
  mt_gas = case_when(
    gas_type == "sf6" ~ value_emissions / gwp$sf6,
    gas_type == "nf3" ~ value_emissions / gwp$nf3,
    gas_type == "hfc" ~ value_emissions / gwp$hfc,
    gas_type == "pfc" ~ value_emissions / gwp$cf4,
    gas_type == "other_fully_fluorinated_ghg" ~ value_emissions / ((gwp$cf4 + gwp$nf3) / 2) # best guess for what these might be based on NAICS codes
  ),
  units_emissions = paste("Metric tons fluorocarbons"),
  emissions_year = as.numeric(inventory_year)
) %>%
  filter(emissions_year == 2022) %>%
  group_by(emissions_year, county_name, units_emissions) %>%
  summarize(
    metric_tons_co2e = sum(value_emissions),
    value_emissions = sum(mt_gas)
  ) %>%
  ungroup() %>%
  mutate(sector = "Industrial")

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


