#### This script processes EPA Air Emissions point data to account for airport emissions

source("R/_load_pkgs.R")
source("R/global_warming_potential.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")

epa_airport <- readRDS("_transportation/data-raw/epa/air_emissions_modeling/air_emissions_modeling_mn_wi/point/2022hd_airports_2022_point_top51_adjusted_ATL_data_20250428_27may2025_v0.RDS")
msp_emissions <- readRDS("_transportation/data/aviation_emissions.rds") %>% 
  filter(inventory_year == 2020)

epa_airport_cprg <- cprg_county %>% 
  st_drop_geometry() %>% 
  select(geoid, county_name) %>% 
  left_join(epa_airport %>% 
              select(region_cd, 
                     facility_id, 
                     unit_id, 
                     unit_type_code,
                     facility_name, 
                     scc, 
                     scc6, 
                     poll, 
                     emissions_short_tons,
                     calc_year),
            by = c("geoid" = "region_cd"))

epa_airport_cprg %>% filter(poll == "CO2") %>% pull(emissions_short_tons) %>% sum()

epa_airport_cprg %>% filter(poll == "CO2", grepl("Minneapolis", facility_name)) %>% pull(emissions_short_tons) %>% sum()
