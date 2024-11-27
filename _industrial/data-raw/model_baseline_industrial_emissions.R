### model early industrial emissions based on 2011-2022 GHGRP data 
### and MPCA 2005-2020 state inventoy

source("R/_load_pkgs.R")

mpca_industrial_inv <- readRDS(file.path(here::here(), "_meta/data/mpca_ghg_inv_2005_2020.RDS")) %>% 
  filter(Sector == "Industrial") %>% 
  clean_names()

ghgrp_emissions <- readRDS(file.path(here::here(), 
                                     "_industrial/data/ghgrp_industrial_point_sources_ctu.rds"))

subpart_c_emissions <- readRDS(file.path(here::here(), "_industrial/data/fuel_combustion_emissions.RDS"))
mpca_emissions <- readRDS(file.path(here::here(), "_industrial/data/mpca_fuel_emissions.RDS"))

ghgrp_emissions_combustion <- bind_rows(
  ghgrp_emissions %>% ungroup() %>% 
    filter(source != "stationary_combustion",
           doublecount == "No") %>% 
    select(inventory_year, facility_name, city_name, county_name,
           value_emissions,category, source),
  subpart_c_emissions %>% 
    mutate(category = "fuel_combustion") %>% ungroup() %>% 
    select(inventory_year = reporting_year, facility_name, city_name, county_name,
           value_emissions = values_emissions,
           category, source = general_fuel_type)
)
    
           
