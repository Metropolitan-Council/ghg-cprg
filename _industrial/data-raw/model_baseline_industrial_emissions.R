### model early industrial emissions based on 2011-2022 GHGRP data 
### and MPCA 2005-2020 state inventoy

source("R/_load_pkgs.R")

mpca_industrial_inv <- readRDS(file.path(here::here(), "_meta/data/mpca_ghg_inv_2005_2020.RDS")) %>% 
  filter(Sector %in% c("Waste", "Industrial"))

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
    mutate(category = "fuel_combustion",
           source = if_else(general_fuel_type == "Other",
                            specific_fuel_type, general_fuel_type)) %>% 
    ungroup() %>% 
    select(inventory_year = reporting_year, facility_name, city_name, county_name,
           value_emissions = values_emissions,
           category, source)
)

### match ghgrp emission categories to mpca subsectors as much as possible    
sort(unique(mpca_industrial_inv$Subsector))           
sort(unique(ghgrp_emissions_combustion$source))           

#refinery is a constant problem in these comparisons
subpart_c_emissions %>% 
  filter(facility_name == "Flint Hills Resources Pine Bend Refinery",
         reporting_year == 2020) %>% ungroup() %>%  select(unit_name,general_fuel_type, values_emissions)

ghgrp_emissions %>% 
  filter(facility_name == "Flint Hills Resources Pine Bend Refinery",
         inventory_year == 2020) %>% ungroup() %>%  select(category, source, value_emissions)

mpca_industrial_inv %>% filter(year == 2020, co2e > 0) 


ghgrp_mpca_emissions <- ghgrp_emissions %>% 
  mutate(mpca_subsector = case_when(
    source %in% c("Agricultural Byproducts",
                  "fluorinated_ghg_production") ~ "Industrial processes",
    source == "Fuel Gas" ~ "Natural Gas", #seems likely to be grouped based on descriptions and magnitude of emissions
    source == "glass_production" ~ "Glass manufacture",
    source == "hydrogen_production" ~ "Refinery processes", #large emissions, only happens in refineries, best guess here
    source == "iron_and_steel_production" ~ "Steel production", 
    source == "lead_production" ~ "Secondary lead production", 
    source == "magnesium_production" ~ "Magnesium casting",
    source == "Petroleum Products" ~ "Oil",
    source == "petroleum_refining" ~ "Oil refining",
    source == "Wood and Wood Residuals" ~ "Other fossil fuels", #best worst option?
    source == "industrial_waste_landfills" ~ "Landfills", #MPCA technical confirms industrial waste emission is in this category
    source == "electronics_manufacture" ~ "Semiconductor manufacture",
    TRUE ~ source
  )) %>% 
  group_by(inventory_year, city_name, county_name, mpca_subsector) %>% 
  summarize(value_emissions_ghgrp = sum(value_emissions)) %>% 
  right_join(mpca_industrial_inv,
             by = c("inventory_year" = "year",
                    "mpca_subsector" = "Subsector")) %>% 
  mutate(emission_percent = value_emissions_ghgrp/co2e,
         emission_percent = if_else(is.infinite(emission_percent),NA, emission_percent))

mutate(
  msp_mt_co2e_impute = na_kalman(msp_mt_co2e),
  msp_proportion_impute = na_kalman(msp_proportion),
  # second methods uses time series imputation between missing msp_proportion values,
  # then multiplies imputed proportion by state value
  msp_mt_co2e_state_prop = if_else(is.na(msp_mt_co2e),
                                   msp_proportion_impute * state_mt_co2e,
                                   msp_mt_co2e
  ),
