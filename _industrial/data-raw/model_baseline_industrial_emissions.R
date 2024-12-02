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
) %>% 
  mutate(city_name = str_to_title(city_name))

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



### create six categories - ind process, ref process, nat gas, oil, coal, other fuel combustion
ghgrp_simplified <- ghgrp_emissions_combustion %>% 
  mutate(mpca_subsector = case_when(
    source %in% c( "fluorinated_ghg_production",
                   "glass_production",
                   "iron_and_steel_production",
                   "lead_production",
                   "magnesium_production",
                   "electronics_manufacture",
                   "petroleum_and_natural_gas_systems_transmission_compression") ~ 
      "Industrial processes",
    source %in% c("Fuel Gas", "Natural Gas") ~ "Natural gas", #seems likely to be grouped based on descriptions and magnitude of emissions
    source %in% c("hydrogen_production", #large emissions, only happens in refineries, best guess here for matching MPCA
                  "petroleum_refining") ~ "Refinery processes", 
    source == "Petroleum Products" ~ "Oil",
    source %in% c("Agricultural Byproducts","Wood and Wood Residuals") ~ "Other fuel combustion", #best worst option?
    source == "industrial_waste_landfills" ~ "Landfills", #MPCA technical confirms industrial waste emission is in this category
    TRUE ~ source
  )) %>% 
  filter(mpca_subsector != "Municipal Solid Waste") %>% 
  group_by(inventory_year, city_name, county_name, mpca_subsector) %>% 
  summarize(value_emissions_ghgrp = sum(value_emissions))

## taking a slightly different approach with MPCA, leaving those with non-obvious GHGRP correlates untransformed
mpca_simplified <- mpca_industrial_inv %>% 
  mutate(mpca_subsector = case_when(
    Subsector %in% c( "Industrial processes",
                      "Glass manufacture",
                      "Steel production",
                      "Secondary lead production",
                      "Magnesium casting",
                      "Semiconductor manufacture") ~ "Industrial processes",
    Subsector %in% c("Refinery processes", 
                     "Oil refining") ~ "Refinery processes", 
    Subsector %in% c("Other fossil fuels") ~ "Other fuel combustion", #best worst option?
    TRUE ~ Subsector
  )) %>% 
  group_by(year, mpca_subsector) %>% 
  summarize(value_emissions_mpca = sum(co2e))

ghgrp_mpca_emissions <-  
  left_join(ghgrp_simplified,mpca_simplified,
             by = c("inventory_year" = "year",
                    "mpca_subsector")) %>% 
  mutate(emission_percent = value_emissions_ghgrp/value_emissions_mpca,
         emission_percent = if_else(is.infinite(emission_percent),NA, emission_percent))

### create grid of needed city-subsector-year combinations
ghgrp_extrapolated <- left_join(
  expand.grid(
    inventory_year = seq(2005, 2020, by = 1),
    city_name = unique(ghgrp_mpca_emissions$city_name),
    mpca_subsector = unique(ghgrp_mpca_emissions$mpca_subsector)
  ) %>% 
    semi_join(., ghgrp_mpca_emissions %>% 
                ungroup() %>%  
                distinct(city_name, mpca_subsector)),
  ghgrp_mpca_emissions %>% 
    ungroup() %>% 
    select(inventory_year, city_name, mpca_subsector, value_emissions_ghgrp, emission_percent),
  by = c("inventory_year","city_name", "mpca_subsector")) %>% 
  ### use na.kalman to extrapolate across time-series
  group_by(city_name, mpca_subsector) %>%
  arrange(inventory_year) %>%
  #first add zeros to 2011-2020 years if there aren't enough years(3+) for extrapolation
  mutate(non_na_count_2011_2020 = sum(!is.na(emission_percent) & inventory_year >= 2011 & inventory_year <= 2020, na.rm = TRUE)) %>%
  # Set NA to zero only if there are 1 or 2 years of data in the range
  mutate(emission_percent = if_else(
    is.na(emission_percent) & inventory_year >= 2011 & inventory_year <= 2020 & non_na_count_2011_2020 <= 2,
    0,
    emission_percent
  )) %>% 
  #extrapolate
  mutate(
    emission_percent = na_kalman(emission_percent),
    data_type = ifelse(is.na(value_emissions_ghgrp), "modeled", "measured") # marking whether values are from the census or interpolation
  ) %>% 
  
### bring mn state emissions back in
  left_join(., mpca_simplified,
            by = c("inventory_year" = "year",
                   "mpca_subsector")
  ) %>% 
  # recalculate emissions based on percent of MPCA inventory
  mutate(value_emission_percentile = emission_percent * value_emissions_mpca) %>% 
  #bring back county_name
  left_join(., ghgrp_simplified %>% ungroup() %>% distinct(city_name,county_name))

      
ghgrp_extrapolated_county <- ghgrp_extrapolated %>% 
  group_by(inventory_year, county_name) %>% 
  summarize(value_emissions = sum(value_emission_percentile))

ggplot(ghgrp_extrapolated_county, aes(x = inventory_year, y = value_emissions, col = county_name)) + geom_line()
      
mutate(
  msp_mt_co2e_impute = na_kalman(msp_mt_co2e),
  msp_proportion_impute = na_kalman(msp_proportion),
  # second methods uses time series imputation between missing msp_proportion values,
  # then multiplies imputed proportion by state value
  msp_mt_co2e_state_prop = if_else(is.na(msp_mt_co2e),
                                   msp_proportion_impute * state_mt_co2e,
                                   msp_mt_co2e
  ),

  
  #deprecated code to match subsectors more finely (lots of numerical issues)
  # ghgrp_mpca_emissions <- ghgrp_emissions_combustion %>% 
  #   mutate(mpca_subsector = case_when(
  #     source %in% c("Agricultural Byproducts",
  #                   "fluorinated_ghg_production") ~ "Industrial processes",
  #     source == "Fuel Gas" ~ "Natural Gas", #seems likely to be grouped based on descriptions and magnitude of emissions
  #     source == "glass_production" ~ "Glass manufacture",
  #     source == "hydrogen_production" ~ "Refinery processes", #large emissions, only happens in refineries, best guess here
  #     source == "iron_and_steel_production" ~ "Steel production", 
  #     source == "lead_production" ~ "Secondary lead production", 
  #     source == "magnesium_production" ~ "Magnesium casting",
  #     source == "Petroleum Products" ~ "Oil",
  #     source == "petroleum_refining" ~ "Oil refining",
  #     source == "Wood and Wood Residuals" ~ "Other fossil fuels", #best worst option?
  #     source == "industrial_waste_landfills" ~ "Landfills", #MPCA technical confirms industrial waste emission is in this category
  #     source == "electronics_manufacture" ~ "Semiconductor manufacture",
  #     TRUE ~ source
  #   )) %>% 
  #   group_by(inventory_year, city_name, county_name, mpca_subsector) %>% 
  #   summarize(value_emissions_ghgrp = sum(value_emissions)) %>% 
  #   right_join(mpca_industrial_inv,
  #              by = c("inventory_year" = "year",
  #                     "mpca_subsector" = "Subsector")) %>% 
  #   mutate(emission_percent = value_emissions_ghgrp/co2e,
  #          emission_percent = if_else(is.infinite(emission_percent),NA, emission_percent))
  # 
