source("R/_load_pkgs.R")

## This module estimates methane (CH4) emissions from municipal wastewater
epa_wastewater_constants <- readRDS("_waste/data-raw/wastewater/epa/epa_wastewater_constants.rds")

calculate_mww_ch4_emissions <- function(population, year) {
  # Define constants for the current year
  days_per_year <- ifelse(lubridate::leap_year(year), 366, 365)
  
  get_epa_wastewater_constant <- function(variable_name) {
    epa_wastewater_constants %>%
      filter(short_text == variable_name) %>%
      pull(value)
  }
  
  # Grab constants
  per_capita_BOD5 <- get_epa_wastewater_constant("Per_capita_BOD5")
  MT_per_kg <- get_epa_wastewater_constant("MT_per_kg")
  Emission_Factor_CH4_BOD5 <- get_epa_wastewater_constant("Emission_Factor_CH4_BOD5")
  Fraction_BOD5_anaerobically_digested <- get_epa_wastewater_constant("Fraction_BOD5_anaerobically_digested")
  CH4_GWP <- get_epa_wastewater_constant("CH4_GWP")
  MMT_per_MT <- get_epa_wastewater_constant("MMT_per_MT")
  
  # Calculate methane emissions
  emissions_metric_tons_CH4 <- population * per_capita_BOD5 * days_per_year * MT_per_kg * Emission_Factor_CH4_BOD5 * Fraction_BOD5_anaerobically_digested

  # Create data frame
  df <- data.frame(sector="Waste",
                   category="Wastewater",
                   source="Municipal_CH4",
                   data_source="EPA State Inventory Tool - Wastewater Module",
                   population = population, inventory_year = year, 
                   value_emissions = emissions_metric_tons_CH4,
                   units_emissions = "Metric tons CH4") 
  
  
  return(df)
  
}

rm(epa_wastewater_constants)

# calculate_mww_ch4_emissions(population = 4904562, year = 1990)



