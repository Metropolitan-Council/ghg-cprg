source("R/_load_pkgs.R")

## This module estimates direct nitrous oxide (N2O) emissions from municipal wastewater
epa_wastewater_constants <- readRDS("_waste/data-raw/wastewater/epa/epa_wastewater_constants.rds")

calculate_mww_n2o_direct_emissions <- function(population, year) {
  # Define constants for the current year
  days_per_year <- ifelse(lubridate::leap_year(year), 366, 365)
  
  get_epa_wastewater_constant <- function(variable_name) {
    epa_wastewater_constants %>%
      filter(short_text == variable_name) %>%
      pull(value)
  }
  
  # Grab constants
  Fraction_population_not_on_septic <- get_epa_wastewater_constant("Fraction_population_not_on_septic")
  Direct_wwtp_emissions <- get_epa_wastewater_constant("Direct_wwtp_emissions")
  g_per_MT <- get_epa_wastewater_constant("g_per_MT")
  N2O_GWP <- get_epa_wastewater_constant("N2O_GWP")
  MMT_per_MT <- get_epa_wastewater_constant("MMT_per_MT")
  
  # Calculate nitrous oxide emissions
  emissions_metric_tons_N2O <- population * Fraction_population_not_on_septic * Direct_wwtp_emissions * g_per_MT

  # Create data frame
  df <- data.frame(sector="Waste",
                   category="Wastewater",
                   source="Municipal_N2O_direct",
                   data_source="EPA State Inventory Tool - Wastewater Module",
                   population = population, inventory_year = year, 
                   value_emissions = emissions_metric_tons_N2O,
                   units_emissions = "Metric tons N2O") 
  
  
  return(df)
}

rm(epa_wastewater_constants)

# calculate_mww_n2o_direct_emissions(population = 4904562, year = 1990)



