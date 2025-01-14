# rm(list=ls())
source("R/_load_pkgs.R")

## This module estimates methane (CH4) emissions from municipal wastewater
epa_wastewater_constants <- readRDS("_waste/data-raw/wastewater_v2/data-raw/epa_wastewater_constants.RDS")


calculate_mww_n2o_direct_emissions <- function(population, state, year) {
    # Define constants for the current year
    days_per_year <- ifelse(lubridate::leap_year(year), 366, 365)
    
    get_epa_wastewater_constant <- function(variable_name) {
        epa_wastewater_constants %>%
            filter(STATE == state & Variable == variable_name) %>%
            pull(Value)
    }
    
    # Grab constants
    Fraction_population_not_on_septic <- get_epa_wastewater_constant("Fraction_population_not_on_septic")
    Direct_wwtp_emissions <- get_epa_wastewater_constant("Direct_wwtp_emissions")
    g_per_MT <- get_epa_wastewater_constant("g_per_MT")
    N2O_GWP <- get_epa_wastewater_constant("N2O_GWP")
    MMT_per_MT <- get_epa_wastewater_constant("MMT_per_MT")

    # Calculate nitrous oxide emissions
    emissions_metric_tons_N2O <- population * Fraction_population_not_on_septic * Direct_wwtp_emissions * g_per_MT
    emissions_MMT_CO2_eq <- emissions_metric_tons_N2O * N2O_GWP * MMT_per_MT
    
    # Create data frame
    df <- data.frame(State = state, Population = population, Year = year, 
                     MWW_N2O_direct_MTN2O = emissions_metric_tons_N2O, 
                     MWW_N2O_direct_MMTCO2e = emissions_MMT_CO2_eq)
    
    return(df)
}


# calculate_mww_n2o_direct_emissions(population = 5707390, state = "MN", year = 2021)


