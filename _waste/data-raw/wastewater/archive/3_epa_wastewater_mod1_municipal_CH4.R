# rm(list=ls())
source("R/_load_pkgs.R")

## This module estimates methane (CH4) emissions from municipal wastewater
epa_wastewater_constants <- readRDS("_waste/data-raw/wastewater_v2/data-raw/epa_wastewater_constants.RDS")


calculate_mww_ch4_emissions <- function(population, state, year) {
    # Define constants for the current year
    days_per_year <- ifelse(lubridate::leap_year(year), 366, 365)
    
    get_epa_wastewater_constant <- function(variable_name) {
        epa_wastewater_constants %>%
            filter(STATE == state & Variable == variable_name) %>%
            pull(Value)
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
    emissions_MMT_CO2_eq <- emissions_metric_tons_CH4 * CH4_GWP * MMT_per_MT
    
    # Create data frame
    df <- data.frame(State = state, Population = population, Year = year, MWW_CH4_MMTCO2e = emissions_MMT_CO2_eq)
    # Create data frame
    df <- data.frame(State = state, Population = population, Year = year, 
                     MWW_CH4_MTCH4 = emissions_metric_tons_CH4, 
                     MWW_CH4_MMTCO2e = emissions_MMT_CO2_eq)
    
    return(df)
}


# calculate_mww_ch4_emissions(population = 4982796, state = "MN", year = 1990)


