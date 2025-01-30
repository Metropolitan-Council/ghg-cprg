# rm(list=ls())
source("R/_load_pkgs.R")
source("_waste/data-raw/wastewater_v2/data-raw/02_compile_ww_municipal_n2o_direct.R")


## "Municipal wastewater N₂O emissions from biosolids are calculated by multiplying the total
## annual protein consumption by the nitrogen content of protein and fraction of nitrogen not
## consumed, an N₂O emission factor per metric ton of nitrogen treated, subtracting direct
## emissions, converted to million metric tons carbon equivalent (MMTCE), and converted to
## million metric tons carbon dioxide equivalent (MMTCO₂E). Direct and biosolids N₂O emissions
## are then added to produce an estimate of total municipal wastewater treatment N₂O emissions.
## The methodology and factors used for these calculations are discussed in detail in the
## Wastewater Chapter of the User's Guide. Sewage sludge is often applied to agricultural fields
## as fertilizer. Emissions from this use should be accounted under Agricultural Soil Management.
## The Agriculture Module of the SIT is designed to calculate emissions from sewage sludge applied
## to land, but to be consistent, users should enter the percentage of sewage sludge applied to
## agricultural soils in Column S so that emissions are not double-counted. Currently, there are
## no default data for this percentage."




epa_wastewater_constants <- readRDS("_waste/data-raw/wastewater_v2/data-raw/epa_wastewater_constants.RDS")
epa_protein_consumption <- readRDS("_waste/data-raw/wastewater_v2/data-raw/epa_protein_consumption.RDS")

# population <- 4904562
# state <- "WI"
# year <- 1990
calculate_mww_n2o_effluent_emissions <- function(population, state, year) {
  # Define constants for the current year
  days_per_year <- ifelse(lubridate::leap_year(year), 366, 365)

  get_epa_wastewater_constant <- function(variable_name) {
    epa_wastewater_constants %>%
      filter(STATE == state & Variable == variable_name) %>%
      pull(Value)
  }

  # Grab constants
  protein_consumption <- epa_protein_consumption %>%
    filter(State == state & Year == year) %>%
    pull(Protein_kg_per_person_per_year)

  Fraction_nitrogen_in_protein <- get_epa_wastewater_constant("Fraction_nitrogen_in_protein")
  Factor_non_consumption_nitrogen <- get_epa_wastewater_constant("Factor_non_consumption_nitrogen")
  MT_per_kg <- get_epa_wastewater_constant("MT_per_kg")
  N2O_N_MWR <- get_epa_wastewater_constant("N2O_N_MWR")
  N2O_GWP <- get_epa_wastewater_constant("N2O_GWP")
  Emission_Factor_N2O_N <- get_epa_wastewater_constant("Emission_Factor_N2O_N")
  MMT_per_MT <- get_epa_wastewater_constant("MMT_per_MT")


  # N in domestic wastewater (metric tons)
  N_in_domestic_wastewater <- population * protein_consumption * Fraction_nitrogen_in_protein * Factor_non_consumption_nitrogen * MT_per_kg

  # direct N emissions from domestic wastewater (metric tons)
  ## NOTE: here we're sourcing the function 'calculate_mww_n2o_direct_emissions' to calculate this value
  N2O_direct_emissions <- calculate_mww_n2o_direct_emissions(population, state, year) %>% pull(MWW_N2O_direct_MTN2O)
  N2O_direct_emissions <- N2O_direct_emissions * (1 / N2O_N_MWR)

  Biosolids_avail_N_MT <- N_in_domestic_wastewater - N2O_direct_emissions

  Percent_biosolids_as_fertilizer <- epa_protein_consumption %>%
    filter(State == state & Year == year) %>%
    pull(pct_of_biosolids_as_fertilizer)


  emissions_metric_tons_N2O <- Biosolids_avail_N_MT * (1 - Percent_biosolids_as_fertilizer) * Emission_Factor_N2O_N * N2O_N_MWR
  emissions_MMT_CO2_eq <- emissions_metric_tons_N2O * N2O_GWP * MMT_per_MT


  # Create data frame
  df <- data.frame(
    State = state, Population = population, Year = year,
    MWW_N2O_effluent_MTN2O = emissions_metric_tons_N2O,
    MWW_N2O_effluent_MMTCO2e = emissions_MMT_CO2_eq
  )

  return(df)
}


# (calculate_mww_n2o_direct_emissions(population = 5546166, state = "WI", year = 1990) %>% pull(MWW_N2O_direct_MMTCO2e)) +
# (calculate_mww_n2o_effluent_emissions(population = 5546166, state = "WI", year = 1990) %>% pull(MWW_N2O_effluent_MMTCO2e))
