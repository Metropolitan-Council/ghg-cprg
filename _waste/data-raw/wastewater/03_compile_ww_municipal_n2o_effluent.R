source("R/_load_pkgs.R")
source("_waste/data-raw/wastewater/02_compile_ww_municipal_n2o_direct.R")

## This module estimates effluent nitrous oxide (N2O) emissions from municipal wastewater

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

epa_wastewater_constants <- readRDS("_waste/data-raw/wastewater/epa/epa_wastewater_constants.rds")
epa_protein_consumption <- readRDS("_waste/data-raw/wastewater/epa/epa_protein_consumption.RDS")


# population <- 4904562
# state <- "WI"
# year <- 1990
calculate_mww_n2o_effluent_emissions <- function(population, year) {
  # Define constants for the current year
  days_per_year <- ifelse(lubridate::leap_year(year), 366, 365)

  get_epa_wastewater_constant <- function(variable_name) {
    epa_wastewater_constants %>%
      filter(short_text == variable_name) %>%
      pull(value)
  }


  # if (year == 2022) browser()

  if (any(epa_protein_consumption$year %in% year)) {
    # Grab constants
    protein_consumption <- epa_protein_consumption %>%
      filter(year == .env$year) %>%
      pull(Protein_kg_per_person_per_year)

    Percent_biosolids_as_fertilizer <- epa_protein_consumption %>%
      filter(year == .env$year) %>%
      pull(pct_of_biosolids_as_fertilizer)
  } else {
    # for years that fall after the last estimate of protein consumption,
    # apply that last year value to extrapolate to the current year
    if (year > rev(sort(as.numeric(epa_protein_consumption$year)))[1]) {
      protein_consumption <- epa_protein_consumption %>%
        filter(year == rev(sort(as.numeric(epa_protein_consumption$year)))[1]) %>%
        pull(Protein_kg_per_person_per_year)

      Percent_biosolids_as_fertilizer <- epa_protein_consumption %>%
        filter(year == rev(sort(as.numeric(epa_protein_consumption$year)))[1]) %>%
        pull(pct_of_biosolids_as_fertilizer)
    } else {
      # for years that fall before the earliest estimate of protein consumption,
      # apply that earliest year value to extrapolate to the current year
      protein_consumption <- epa_protein_consumption %>%
        filter(year == sort(as.numeric(epa_protein_consumption$year))[1]) %>%
        pull(Protein_kg_per_person_per_year)

      Percent_biosolids_as_fertilizer <- epa_protein_consumption %>%
        filter(year == sort(as.numeric(epa_protein_consumption$year))[1]) %>%
        pull(pct_of_biosolids_as_fertilizer)
    }
  }



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
  N2O_direct_emissions <- calculate_mww_n2o_direct_emissions(population, year) %>% pull(value_emissions)
  N2O_direct_emissions <- N2O_direct_emissions * (1 / N2O_N_MWR)

  Biosolids_avail_N_MT <- N_in_domestic_wastewater - N2O_direct_emissions

  emissions_metric_tons_N2O <- Biosolids_avail_N_MT * (1 - Percent_biosolids_as_fertilizer) * Emission_Factor_N2O_N * N2O_N_MWR


  # Create data frame
  df <- data.frame(
    sector = "Waste",
    category = "Wastewater",
    source = "Municipal_N2O_effluent",
    data_source = "EPA State Inventory Tool - Wastewater Module",
    population = population, inventory_year = year,
    value_emissions = emissions_metric_tons_N2O,
    units_emissions = "Metric tons N2O"
  )



  return(df)
}


# calculate_mww_n2o_direct_emissions(population = 4904562, year = 1990)
# calculate_mww_n2o_effluent_emissions(population = 4904562, year = 1990)
