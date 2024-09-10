#
# == Comparison of EQUATES emissions totals other EPA emissions data ==
# Emissions_comparison_EQUATES_vs_EPA_Trends_version_20220210.csv provides annual total emissions (US short tons) summed over the Lower 48 from three data sources:
#   - Feb 10, 2022 version of the EPA's Air Pollution Emissions Trends data https://www.epa.gov/air-emissions-inventories/air-pollutant-emissions-trends-data (Note: Only the most recent version of the trends data are available on this website)
#   - EQUATES INV annual summaries for the Tier 1 source categories used in the Trends data
#   - Annual summaries from some of EPA's previous emissions modeling platforms for the Tier 1 source categories used in the Trends data
#   NOTE this does not contain CO2

equates_epa_comps <- read.csv("_transportation/data-raw/epa/air_emissions_modeling/EQUATES/Dataverse/Emissions_comparison_EQUATES_vs_EPA_Trends_version_20220210.csv") %>%
  clean_names() %>%
  pivot_longer(4:19,
    names_to = "inventory_year",
    values_to = "emissions_short_tons"
  ) %>%
  filter(
    source_category %in% c(
      "HIGHWAY VEHICLES",
      "OFF-HIGHWAY"
    )
  ) %>%
  mutate(
    inventory_year = stringr::str_remove(inventory_year, "x") %>%
      as.numeric(),
    emissions_ton = emissions_short_tons %>%
      units::as_units("short_ton") %>%
      units::set_units("ton") %>%
      as.numeric()
  )


equates_national <- fread("_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_M3_bySCC/EQUATES_M3_bySCC_annual.csv",
  colClasses = "character"
) %>%
  pivot_longer(3:19) %>%
  mutate(value = as.numeric(value)) %>%
  clean_names()
