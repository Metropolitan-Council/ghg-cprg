### compile all agricultural subsectors and output as one file

agricultural_emissions <- bind_rows(
  readRDS("./_agriculture/data/enteric_fermentation_emissions.rds"),
  readRDS("./_agriculture/data/manure_emissions.rds"),
  readRDS("./_agriculture/data/soil_residue_emissions.rds"),
  readRDS("./_agriculture/data/fertilizer_emissions.rds")
)

agricultural_emissions_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "inventory_year", class(agricultural_emissions$inventory_year), "Year of survey",
    "geoid", class(agricultural_emissions$geoid), "County GEOID",
    "sector", class(agricultural_emissions$sector), "Emissions sector. One of Transportation, Energy, Waste, Nature, Agriculture",
    "category", class(agricultural_emissions$category), "Category of emissions within given sector",
    "source", class(agricultural_emissions$source), "Source of emissions. Most detailed sub-category in this table",
    "data_source", class(agricultural_emissions$data_source), "Activity data source",
    "factor_source", class(agricultural_emissions$factor_source), "Emissions factor data source",
    "value_emissions", class(agricultural_emissions$value_emissions), "Numerical value of emissions",
    "units_emissions", class(agricultural_emissions$units_emissions), "Units and gas type of emissions",
    "mt_co2e", class(agricultural_emissions$mt_co2e), "Metric tons of gas in CO2 equivalency"
  )

saveRDS(agricultural_emissions, "./_agriculture/data/_agricultural_emissions.rds")
saveRDS(agricultural_emissions_meta, "./_agriculture/data/_agricultural_emissions_meta.rds")
