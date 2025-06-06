## run scripts - 01 scripts depend on raw data, 02 have 01, raw data dependencies,
## 03 have 02, 01, raw data dependencies

# #01
# source("_agriculture/data-raw/01_compile_ag_constants.R")
# source("_agriculture/data-raw/01_compile_county_fertilizer_proportion.R")
# source("_agriculture/data-raw/01_compile_enteric_fermentation_emission_factors.R")
# source("_agriculture/data-raw/01_compile_manure_management_systems.R")
# source("_agriculture/data-raw/01_compile_methane_conversion_factors.R")
# source("_agriculture/data-raw/01_compile_nlcd_ctu_landcover.R")
# source("_agriculture/data-raw/01_compile_typical_animal_mass.R")
# source("_agriculture/data-raw/01_compile_usda_cattle_survey.R")
# source("_agriculture/data-raw/01_compile_usda_census_data.R")
#
# #02
# source("_agriculture/data-raw/02_compile_county_crop_production.R")
# source("_agriculture/data-raw/02_compile_nitrogen_excretion.R")
# source("_agriculture/data-raw/02_compile_volatile_solids.R")
#
# #03
# source("_agriculture/data-raw/03_compile_enteric_fermentation_emissions.R")
# source("_agriculture/data-raw/03_compile_fertilizer_emissions.R")
# source("_agriculture/data-raw/03_compile_manure_emissions.R")
# source("_agriculture/data-raw/03_compile_soil_residue_emissions.R")


### compile all agricultural subsectors and output as one file

agricultural_emissions <- bind_rows(
  readRDS("./_agriculture/data/enteric_fermentation_emissions.rds"),
  readRDS("./_agriculture/data/manure_emissions.rds"),
  readRDS("./_agriculture/data/soil_residue_emissions.rds"),
  readRDS("./_agriculture/data/fertilizer_emissions.rds")
)

ctu_agricultural_emissions <- bind_rows(
  readRDS("./_agriculture/data/ctu_enteric_fermentation_emissions.rds"),
  readRDS("./_agriculture/data/ctu_manure_emissions.rds"),
  readRDS("./_agriculture/data/ctu_soil_residue_emissions.rds"),
  readRDS("./_agriculture/data/ctu_fertilizer_emissions.rds")
)

waldo::compare(
  agricultural_emissions %>%
    filter(inventory_year == 2021) %>%
    pull(mt_co2e) %>%
    sum(),
  ctu_agricultural_emissions %>%
    filter(inventory_year == 2021) %>%
    pull(mt_co2e) %>%
    sum()
) # checks out

waldo::compare(
  agricultural_emissions %>%
    distinct(source, inventory_year) %>%
    arrange(inventory_year),
  ctu_agricultural_emissions %>%
    distinct(source, inventory_year) %>%
    arrange(inventory_year)
)
# county has 2023 soil residue emissions but not ctu (expected)

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

saveRDS(ctu_agricultural_emissions, "./_agriculture/data/_ctu_agricultural_emissions.rds")
