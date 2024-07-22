source("R/_load_pkgs.R")
source("R/global_warming_potential.R")
#source("_agriculture/data-raw/_fetch_usda_key.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")

### load in EPA Ag SIT csvs
enteric_ef <- read_rds("_agriculture/data/enteric_fermentation_emission_factors.rds")

# formatted files
ag_constants <- readRDS("_agriculture/data/ag_constants_formatted.rds")

## convert to named vector for easier indexing
ag_constants_vec <- ag_constants %>%
  dplyr::select(short_text, value) %>%
  tibble::deframe()

# livestock data
usda_livestock <- readRDS("_agriculture/data/usda_census_data.rds") %>% 
  mutate(state = if_else(county_name %in% c("St. Croix", "Pierce"), "Wisconsin", "Minnesota"))

## merge livestock data with enteric emission factors, multiply and convert to co2e
animal_burps <- left_join(
  usda_livestock %>% filter(year >= 2005 & year <= 2021),
  enteric_ef, 
  by = c("year", "livestock_type", "state")) %>%
  filter(!is.na(mt_ch4_head_yr)) %>%  #poultry do not contribute to enteric fermentation
  mutate(
    gas_type = "ch4",
    MT_gas = mt_ch4_head_yr * head_count,
    MT_co2e = MT_gas * gwp$ch4,
  ) %>% 
  select(year, county_name, livestock_type,gas_type, MT_gas, MT_co2e)


animal_burps_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "year", class(animal_burps$year), "Year of survey",
    "county_name", class(animal_burps$county_name), "County name",
    "livestock_type", class(animal_burps$livestock_type), "Livestock classification",
    "gas_type", class(animal_burps$gas_type), "Greenhouse gas emitted from source",
    "MT_co2e", class(animal_burps$MT_co2e), "Metric tons of CO2 equivalency",
    "MT_gas", class(animal_burps$MT_gas), "Total metric tons of gas emitted from source"
  )

saveRDS(animal_burps, "./_agriculture/data/county_enteric_fermentation_2005_2021.rds")
saveRDS(animal_burps_meta, "./_agriculture/data/county_enteric_fermentation_2005_2021_meta.rds")
