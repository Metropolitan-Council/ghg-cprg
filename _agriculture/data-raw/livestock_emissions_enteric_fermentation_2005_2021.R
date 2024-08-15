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
    mt_gas = mt_ch4_head_yr * head_count,
    mt_co2e = mt_gas * gwp$ch4,
  ) %>% 
  select(year, county_name, livestock_type,gas_type, mt_gas, mt_co2e)

animal_burps_out <- animal_burps %>% 
  left_join(., cprg_county %>% select(geoid, county_name) %>% st_drop_geometry(),
            by = c("county_name")) %>% 
  rename(inventory_year = year, 
         value_emissions = mt_gas, 
         units_emissions = gas_type) %>% 
  mutate(sector = "Agriculture",
         category = "Livestock",
         source = "Enteric fermentation",
         units_emissions = case_when(
           units_emissions == "ch4" ~ "Metric tons CH4",
           units_emissions == "n2o" ~ "Metric tons N2O",
           units_emissions == "co2" ~ "Metric tons CO2"
         ),
         data_source = "USDA livestock census",
         factor_source = "EPA SIT") %>% 
  select(geoid, inventory_year, sector, category, source,
         data_source, factor_source, value_emissions, units_emissions, mt_co2e)

animal_burps_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "inventory_year", class(animal_burps_out$inventory_year), "Year of survey",
    "geoid", class(animal_burps_out$geoid), "County GEOID",
    "sector", class(animal_burps_out$sector), "Emissions sector. One of Transportation, Energy, Waste, Nature, Agriculture",
    "category", class(animal_burps_out$category), "Category of emissions within given sector",
    "source", class(animal_burps_out$source), "Source of emissions. Most detailed sub-category in this table",
    "data_source", class(animal_burps_out$data_source), "Activity data source",
    "factor_source", class(animal_burps_out$factor_source), "Emissions factor data source",
    "value_emissions", class(animal_burps_out$value_emissions), "Numerical value of emissions",
    "units_emissions", class(animal_burps_out$units_emissions), "Units and gas type of emissions",
    "mt_co2e", class(animal_burps_out$mt_co2e), "Metric tons of gas in CO2 equivalency"
  )

saveRDS(animal_burps_out, "./_agriculture/data/enteric_fermentation_emissions.rds")
saveRDS(animal_burps_meta, "./_agriculture/data/enteric_fermentation_emissions_meta.rds")
