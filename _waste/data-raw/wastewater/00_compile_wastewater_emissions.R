# rm(list=ls())
source("R/_load_pkgs.R")
source("R/global_warming_potential.R")


# 01 ----------------------------------------------------------------------
source("_waste/data-raw/wastewater/01_compile_ww_constants.R")
source("_waste/data-raw/wastewater/01_compile_ww_protein_consumption.R")

# 02 ----------------------------------------------------------------------
source("_waste/data-raw/wastewater/02_compile_ww_municipal_ch4.R")
source("_waste/data-raw/wastewater/02_compile_ww_municipal_n2o_direct.R")

# 03 ----------------------------------------------------------------------
source("_waste/data-raw/wastewater/03_compile_ww_municipal_n2o_effluent.R")


# Load constants ----------------------------------------------------------
epa_wastewater_constants <- readRDS("_waste/data-raw/wastewater/epa/epa_wastewater_constants.RDS")
epa_protein_consumption <- readRDS("_waste/data-raw/wastewater/epa/epa_protein_consumption.RDS")


# Load census data --------------------------------------------------------
cprg_census_county_population <- readRDS("_meta/data/census_county_population.rds") %>% filter(cprg_area)

states_to_analyze <- c("MN", "WI")
years_to_analyze <- seq(2005, 2021)


# Pre-allocate memory to this dataframe by constructing it first then populating it
# 11 counties, 17 years, 3 variables = 561 rows
all_counties <- unique(cprg_census_county_population$county_name)
all_years <- years_to_analyze

wastewater_emissions <- expand.grid(
  sector = "Waste",
  category = "Wastewater",
  county_name = all_counties,
  inventory_year = all_years,
  source = c("Municipal_CH4", "Municipal_N20_direct", "Municipal_N20_effluent"),
  data_source = "EPA State Inventory Tool - Wastewater Module"
) %>%
  arrange(county_name, inventory_year, source) %>%
  left_join(
    cprg_census_county_population %>%
      dplyr::select(county_name, state_name, population_year, population) %>%
      mutate(inventory_year = as.numeric(population_year)) %>%
      dplyr::select(-population_year),
    by = join_by(county_name, inventory_year)
  ) %>%
  mutate(
    state =
      case_when(
        state_name == "Minnesota" ~ "MN",
        state_name == "Wisconsin" ~ "WI"
      )
  ) %>%
  relocate(c(population, inventory_year), .after = everything()) %>%
  mutate(
    value_emissions = NA,
    units_emissions = case_when(
      source == "Municipal_CH4" ~ "Metric tons CH4",
      source == "Municipal_N20_direct" ~ "Metric tons N2O",
      source == "Municipal_N20_effluent" ~ "Metric tons N2O"
    )
  ) %>%
  as_tibble()



wastewater_emissions <- wastewater_emissions %>%
  group_by(county_name, state_name, inventory_year, source) %>%
  mutate(
    value_emissions =
      case_when(
        source == "Municipal_CH4" ~ (calculate_mww_ch4_emissions(year = inventory_year, population = population) %>% pull(value_emissions)),
        source == "Municipal_N20_direct" ~ (calculate_mww_n2o_direct_emissions(year = inventory_year, population = population) %>% pull(value_emissions)),
        source == "Municipal_N20_effluent" ~ (calculate_mww_n2o_effluent_emissions(year = inventory_year, population = population) %>% pull(value_emissions))
      ),
    mt_co2e =
      case_when(
        units_emissions == "Metric tons CH4" ~ value_emissions * gwp$ch4,
        units_emissions == "Metric tons N2O" ~ value_emissions * gwp$n2o
      )
  ) %>%
  arrange(state, county_name, inventory_year, source) %>%
  relocate(
    inventory_year, county_name, state_name, state,
    sector, category, source, data_source,
    population, value_emissions,
    units_emissions, mt_co2e
  )




wastewater_emissions_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "inventory_year", class(wastewater_emissions$inventory_year), "Year of survey",
    "county_name", class(wastewater_emissions$county_name), "County name",
    "state_name", class(wastewater_emissions$state_name), "State name",
    "state", class(wastewater_emissions$state), "State name (abbreviated)",
    "sector", class(wastewater_emissions$sector), "Emissions sector. One of Transportation, Energy, Waste, Nature, Agriculture",
    "category", class(wastewater_emissions$category), "Category of emissions within given sector",
    "source", class(wastewater_emissions$source), "Source of emissions. Most detailed sub-category in this table",
    "data_source", class(wastewater_emissions$data_source), "Activity data source",
    "value_emissions", class(wastewater_emissions$value_emissions), "Numerical value of emissions",
    "units_emissions", class(wastewater_emissions$units_emissions), "Units and gas type of emissions",
    "mt_co2e", class(wastewater_emissions$mt_co2e), "Metric tons of gas in CO2 equivalency"
  )

saveRDS(wastewater_emissions, "./_waste/data/_county_wastewater_emissions.rds")
saveRDS(wastewater_emissions_meta, "./_waste/data/_county_wastewater_emissions_meta.rds")


# wastewater_emissions %>%
#   ggplot() + theme_minimal() +
#   geom_path(aes(x=inventory_year, y=mt_co2e, color=source)) +
#   geom_point(aes(x=inventory_year, y=mt_co2e, color=source)) +
#   facet_wrap(~county_name)
