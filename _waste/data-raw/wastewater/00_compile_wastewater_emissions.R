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


# Load county census data
cprg_census_county_population <- readRDS("_meta/data/census_county_population.rds") %>% filter(cprg_area)


# County-level emissions (11-county MSA) ----------------------------------
# Pre-allocate memory to this dataframe by constructing it first then populating it
# 11 counties, 17 years, 3 variables = 561 rows
states_to_analyze <- c("MN", "WI")

# Identify all available years for county population
all_county_pop_yrs <- unique(cprg_census_county_population$population_year) %>% sort()

# Create a sequence starting in 2005 and going to the maximum year where data was available for this dataset
all_years <- seq(
  ifelse(all_county_pop_yrs[1]>=2005, all_county_pop_yrs[1], 2005),
  rev(all_county_pop_yrs)[1]
)
# NOTE: To calculate effluent N2O emissions, this routine queries the epa_protein_consumption.rds
# table, which contains annual estimates of protein consumption. If we have a population estimate
# for a given year, but that year is not contained in the epa_protein_consumption.rds
# file, a value of NA gets assigned to value emissions

all_counties <- unique(cprg_census_county_population$county_name)

county_wastewater_emissions <- expand.grid(
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



county_wastewater_emissions <- county_wastewater_emissions %>%
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
  ) %>% dplyr::select(-population)




county_wastewater_emissions_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "inventory_year", class(county_wastewater_emissions$inventory_year), "Year of survey",
    "county_name", class(county_wastewater_emissions$county_name), "County name",
    "state_name", class(county_wastewater_emissions$state_name), "State name",
    "state", class(county_wastewater_emissions$state), "State name (abbreviated)",
    "sector", class(county_wastewater_emissions$sector), "Emissions sector. One of Transportation, Energy, Waste, Nature, Agriculture",
    "category", class(county_wastewater_emissions$category), "Category of emissions within given sector",
    "source", class(county_wastewater_emissions$source), "Source of emissions. Most detailed sub-category in this table",
    "data_source", class(county_wastewater_emissions$data_source), "Activity data source",
    "value_emissions", class(county_wastewater_emissions$value_emissions), "Numerical value of emissions",
    "units_emissions", class(county_wastewater_emissions$units_emissions), "Units and gas type of emissions",
    "mt_co2e", class(county_wastewater_emissions$mt_co2e), "Metric tons of gas in CO2 equivalency"
  )

saveRDS(county_wastewater_emissions, "./_waste/data/_county_wastewater_emissions.rds")
saveRDS(county_wastewater_emissions_meta, "./_waste/data/_county_wastewater_emissions_meta.rds")


# county_wastewater_emissions %>%
#   ggplot() + theme_minimal() +
#   geom_path(aes(x=inventory_year, y=mt_co2e, color=source)) +
#   geom_point(aes(x=inventory_year, y=mt_co2e, color=source)) +
#   facet_wrap(~county_name)



# CTU-level emissions (7-county only) -------------------------------------
# Load CTU census data
ctu_population <- readRDS("_meta/data/ctu_population.rds") 

all_ctus <- ctu_population %>%
  group_by(geoid, ctuid, ctu_name, ctu_class) %>%
  summarize(tmp=head(1), .groups = "keep") %>% select(-tmp)

# Identify all available years for county population
all_ctu_pop_yrs <- unique(ctu_population$inventory_year) %>% sort()

# Create a sequence starting in 2005 and going to the maximum year where data was available for this dataset
all_years <- seq(
  ifelse(all_ctu_pop_yrs[1]>=2005, all_ctu_pop_yrs[1], 2005),
  rev(all_ctu_pop_yrs)[1]
)


ctu_wastewater_emissions <- all_ctus %>%
  crossing(
    sector = "Waste",
    category = "Wastewater",
    inventory_year = all_years,
    source = c("Municipal_CH4", "Municipal_N20_direct", "Municipal_N20_effluent"),
    data_source = "EPA State Inventory Tool - Wastewater Module"
    ) %>%
  arrange(ctu_name, inventory_year, source) %>%
  left_join(
    ctu_population %>%
      dplyr::select(geoid, ctuid, ctu_name, ctu_class, inventory_year, population = ctu_population),
    by = join_by(geoid, ctuid, ctu_name, ctu_class, inventory_year)
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




ctu_wastewater_emissions <- ctu_wastewater_emissions %>%
  group_by(geoid, ctuid, ctu_name, ctu_class, inventory_year, source) %>%
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
  arrange(ctu_name, inventory_year, source) %>%
  relocate(
    inventory_year, geoid, ctuid, ctu_name, ctu_class,
    sector, category, source, data_source,
    population, value_emissions,
    units_emissions, mt_co2e
  ) %>% dplyr::select(-population)


ctu_wastewater_emissions_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "inventory_year", class(ctu_wastewater_emissions$inventory_year), "Year of survey",
    "geoid", class(ctu_wastewater_emissions$geoid), "GEOID tag for MN county",
    "ctuid", class(ctu_wastewater_emissions$ctuid), "CTU census tag",
    "ctu_name", class(ctu_wastewater_emissions$ctu_name), "CTU name",
    "ctu_class", class(ctu_wastewater_emissions$ctu_class), "CTU class (either CITY, TOWNSHIP, or UNORGANIZED TERRITORY",
    "sector", class(ctu_wastewater_emissions$sector), "Emissions sector. One of Transportation, Energy, Waste, Nature, Agriculture",
    "category", class(ctu_wastewater_emissions$category), "Category of emissions within given sector",
    "source", class(ctu_wastewater_emissions$source), "Source of emissions. Most detailed sub-category in this table",
    "data_source", class(ctu_wastewater_emissions$data_source), "Activity data source",
    "value_emissions", class(ctu_wastewater_emissions$value_emissions), "Numerical value of emissions",
    "units_emissions", class(ctu_wastewater_emissions$units_emissions), "Units and gas type of emissions",
    "mt_co2e", class(ctu_wastewater_emissions$mt_co2e), "Metric tons of gas in CO2 equivalency"
  )

saveRDS(ctu_wastewater_emissions, "./_waste/data/_ctu_wastewater_emissions.rds")
saveRDS(ctu_wastewater_emissions_meta, "./_waste/data/_ctu_wastewater_emissions_meta.rds")



