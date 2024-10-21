# rm(list=ls())
source("R/_load_pkgs.R")

source("_waste/data-raw/wastewater_v2/1_epa_wastewater_annual_protein_consumption.R")
source("_waste/data-raw/wastewater_v2/2_epa_wastewater_constants.R")
source("_waste/data-raw/wastewater_v2/3_epa_wastewater_mod1_municipal_CH4.R")
source("_waste/data-raw/wastewater_v2/4_epa_wastewater_mod2_municipal_N2O_direct.R")
source("_waste/data-raw/wastewater_v2/5_epa_wastewater_mod3_municipal_N2O_effluent.R")


epa_wastewater_constants <- readRDS("_waste/data-raw/wastewater_v2/data-raw/epa_wastewater_constants.RDS")
epa_protein_consumption <- readRDS("_waste/data-raw/wastewater_v2/data-raw/epa_protein_consumption.RDS")

cprg_census_county_population <- readRDS("_meta/data/census_county_population.rds") %>% filter(cprg_area)

states_to_analyze <- c("MN", "WI")
years_to_analyze <- seq(2005, 2021)

# Pre-allocate memory to this dataframe by constructing it first then populating it
# 11 counties, 17 years, 3 variables = 561 rows
all_counties <- unique(cprg_census_county_population$county_name)
all_years <- years_to_analyze

df_ww_emissions <- expand.grid(county_name = all_counties, 
            year = all_years,
            Emission_type = c("Municipal CH4", "Municipal N20 direct", "Municipal N20 effluent") ) %>%
  left_join(cprg_census_county_population %>% 
              dplyr::select(county_name, state_name, population_year, population) %>%
              mutate(year = as.numeric(population_year)) %>%
              dplyr::select(-population_year),
            by = join_by(county_name, year)) %>%
  mutate(state = 
           case_when(state_name == "Minnesota" ~ "MN",
                     state_name == "Wisconsin" ~ "WI")) %>%
  relocate(state, state_name, county_name, year, population, Emission_type) %>%
  mutate(MMT_CO2e = NA) %>% as_tibble()


df_final <- df_ww_emissions %>%
  # filter(state == process_state) %>%
  group_by(county_name, state, year, Emission_type) %>%
  mutate(MMT_CO2e = 
           case_when(
             Emission_type == "Municipal CH4" ~ (calculate_mww_ch4_emissions(state = state, year = year, population = population) %>% pull(MWW_CH4_MMTCO2e)),
             Emission_type == "Municipal N20 direct" ~ (calculate_mww_n2o_direct_emissions(state = state, year = year, population = population) %>% pull(MWW_N2O_direct_MMTCO2e)),
             Emission_type == "Municipal N20 effluent" ~ (calculate_mww_n2o_effluent_emissions(state = state, year = year, population = population) %>% pull(MWW_N2O_effluent_MMTCO2e))
           )) %>%
  arrange(state, county_name, year, Emission_type) %>%
  # convert CO2e from MMTCO2e to metric tonnes CO2e
  mutate(CO2e = as.numeric(MMT_CO2e) * 10^6)



df_final %>%
  ggplot() + theme_minimal() +
  geom_point(aes(x=year, y=CO2e, color=Emission_type)) + facet_wrap(~county_name)

