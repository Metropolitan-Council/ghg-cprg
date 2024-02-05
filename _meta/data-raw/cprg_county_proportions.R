source("R/_load_pkgs.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_county_meta <- readRDS("_meta/data/cprg_county_meta.RDS")
library(tidycensus)
options(tidycensus.cache = TRUE)


# get 2021 population from ACS 2017-2021 -----
acs_proportions <- tidycensus::get_acs(
  # Minnesota
  survey = "acs5",
  year = 2021,
  state = "MN",
  geography = "county",
  variables = c(total_pop = "DP05_0001E")
) %>%
  mutate(STATE = "Minnesota") %>% 
  bind_rows(
    # wisconsin
    tidycensus::get_acs(
      survey = "acs5",
      year = 2021,
      state = "WI",
      geography = "county",
      variables = c(total_pop = "DP05_0001E")
    ) %>% 
      mutate(STATE = "Wisconsin")
  ) %>% 
  mutate(year = "2021") %>% 
  group_by(STATE, variable, year) %>% 
  mutate(state_population = sum(estimate)) %>% 
  group_by(GEOID, NAME, STATE, variable, year) %>% 
  mutate(county_population = estimate) %>% 
  ungroup() %>% 
  select(-NAME, -moe) %>% 
  right_join(
    cprg_county %>% 
      sf::st_drop_geometry(),
    by = c("GEOID", "STATE")
  ) %>% 
  rowwise() %>% 
  mutate(county_proportion_of_state_pop = county_population/state_population,
         population_data_source = "ACS 5-Year Estimates 2021, Table DP05"
  ) %>% 
  
  select(
    names(cprg_county)[1:6],
    year,
    state_population, 
    county_population, 
    county_proportion_of_state_pop,
    population_data_source
  ) 


# use decennial census -----


decennial_proportions <- tidycensus::get_decennial("county",
                          state = "MN",
                          variables = "P1_001N",
                          year = 2020
) %>%
  mutate(STATE = "Minnesota") %>% 
  bind_rows(tidycensus::get_decennial("county",
                                      state = "WI",
                                      variables = "P1_001N",
                                      year = 2020
  ) %>% mutate(STATE =  "Wisconsin")) %>% 
  mutate(year = "2020") %>% 
  group_by(STATE, variable, year) %>% 
  mutate(state_population = sum(value)) %>% 
  group_by(GEOID, NAME, STATE, variable, year) %>% 
  mutate(county_population = value) %>% 
  ungroup() %>% 
  select(-NAME) %>% 
  right_join(
    cprg_county %>% 
      sf::st_drop_geometry(),
    by = c("GEOID", "STATE")
  ) %>% 
  rowwise() %>% 
  mutate(county_proportion_of_state_pop = county_population/state_population,
         population_data_source = "Decennial Census PL 94-171 Redistricting Data Summary File"
  ) %>% 
  select(
    names(cprg_county)[1:6],
    year,
    state_population, 
    county_population, 
    county_proportion_of_state_pop,
    population_data_source
  ) 


cprg_county_proportions <- 
  bind_rows(acs_proportions,
            decennial_proportions)




cprg_county_proportions_meta <- bind_rows(
  cprg_county_meta,
  tribble(
    ~Column, ~Class, ~Description,
    "year", class(cprg_county_proportions$year), "Estimate year",
    "county_population", class(cprg_county_proportions$county_population), "Total county population estimate",
    "state_population", class(cprg_county_proportions$state_population), "Total state population estimate",
    "county_proportion_of_state_pop", class(cprg_county_proportions$county_proportion_of_state_pop), "Proportion of the county population relative to the total state population",
    "population_data_source", class(cprg_county_proportions$population_data_source), "Population estimate data source"
  )
) %>%
  filter(Column %in% names(cprg_county_proportions))


saveRDS(cprg_county_proportions, "_meta/data/cprg_county_proportions.RDS")
saveRDS(cprg_county_proportions_meta, "_meta/data/cprg_county_proportions_meta.RDS")
