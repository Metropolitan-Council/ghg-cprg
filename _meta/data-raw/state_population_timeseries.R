source("R/_load_pkgs.R")
library(tidycensus)
options(tidycensus.cache = TRUE)
# To get started working with tidycensus, users should set their Census API key.
# A key can be obtained from http://api.census.gov/data/key_signup.html.
# census_api_key("YOUR API KEY GOES HERE")

# we will pull ACS 1-Year and decennial estimates for total population
# state level. 

vars <- load_variables("pl", year = "2020")

state_pop_acs <- purrr::map_dfr(
  c(2005:2019, 2021:2022),
  function(x){
    tidycensus::get_acs(
      survey = "acs1",
      year = x,
      state = "MN",
      geography = "state",
      variables = c(total_pop = "B01003_001")
    ) %>% 
      mutate(acs_year = x)
  }
) %>% 
  bind_rows(
    purrr::map_dfr(
      c(2005:2019, 2021:2022),
      function(x){
        tidycensus::get_acs(
          survey = "acs1",
          year = x,
          state = "WI",
          geography = "state",
          variables = c(total_pop = "B01003_001")
        ) %>% 
          mutate(acs_year = x)
      }
    )) %>% 
  mutate(
    population_data_source = "ACS 1-Year Estimates, Table B01003",
    population = estimate
  )


state_pop_decennial <- 
  bind_rows(
    purrr::map_dfr(
      c(2020),
      function(x){
        tidycensus::get_decennial(
          geography = "state",
          state = "MN",
          year = x,
          variables = c(total_pop = "P1_001N")
        ) %>% 
          mutate(decennial_year = x)
      }
    ),
    purrr::map_dfr(
      c(2020),
      function(x){
        tidycensus::get_decennial(
          geography = "state",
          state = "WI",
          year = x,
          variables = c(total_pop = "P1_001N")
        ) %>% 
          mutate(decennial_year = x)
      }
    )) %>% 
  mutate(
    population = value,
    population_data_source = "Decennial Census, Table P1"
  )


# confirm that the 2010 and 2020 population estimates
# to ensure they are the same across tables
state_pop_decennial %>% 
  left_join(state_pop_acs, by = c("decennial_year" = "acs_year",
                                  "GEOID",
                                  "NAME",
                                  "variable"))



# combine all 
state_population_timeseries <- 
  state_pop_acs %>% 
  select(GEOID, state = NAME, 
         population_year = acs_year, population, population_data_source) %>% 
  # remove 2010 and 2020 from acs 1 year
  filter(!population_year %in% state_pop_decennial$decennial_year) %>% 
  # bind pop decennial
  bind_rows(state_pop_decennial %>% 
              select(GEOID, 
                     state = NAME,
                     population_year = decennial_year,
                     population, population_data_source)) %>% 
  arrange(state, population_year) %>% 
  select(state,
         population_year, population, population_data_source) %>% 
  unique()


# create metadata
state_population_timeseries_meta <- 
  tribble(
    ~Column, ~Class, ~Description,
    "state", class(state_population_timeseries$state), "State name",
    "population_year", class(state_population_timeseries$population_year), "Population estimate year",
    "population", class(state_population_timeseries$population), "Total state population estimate",
    "population_data_source", class(state_population_timeseries$population_data_source), "Population estimate data source"
  )

saveRDS(state_population_timeseries, "_meta/data/state_population_timeseries.RDS")
saveRDS(state_population_timeseries_meta, "_meta/data/state_population_timeseries_meta.RDS")
