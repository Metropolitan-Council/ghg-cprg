source("R/_load_pkgs.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_county_meta <- readRDS("_meta/data/cprg_county_meta.RDS")

library(tidycensus)
options(tidycensus.cache = TRUE)
# To get started working with tidycensus, users should set their Census API key.
# A key can be obtained from http://api.census.gov/data/key_signup.html.
# census_api_key("YOUR API KEY GOES HERE")

# we will pull ACS 1-Year and decennial estimates for total population
# county level. 

vars <- load_variables("pl", year = "2020")

pop_acs <- purrr::map_dfr(
  c(2005:2019, 2021:2022),
  function(x){
    tidycensus::get_acs(
      survey = "acs1",
      year = x,
      state = "MN",
      geography = "county",
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
          geography = "county",
          variables = c(total_pop = "B01003_001")
        ) %>% 
          mutate(acs_year = x)
      }
    )) %>% 
  filter(GEOID %in% cprg_county$GEOID) %>% 
  mutate(
    population_data_source = "ACS 1-Year Estimates, Table B01003",
    population = estimate
  )


pop_decennial <- 
  bind_rows(
    purrr::map_dfr(
      c(2010, 2020),
      function(x){
        tidycensus::get_decennial(
          geography = "county",
          state = "MN",
          variables = c(total_pop = "P1_001N")
        ) %>% 
          mutate(decennial_year = x)
      }
    ),
    purrr::map_dfr(
      c(2010, 2020),
      function(x){
        tidycensus::get_decennial(
          geography = "county",
          state = "WI",
          variables = c(total_pop = "P1_001N")
        ) %>% 
          mutate(decennial_year = x)
      }
    )) %>% 
  filter(GEOID %in% cprg_county$GEOID) %>% 
  mutate(
    population = value,
    population_data_source = "Decennial Census, Table P1"
  )


# confirm that the 2010 and 2020 population estimates
# to ensure they are the same across tables
pop_decennial %>% 
  left_join(pop_acs, by = c("decennial_year" = "acs_year",
                                    "GEOID",
                                    "NAME"))
  
  

# combine all 
cprg_population_timeseries <- 
  pop_acs %>% 
  select(GEOID, population_year = acs_year, population, population_data_source) %>% 
  # remove 2010 and 2020 from acs 1 year
  filter(!population_year %in% c(2010, 2020)) %>% 
  # bind pop decennial
  bind_rows(pop_decennial %>% 
              select(GEOID, 
                     population_year = decennial_year,
                     population, population_data_source)) %>% 
  # join with cprg_county to get metadata
  right_join(
    cprg_county %>% 
      sf::st_drop_geometry(),
    by = c("GEOID")
  ) %>% 
  arrange(STATE, NAME) %>% 
  select(names(cprg_county)[1:7],
         population_year, population, population_data_source)
  

# create metadata
cprg_population_timeseries_meta <- bind_rows(
  cprg_county_meta,
  tribble(
    ~Column, ~Class, ~Description,
    "population_year", class(cprg_population_timeseries$population_year), "Population estimate year",
    "population", class(cprg_population_timeseries$population), "Total county population estimate",
    "population_data_source", class(cprg_population_timeseries$population_data_source), "Population estimate data source"
  )
) %>%
  filter(Column %in% names(cprg_population_timeseries))

saveRDS(cprg_population_timeseries, "_meta/data/cprg_population_timeseries.RDS")
saveRDS(cprg_population_timeseries_meta, "_meta/data/cprg_population_timeseries_meta.RDS")
