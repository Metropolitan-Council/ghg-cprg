source("R/_load_pkgs.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_county_meta <- readRDS("_meta/data/cprg_county_meta.RDS")
library(tidycensus)
options(tidycensus.cache = TRUE)
# To get started working with tidycensus, users should set their Census API key.
# A key can be obtained from http://api.census.gov/data/key_signup.html.
# census_api_key("YOUR API KEY GOES HERE")

# we will pull 2021 ACS 5-Year estimates for total population
# county level

cprg_population <- tidycensus::get_acs(
  survey = "acs5",
  year = 2021,
  state = "MN",
  geography = "county",
  variables = c(total_pop = "DP05_0001E")
) %>%
  bind_rows(
    tidycensus::get_acs(
      survey = "acs5",
      year = 2021,
      state = "WI",
      geography = "county",
      variables = c(total_pop = "DP05_0001E")
    )
  ) %>%
  select(-NAME, -moe) %>%
  right_join(
    cprg_county,
    by = c("GEOID")
  ) %>%
  sf::st_drop_geometry() %>%
  mutate(
    population = estimate,
    population_data_source = "ACS 5-Year Estimates 2021, Table DP05"
  ) %>%
  select(
    names(cprg_county),
    population,
    population_data_source,
    -geometry
  ) %>%
  arrange(STATE, NAME)

cprg_population

cprg_population_meta <- bind_rows(
  cprg_county_meta,
  tribble(
    ~Column, ~Class, ~Description,
    "population", class(cprg_population$population), "Total county population estimate",
    "population_data_source", class(cprg_population$population_data_source), "Population estimate data source"
  )
) %>%
  filter(Column %in% names(cprg_population))

saveRDS(cprg_population, "_meta/data/cprg_population.RDS")
saveRDS(cprg_population_meta, "_meta/data/cprg_population_meta.RDS")
