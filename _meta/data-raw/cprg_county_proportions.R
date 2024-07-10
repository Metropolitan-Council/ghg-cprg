source("R/_load_pkgs.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_county_meta <- readRDS("_meta/data/cprg_county_meta.RDS")

state_population <- readRDS("_meta/data/state_population.RDS")
census_county_population <- readRDS("_meta/data/census_county_population.RDS")

cprg_county_proportions <- census_county_population %>% 
  filter(cprg_area == TRUE) %>% 
  mutate(county_population = population) %>% 
  left_join(state_population,
            by = c("STATE", "population_year", "population_data_source")) %>% 
  mutate(
    year = population_year,
    county_proportion_of_state_pop = county_population/state_population %>% round(digits = 6),
    name = NAME) %>% 
  select(STATE, GEOID, name, year, county_population, state_population, county_proportion_of_state_pop,
         population_data_source)


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
