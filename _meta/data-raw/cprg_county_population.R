source("R/_load_pkgs.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_county_meta <- readRDS("_meta/data/cprg_county_meta.RDS")

census_county_population <- readRDS("_meta/data/census_county_population.RDS")

cprg_population <- census_county_population %>% 
  filter(cprg_area == TRUE,
         population_year == 2021) %>% 
  select(STATE, STATE_ABB, GEOID, COUNTYFP,
         NAME, population, population_data_source)


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
