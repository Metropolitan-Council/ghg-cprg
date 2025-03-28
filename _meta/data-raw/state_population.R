source("R/_load_pkgs.R")
cprg_county_meta <- readRDS("_meta/data/cprg_county_meta.RDS")

census_county_population <- readRDS("_meta/data/census_county_population.RDS")
demographer_state_population <- readRDS("_meta/data/state_population_demographer.RDS")


state_population <- census_county_population %>%
  group_by(state_name, state_abb, population_data_source, population_year) %>%
  summarize(
    state_population = sum(population, na.rm = T),
    .groups = "keep"
  ) %>%
  clean_names() %>%
  mutate(population_year = as.numeric(population_year)) %>%
  # use state demography office for 2021 on state ests
  filter(population_year < 2021) %>%
  bind_rows(
    .,
    demographer_state_population %>%
      filter(inventory_year >= 2021) %>%
      select(-households) %>%
      rename(
        state_population = population,
        population_year = inventory_year
      )
  )

# create metadata
state_population_meta <-
  bind_rows(
    cprg_county_meta,
    tribble(
      ~Column, ~Class, ~Description,
      "population_year", class(state_population$population_year), "Population estimate year",
      "state_population", class(state_population$state_population), "Total state population estimate",
      "population_data_source", class(state_population$population_data_source), "Population estimate data source"
    )
  ) %>%
  filter(Column %in% names(state_population))

saveRDS(state_population, "_meta/data/state_population.RDS")
saveRDS(state_population_meta, "_meta/data/state_population_meta.RDS")
