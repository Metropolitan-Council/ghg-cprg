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
  mutate(
    county_proportion_of_state_pop = county_population / state_population,
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
  ) %>% mutate(STATE = "Wisconsin")) %>%
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
  mutate(
    county_proportion_of_state_pop = county_population / state_population,
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

# intercensal population estimates provided by US Census for 2005 at county level
# downloaded from https://www.census.gov/data/tables/time-series/demo/popest/intercensal-2000-2010-counties.html and cleaned to read/save as CSV

intercensal_pop_2005_MN <- read_csv(here("_energy", "data-raw", "nhgis_blk2000_blk2010_MN", "co-est00int-01-27.csv")) %>%
  mutate(
    county_name = sub("^\\.", "", county_name),
    state = "MN"
  )
intercensal_pop_2005_WI <- read_csv(here("_energy", "data-raw", "nhgis_blk2000_blk2010_WI", "co-est00int-01-55.csv")) %>%
  mutate(
    county_name = sub("^\\.", "", county_name),
    state = "WI"
  )

# calculate total state populations in 2005 and add back as constant column
# MN
total_pop_MN <- intercensal_pop_2005_MN %>%
  summarise(state_population = sum(`2005`, na.rm = TRUE))

intercensal_pop_2005_MN <- intercensal_pop_2005_MN %>%
  mutate(
    year = 2005,
    state_population = total_pop_MN$state_population
  ) %>%
  rename(
    county_population = `2005`
  )

# WI
total_pop_WI <- intercensal_pop_2005_WI %>%
  summarise(state_population = sum(`2005`, na.rm = TRUE))

intercensal_pop_2005_WI <- intercensal_pop_2005_WI %>%
  mutate(
    year = 2005,
    state_population = total_pop_WI$state_population
  ) %>%
  rename(
    county_population = `2005`
  )

intercensal_pop_2005_MNWI <- rbind(intercensal_pop_2005_MN, intercensal_pop_2005_WI) %>%
  mutate(
    year = as.character(year),
    county_proportion_of_state_pop = county_population / state_population,
    population_data_source = "US Census County Intercensal Tables: 2000-2010 (2005)"
  )


# for population source of truth (cprg_population.RDS)
cprg_population_2005 <- cprg_county %>%
  left_join((intercensal_pop_2005_MNWI),
    by = join_by(NAMELSAD == county_name, STATE_ABB == state)
  ) %>%
  st_drop_geometry() %>%
  select(
    -state_population,
    -county_proportion_of_state_pop
  ) %>%
  rename(population = county_population)

# for proportions
cprg_county_population2005 <- cprg_county %>%
  left_join((intercensal_pop_2005_MNWI),
    by = join_by(NAMELSAD == county_name, STATE_ABB == state)
  ) %>%
  st_drop_geometry() %>%
  select(-NAMELSAD)

# save off 2005 base year
write_rds(cprg_population_2005, here(
  "_meta",
  "data",
  "cprg_population_2005.RDS"
))

cprg_county_proportions <-
  bind_rows(
    acs_proportions,
    decennial_proportions,
    cprg_county_population2005
  )


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
