# This script sources intercensal population data for ctus within the 7-county region.
# Data from 2000-2010 comes from census estimates
# Data from 2011-2019 comes from Met Council estimates based on housing data

source("R/_load_pkgs.R")
# library(tidycensus)

## 2011-2019 ----
ctu_estimates_2011 <- readxl::read_xlsx("_meta/data-raw/population/IntercensalEstimates.xlsx") %>%
  mutate(
    geoid = paste0("27", COUNTY_CODE),
    population_data_source = "Met Council Intercensal Estimates 2024"
  ) %>%
  # separate_wider_delim(
  #   GEONAME, delim = ",", names = c("ctu_name", "county_name", "state")
  #   ) %>%
  select(
    geoid,
    ctuid = CTU_CODE,
    # ctu_name,
    inventory_year = EST_YEAR,
    ctu_population = POPTOTAL_EST,
    population_data_source
  )


## 2000-2009 ------
# Using Intercensal year estimates from US Census

# directly download intercensal data years from census.gov
# if they don't already exist
if (!file.exists("_meta/data-raw/population/sub-est00int.csv")) {
  # download directly from census.gov
  download.file("https://www2.census.gov/programs-surveys/popest/datasets/2000-2010/intercensal/cities/sub-est00int.csv",
    destfile = "_meta/data-raw/population/sub-est00int.csv",
    mode = "wb"
  )
}

ctu_estimates_2000 <- read.csv("_meta/data-raw/population/sub-est00int.csv") %>%
  filter(STATE == 27) %>%
  mutate(
    ctuid = str_pad(COUSUB, 5, pad = "0"),
    geoid = paste0(STATE, str_pad(COUNTY, 3, pad = "0"))
  ) %>%
  filter(
    # ctuid %in% ctu_estimates_2011$ctuid,
    geoid %in% ctu_estimates_2011$geoid,
    SUMLEV == 61 # thank you Matt for knowing things!
  ) %>%
  select(
    geoid,
    ctuid,
    POPESTIMATE2000:CENSUS2010POP
  ) %>%
  pivot_longer(
    cols = !c(ctuid, geoid),
    names_to = "inventory_year",
    names_prefix = "POPESTIMATE",
    values_to = "ctu_population"
  ) %>%
  mutate(
    inventory_year = if_else(
      inventory_year == "CENSUS2010POP", "2010", inventory_year
    ),
    inventory_year = as.numeric(inventory_year),
    population_data_source = "Census Bureau Intercensal Estimates"
  )

## 2020 ----
# from decennial census
# jk it's already in the 2011 series

# ctu_2020 <- tidycensus::get_decennial(
#   geography = "county subdivision",
#   variables = "DP1_0001C",
#   year = 2020,
#   sumfile = "dp",
#   state = "MN"
# ) %>%
#   mutate(
#     geoid = str_sub(GEOID, start = 1, end = 5),
#     ctuid = str_sub(GEOID, start = 6, end = 10),
#     inventory_year = 2020
#   ) %>%
#   filter(
#     geoid %in% ctu_estimates_2011$geoid
#   ) %>%
#   select(
#     geoid,
#     ctuid,
#     inventory_year,
#     ctu_population = value
#   )

## 2021-2023 ----
# from Met Council estimates

ctu_estimates_2021 <- readxl::read_xlsx("_meta/data-raw/population/EstimateSeries 1.xlsx") %>%
  mutate(
    geoid = paste0("27", COUNTY_CODE),
    population_data_source = "Met Council Intercensal Estimates 2024"
  ) %>%
  select(
    geoid,
    ctuid = CTU_ID_FIPS,
    inventory_year = EST_YEAR,
    ctu_population = POPTOTAL_EST,
    population_data_source
  )


# join ----

ctu_pop_estimates <- ctu_estimates_2000 %>%
  rbind(
    ctu_estimates_2011,
    # ctu_2020,
    ctu_estimates_2021
  ) %>%
  group_by(
    geoid, inventory_year
  ) %>%
  mutate(
    county_population = sum(ctu_population)
  ) %>%
  ungroup() %>%
  mutate(
    ctu_proportion_of_county_pop = ctu_population / county_population
  )

## test ----
# pop_summary <- ctu_pop_estimates %>%
#   group_by(inventory_year, geoid) %>%
#   summarize(pop_totals = sum(ctu_population))
#
# ggplot(pop_summary, aes(x = inventory_year, y = pop_totals, color = geoid)) +
#   geom_line()
#
# ctu_pop_estimates %>%
#   group_by(geoid, inventory_year) %>%
#   summarize(total_proportion = sum(ctu_proportion_of_county_pop)) %>%
#   filter(total_proportion != 1)

## save ----
ctu_pop_meta <- tribble(
  ~Column, ~Class, ~Description,
  "geoid", class(ctu_pop_estimates$geoid), "GEOID tag for MN county",
  "ctuid", class(ctu_pop_estimates$ctuid), "CTU census tag",
  "inventory_year", class(ctu_pop_estimates$inventory_year), "Population year, between 2000 and 2023",
  "ctu_population", class(ctu_pop_estimates$ctu_population),
  "Population of CTU in given year",
  "county_population", class(ctu_pop_estimates$county_population),
  "Population of county in given year",
  "ctu_proportion_of_county_pop", class(ctu_pop_estimates$ctu_proportion_of_county_pop),
  "Percentage of county population atttributed to this CTU in the given year",
  "population_data_source", class(ctu_pop_estimates$population_data_source),
  "Source of CTU-level population data"
)

saveRDS(ctu_pop_estimates, file.path(here::here(), "_meta/data/ctu_population.RDS"))
saveRDS(ctu_pop_meta, file.path(here::here(), "_meta/data/ctu_population_meta.RDS"))
