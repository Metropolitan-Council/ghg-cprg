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
  separate_wider_delim(
    GEONAME, delim = ",", names = c("ctu_name", "county_name", "state")
    ) %>%
  select(
    geoid,
    ctuid = CTU_CODE,
    ctu_name,
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
    ctu_name = NAME,
    POPESTIMATE2000:CENSUS2010POP
  ) %>%
  pivot_longer(
    cols = !c(ctuid, geoid, ctu_name),
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

hassan_population <- ctu_estimates_2000 %>%
  filter(ctu_name == "Hassan township") %>%
  select(inventory_year, hassan_pop = ctu_population)

# update Rogers city population by adding Hassan township population for each year
ctu_estimates_2000 <- ctu_estimates_2000 %>%
  left_join(hassan_population, by = "inventory_year") %>%
  mutate(
    ctu_population = case_when(
      ctu_name == "Rogers city" & !is.na(hassan_pop) ~ ctu_population + hassan_pop,
      TRUE ~ ctu_population
    )
  ) %>%
  select(-hassan_pop) %>% 
  filter(ctu_name != "Hassan township")


## 2021-2023 ----
# from Met Council estimates

ctu_estimates_2021 <- readxl::read_xlsx("_meta/data-raw/population/EstimateSeries 1.xlsx") %>%
  mutate(
    geoid = paste0("27", COUNTY_CODE),
    population_data_source = "Met Council Intercensal Estimates 2024"
  ) %>%
  separate_wider_delim(
    GEONAME, delim = ",", names = c("ctu_name", "county_name", "state")
  ) %>%
  select(
    geoid,
    ctuid = CTU_ID_FIPS,,
    ctu_name,
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
  mutate(
    ctu_class = case_when(
      str_detect(ctu_name, "\\scity") ~ "CITY",
      str_detect(ctu_name, "\\stownship") ~ "TOWNSHIP",
      str_detect(ctu_name, "\\sUT") ~ "UNORGANIZED TERRITORY",
      .default = "NONE"
    ),
    ctu_name = str_remove(ctu_name, "\\scity"),
    ctu_name = str_remove(ctu_name, "\\stownship"),
    ctu_name = str_remove(ctu_name, "\\sUT"),
    ctu_name = str_replace(ctu_name, "St. ", "Saint ")
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
  ) %>% 
  select(
    geoid,
    ctuid,
    ctu_name,
    ctu_class,
    inventory_year,
    population_data_source,
    ctu_population,
    county_population,
    ctu_proportion_of_county_pop
  ) # added this for ordering columns

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
  "ctu_name", class(ctu_pop_estimates$ctu_name), "CTU name",
  "ctu_class", class(ctu_pop_estimates$ctu_class), "CTU class (either CITY, 
  TOWNSHIP, or UNORGANIZED TERRITORY",
  "inventory_year", class(ctu_pop_estimates$inventory_year), 
  "Population year, between 2000 and 2023",
  "population_data_source", class(ctu_pop_estimates$population_data_source),
  "Source of CTU-level population data",
  "ctu_population", class(ctu_pop_estimates$ctu_population),
  "Population of CTU in given year",
  "county_population", class(ctu_pop_estimates$county_population),
  "Population of county in given year",
  "ctu_proportion_of_county_pop", class(ctu_pop_estimates$ctu_proportion_of_county_pop),
  "Percentage of county population atttributed to this CTU in the given year"
)

saveRDS(ctu_pop_estimates, file.path(here::here(), "_meta/data/ctu_population.RDS"))
saveRDS(ctu_pop_meta, file.path(here::here(), "_meta/data/ctu_population_meta.RDS"))
