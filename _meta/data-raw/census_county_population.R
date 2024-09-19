# county population data ------
# create population timeseries for county level back to the year 2000
# US Census products available vary by year and geography
# For years 2000, 2010, and 2020, we will use the Decennial Census
# For years 2001-2009 and 2011-2019, we will use Intercensal year estimates
# For years 2021-2022 (and onward), we will use ACS 5-year estimates

source("R/_load_pkgs.R")
source("R/download_read_table.R")
library(tidycensus)
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_county_meta <- readRDS("_meta/data/cprg_county_meta.RDS")
source("_meta/data-raw/county_geography.R")




# 2001-2009 ----
# 2001-2009: intercensal year table
# directly download intercensal data years from census.gov
# if they don't already exist
if (!file.exists("_meta/data-raw/population/co-est00int-01-27.xls")) {
  # make a folder
  fs::dir_create("_meta/data-raw/population/")
  # download directly from census.gov
  download.file("https://www2.census.gov/programs-surveys/popest/tables/2000-2010/intercensal/county/co-est00int-01-27.xls",
    destfile = "_meta/data-raw/population/co-est00int-01-27.xls",
    mode = "wb"
  )
  download.file("https://www2.census.gov/programs-surveys/popest/tables/2000-2010/intercensal/county/co-est00int-01-55.xls",
    destfile = "_meta/data-raw/population/co-est00int-01-55.xls",
    mode = "wb"
  )
}



county_pop_intercensal1 <- download_read_table(
  "https://www2.census.gov/programs-surveys/popest/tables/2000-2010/intercensal/county/co-est00int-01-27.xls",
  exdir = "_meta/data-raw/population",
  skip = 3
) %>%
# if the above code chunk fails with error:
# libxls error: Unable to open file  
# try manually opening downloaded xls files above and resaving. 
# restart by uncommenting below code and 
  mutate(NAMELSAD = `...1`) %>%
  # remove state total row and metadata rows
  filter(stringr::str_detect(NAMELSAD, "County")) %>%
  # we will use the official 2000 and 2010 (April 1) estimates
  # and the July 1 estimates for all intercensal years
  select(NAMELSAD, everything(),
    -`2000`,
    `2000` = `...2`,
    -`...1`,
    `2010` = `...14`,
    -`...13`
  ) %>%
  mutate(
    NAMELSAD = stringr::str_sub(NAMELSAD, start = 2, end = -1),
    STATE = "Minnesota"
  ) %>%
  bind_rows(
    # repeat process for Wisconsin
    download_read_table(
      "https://www2.census.gov/programs-surveys/popest/tables/2000-2010/intercensal/county/co-est00int-01-55.xls",
      exdir = "_meta/data-raw/population",
      skip = 3
    ) %>%
      mutate(NAMELSAD = `...1`) %>%
      filter(stringr::str_detect(NAMELSAD, "County")) %>%
      select(NAMELSAD, everything(),
        -`2000`,
        `2000` = `...2`,
        -`...1`,
        `2010` = `...14`,
        -`...13`
      ) %>%
      mutate(
        NAMELSAD = stringr::str_sub(NAMELSAD, start = 2, end = -1),
        STATE = "Wisconsin"
      )
  ) %>%
  group_by(NAMELSAD, STATE) %>%
  pivot_longer(
    cols = 2:12,
    names_to = "population_year",
    values_to = "population"
  ) %>%
  mutate(population_data_source = ifelse(population_year %in% c(2000, 2010),
    "US Decennial Census",
    "US Census County Intercensal Tables (CO-EST00INT-01)"
  )) %>%
  left_join(county_geography %>%
    select(
      STATE, STATEFP, COUNTYFP, GEOID,
      NAMELSAD, NAME
    ))

# 2011-2019 -----

# download directly if not already existing

county_pop_intercensal2 <- download_read_table(
  url = "https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/counties/totals/co-est2020.csv",
  exdir = "_meta/data-raw/population/",
  col_types = c(
    rep("c", 7),
    rep("d", 14)
  )
) %>%
  clean_names() %>%
  # filter for only our states
  filter(
    stname %in% county_pop_intercensal1$STATE,
    # remove state level totals
    !ctyname %in% county_pop_intercensal1$STATE
  ) %>%
  mutate(census2010pop = as.numeric(census2010pop)) %>%
  # pivot longer
  pivot_longer(
    cols = 8:21,
    names_to = "population_source_year",
    values_to = "population"
  ) %>%
  mutate(
    GEOID = paste0(state, county),
    STATE = stname,
    NAMELSAD = ctyname,
    NAME = str_remove(ctyname, " County"),
    # extract the year from the population_source_year
    population_year = str_extract(population_source_year, "[:digit:][:digit:][:digit:][:digit:]"),
    population_data_source = ifelse(population_year %in% c(2000, 2010, 2020),
      "US Decennial Census",
      "US Census County Intercensal Tables (CO-EST2020)"
    )
  ) %>%
  select(-1:-3) %>%
  filter(
    !population_source_year %in% c(
      "estimatesbase2010",
      "popestimate2010"
    ),
    population_year != "0420"
  )


# combine both intercensal tables
county_pop_intercensal <- bind_rows(
  county_pop_intercensal1,
  county_pop_intercensal2
) %>%
  arrange(population_year) %>%
  select(1:5, GEOID, NAME)

# 2000, 2010, 2020 -----
# Decennial census years
#
# for a given state, pull the 2000, 2010, and 2020
# total population variables and combine into a single
# output table
fetch_combine_decennial <- function(state_name) {
  x2020 <- tidycensus::get_decennial(
    geography = "county",
    state = state_name,
    year = 2020,
    variables = c(total_pop = "P1_001N")
  ) %>%
    mutate(
      population_year = "2020",
      population_data_source = "Decennial Census, Table P1",
      STATE = state_name,
      population = value
    )

  x2010 <- get_decennial(
    geography = "county",
    state = state_name,
    year = 2010,
    variables = c(total_pop = "P010001")
  ) %>%
    mutate(
      population_year = "2010",
      population_data_source = "Decennial Census, Table P1",
      STATE = state_name,
      population = value
    )

  x2000 <- get_decennial(
    geography = "county",
    year = 2000,
    state = state_name,
    variable = c(total_pop = "P001001")
  ) %>%
    mutate(
      population_year = "2000",
      population_data_source = "Decennial Census, Table P1",
      STATE = state_name,
      population = value
    )

  bind_rows(
    x2000,
    x2010,
    x2020
  ) %>%
    return()
}

county_pop_decennial <- bind_rows(
  fetch_combine_decennial("Minnesota"),
  fetch_combine_decennial("Wisconsin")
)


# 2021, 2022, onward -----
# ACS 5-year estimates

county_pop_acs <- purrr::map_dfr(
  c(2021:2022),
  # for each year and state, fetch the population table
  function(x) {
    tidycensus::get_acs(
      survey = "acs5",
      year = x,
      state = "MN",
      geography = "county",
      variables = c(total_pop = "DP05_0001E")
    ) %>%
      mutate(
        population_year = as.character(x),
        STATE = "Minnesota",
        population = estimate
      )
  }
) %>%
  bind_rows(
    purrr::map_dfr(
      c(2021:2022),
      function(x) {
        tidycensus::get_acs(
          survey = "acs5",
          year = x,
          state = "WI",
          geography = "county",
          variables = c(total_pop = "DP05_0001E")
        ) %>%
          mutate(
            population_year = as.character(x),
            STATE = "Wisconsin",
            population = estimate
          )
      }
    )
  ) %>%
  mutate(
    population_data_source = "ACS 5-Year Estimates, Table DP05"
  )


# combine all datasets -----

names(county_pop_acs)
names(county_pop_decennial)
names(county_pop_intercensal)


# check that the decennial census data we pulled using tidycensus
# matches that from the intercensal
county_pop_decennial %>%
  left_join(county_pop_intercensal, by = c("GEOID", "population_year")) %>%
  filter(value != population.x) %>%
  nrow() %>%
  testthat::expect_equal(0)

# combine all tables into one
census_county_population <- county_pop_intercensal %>%
  ungroup() %>%
  filter(!population_year %in% county_pop_decennial$population_year) %>%
  bind_rows(county_pop_decennial) %>%
  bind_rows(county_pop_acs) %>%
  select(-NAME, -NAMELSAD, -estimate, -moe, -variable, -value) %>%
  left_join(county_geography %>%
    select(GEOID, NAME, STATE, STATE_ABB, COUNTYFP, NAMELSAD)) %>%
  # add variable discerning whether the county is in our study area
  mutate(cprg_area = ifelse(GEOID %in% cprg_county$geoid, TRUE, FALSE)) %>%
  select(
    STATE, STATE_ABB, GEOID, COUNTYFP, NAME,
    population_year, population, population_data_source, cprg_area
  ) %>%
  arrange(STATE, population_year) %>%
  clean_names() %>%
  select(
    geoid,
    county_name = name,
    state_name = state,
    state_abb,
    population_year,
    population_data_source,
    cprg_area,
    population
  ) %>%
  unique()


# review aggregations ------

census_county_population %>%
  group_by(state_name, population_year) %>%
  summarize(population = sum(population)) %>%
  filter(population_year == 2000)

# create metadata and save!  -----
names(census_county_population)
names(cprg_county)

census_county_population_meta <- bind_rows(
  cprg_county_meta,
  tribble(
    ~Column, ~Class, ~Description,
    "population_year", class(census_county_population$population_year), "Population estimate year",
    "population", class(census_county_population$population), "Total county population estimate (persons)",
    "population_data_source", class(census_county_population$population_data_source), "Population estimate data source"
  )
) %>%
  filter(Column %in% names(census_county_population))


saveRDS(census_county_population, "_meta/data/census_county_population.RDS")
saveRDS(census_county_population_meta, "_meta/data/census_county_population_meta.RDS")
