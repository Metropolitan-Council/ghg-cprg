test_that("All population", {
  census_county_population <- readRDS(file.path(here::here(), "_meta/data/census_county_population.RDS"))


  testthat::expect_equal(nrow(census_county_population), 3657)

  testthat::expect_equal(
    names(census_county_population),
    c(
      "geoid", "county_name", "state_name", "state_abb", "population_year",
      "population_data_source", "cprg_area", "population"
    )
    # c(
    #   "STATE", "STATE_ABB", "GEOID",
    #   "COUNTYFP", "NAME",
    #   "population_year", "population",
    #   "population_data_source", "cprg_area"
    # )
  )


  # expect specific Pierce Co 2018 value
  census_county_population %>%
    filter(
      county_name == "Pierce",
      population_year == 2018
    ) %>%
    extract2("population") %>%
    testthat::expect_equal(42608)


  # expect specific Hennepin Co 2014 value
  census_county_population %>%
    filter(
      county_name == "Hennepin",
      population_year == 2014
    ) %>%
    extract2("population") %>%
    testthat::expect_equal(1211635)

  # expect 2021 data to be from the ACS 5 year estimates
  census_county_population %>%
    filter(population_year == 2021) %>%
    select(population_data_source) %>%
    unique() %>%
    extract2("population_data_source") %>%
    testthat::expect_equal("ACS 5-Year Estimates, Table DP05")

  # test upper and lower county values
  census_county_population %>%
    filter(population == max(population)) %>%
    extract2("county_name") %>%
    testthat::expect_equal("Hennepin")

  census_county_population %>%
    filter(population == min(population)) %>%
    extract2("county_name") %>%
    testthat::expect_equal("Traverse")


  # test statewide totals
  state_population <- census_county_population %>%
    group_by(state_name, population_year) %>%
    summarize(
      state_population = sum(population, na.rm = T),
      .groups = "keep"
    )


  state_population %>%
    filter(
      state_name == "Minnesota",
      population_year == "2011"
    ) %>%
    magrittr::extract2("state_population") %>%
    testthat::expect_equal(5346620)


  state_population %>%
    filter(
      state_name == "Wisconsin",
      population_year == "2007"
    ) %>%
    magrittr::extract2("state_population") %>%
    testthat::expect_equal(5610775)
})
