testthat::test_that("EPA wastewater", {
  epa_county_wastewater <- readRDS(file.path(here::here(), "_waste/data/epa_county_wastewater.RDS"))
  cprg_county_proportions <- readRDS(file.path(here::here(), "_meta/data/cprg_county_proportions.RDS"))
  epa_state_wastewater_by_year <- readRDS(file.path(here::here(), "_waste/data-raw/wastewater/epa_state_wastewater_by_year.RDS"))

  testthat::expect_equal(nrow(epa_county_wastewater), 11)

  # double check manual calculation
  cprg_county_proportions %>%
    filter(year == 2021) %>%
    select(STATE, NAME, county_proportion_of_state_pop) %>%
    left_join(epa_state_wastewater_by_year %>%
      filter(Year == 2021) %>%
      group_by(STATE) %>%
      summarize(emissions = sum(CO2e)), by = "STATE") %>%
    mutate(epa_co2e_test = county_proportion_of_state_pop * emissions) %>%
    left_join(epa_county_wastewater, by = "NAME") %>%
    filter(epa_co2e_test != epa_co2e) %>%
    nrow() %>%
    testthat::expect_equal(0)


  # double check against epa-mn-wastewater.csv
  epa_state_wastewater_by_year %>%
    filter(
      Year == 2021,
      State == "MN"
    ) %>%
    magrittr::extract2("CO2e") %>%
    testthat::expect_equal(c(400000, 150000))

  # double check against epa-wi-wastewater.csv
  epa_state_wastewater_by_year %>%
    filter(
      Year == 2021,
      State == "WI"
    ) %>%
    magrittr::extract2("CO2e") %>%
    testthat::expect_equal(c(410000, 160000))
})
