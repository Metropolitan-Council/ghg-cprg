testthat::test_that("EPA wastewater", {
  cprg_county_emissions <- readRDS(file.path(here::here(), "_meta/data/cprg_county_emissions.RDS"))

  county_wastewater_emissions <- cprg_county_emissions %>%
    filter(category == "Wastewater")

  ## test that are the expected number of years
  testthat::expect_equal(
    unique(county_wastewater_emissions$emissions_year),
    2005:2022
  )

  # test that the largest population county has the largest ww emissions
  county_wastewater_emissions %>%
    filter(value_emissions == max(value_emissions)) %>%
    magrittr::extract2("county_name") %>%
    testthat::expect_equal("Hennepin")
})
