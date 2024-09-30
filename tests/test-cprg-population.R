testthat::test_that("County population is as expected", {
  cprg_population <- readRDS(file.path(here::here(), "_meta/data/cprg_population.RDS"))

  testthat::expect_equal(nrow(cprg_population), 11)
  testthat::expect_equal(
    cprg_population$county_name,
    c(
      "Anoka", "Carver", "Chisago", "Dakota", "Hennepin", "Ramsey",
      "Scott", "Sherburne", "Washington", "Pierce", "St. Croix"
    )
  )

  # Added STATE_ABB and year during 2005 baselining
  testthat::expect_equal(
    names(cprg_population),
    c(
      "state_name", "state_abb", "geoid", "county_name", "population",
      "population_data_source"
    )
  )

  testthat::expect_equal(cprg_population$state_name %>% unique(), c("Minnesota", "Wisconsin"))

  # these values were found on the Census web data viewer
  # check that they are correct

  cprg_population %>%
    dplyr::filter(county_name == "Hennepin") %>%
    magrittr::extract2("population") %>%
    testthat::expect_equal(1270283)


  cprg_population %>%
    dplyr::filter(county_name == "Ramsey") %>%
    magrittr::extract2("population") %>%
    testthat::expect_equal(549377)

  cprg_population %>%
    dplyr::filter(county_name == "Pierce") %>%
    magrittr::extract2("population") %>%
    testthat::expect_equal(42204)
})
