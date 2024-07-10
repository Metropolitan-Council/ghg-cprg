testthat::test_that("County population is as expected", {
  cprg_population <- readRDS(file.path(here::here(), "_meta/data/cprg_population.RDS"))

  testthat::expect_equal(nrow(cprg_population), -11)
  testthat::expect_equal(
    cprg_population$NAME,
    c(
      "Anoka", "Carver", "Chisago", "Dakota", "Hennepin", "Ramsey",
      "Scott", "Sherburne", "Washington", "Pierce", "St. Croix",
      "Anoka", "Carver", "Chisago", "Dakota", "Hennepin", "Ramsey",
      "Scott", "Sherburne", "Washington", "Pierce", "St. Croix"
    )
  )

  # Added STATE_ABB and year during 2005 baselining
  testthat::expect_equal(names(cprg_population), c(
    "STATE", "STATE_ABB", "STATEFP", "COUNTYFP", "GEOID",
    "NAME", "NAMELSAD", "population", "year", "population_data_source",
    "STATE", "STATE_ABB", "GEOID", "COUNTYFP",
    "NAME", "population", "population_data_source"
  ))

  testthat::expect_equal(cprg_population$STATE %>% unique(), c("Minnesota", "Wisconsin"))

  # these values were found on the Census web data viewer
  # check that they are correct

  cprg_population %>%
    dplyr::filter(NAME == "Hennepin" & year == 2021) %>%
    magrittr::extract2("population") %>%
    testthat::expect_equal(1270283)


  cprg_population %>%
    dplyr::filter(NAME == "Ramsey" & year == 2021) %>%
    magrittr::extract2("population") %>%
    testthat::expect_equal(549377)

  cprg_population %>%
    dplyr::filter(NAME == "Pierce" & year == 2021) %>%
    magrittr::extract2("population") %>%
    testthat::expect_equal(42204)
})
