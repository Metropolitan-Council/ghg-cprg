testthat::test_that("County population is as expected", {
  cprg_population <- readRDS(file.path(here::here(), "_meta/data/cprg_population.RDS"))

  testthat::expect_equal(nrow(cprg_population), 11)
  testthat::expect_equal(
    cprg_population$NAME,
    c(
      "Anoka", "Carver", "Chisago", "Dakota", "Hennepin", "Ramsey",
      "Scott", "Sherburne", "Washington", "Pierce", "St. Croix"
    )
  )

  testthat::expect_equal(names(cprg_population), c(
    "STATE", "STATE_ABB", "GEOID", "COUNTYFP",
    "NAME", "population", "population_data_source"
  ))

  testthat::expect_equal(cprg_population$STATE %>% unique(), c("Minnesota", "Wisconsin"))

  # these values were found on the Census web data viewer
  # check that they are correct

  cprg_population %>%
    dplyr::filter(NAME == "Hennepin") %>%
    magrittr::extract2("population") %>%
    testthat::expect_equal(1270283)


  cprg_population %>%
    dplyr::filter(NAME == "Ramsey") %>%
    magrittr::extract2("population") %>%
    testthat::expect_equal(549377)

  cprg_population %>%
    dplyr::filter(NAME == "Pierce") %>%
    magrittr::extract2("population") %>%
    testthat::expect_equal(42204)
})
