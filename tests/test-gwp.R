testthat::test_that("GWP values", {
  source(file.path(here::here(), "R/global_warming_potential.R"))

  testthat::expect_equal(length(gwp), 5)

  testthat::expect_equal(gwp$co2, 1)
  testthat::expect_equal(gwp$ch4, 27.9)
  testthat::expect_equal(gwp$n2o, 273)
})
