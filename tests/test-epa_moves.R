testthat::test_that("epa moves emissions rates", {
  epa_moves <- readRDS(file.path(here::here(), "_transportation/data/epa_moves.RDS"))
  source(file.path(here::here(), "R/global_warming_potential.R"))

  testthat::expect_equal(nrow(epa_moves), 3)

  testthat::expect_equal(
    names(epa_moves),
    c(
      "moves_year", "vehicle_weight", "co2",
      "co2_co2_equivalent",
      "ch4", "n2o"
    )
  )

  epa_moves %>%
    filter(vehicle_weight == "Passenger") %>%
    magrittr::extract2("co2_co2_equivalent") %>%
    testthat::expect_equal(356.439)

  # expect manual CO2e calculation to be same as
  # that in the dataset
  epa_moves %>%
    mutate(
      co2_co2_equivalent_test =
        sum(co2, (ch4 * gwp$ch4), (n2o * gwp$n2o))
    ) %>%
    filter(co2_co2_equivalent_test != co2_co2_equivalent) %>%
    nrow() %>%
    testthat::expect_equal(0)
})
