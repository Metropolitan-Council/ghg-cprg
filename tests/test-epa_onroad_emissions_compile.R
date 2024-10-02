testthat::test_that("Compiled onroad emissions", {
  epa_onroad_emissions_compile <- readRDS(file.path(here::here(), "_transportation/data/epa_onroad_emissions_compile.RDS"))

  # test for values in factor/character columns

  testthat::expect_equal(
    unique(epa_onroad_emissions_compile$data_source),
    c("EQUATES", "National Emissions Inventory", "Air Emissions Modeling")
  )

  testthat::expect_equal(
    unique(epa_onroad_emissions_compile$pollutant),
    c(
      "emissions_metric_tons_co2e", "co2", "ch4", "n2o", "co", "no",
      "nox", "so2", "nh3", "pm10_pri", "pm25_pri"
    )
  )

  testthat::expect_equal(
    unique(epa_onroad_emissions_compile$vehicle_type),
    c(
      "Motorcycles", "Passenger cars", "Passenger trucks", "Light commercial trucks",
      "Intercity buses", "Transit buses", "School buses", "Refuse trucks",
      "Single unit short-haul trucks", "Single unit long-haul trucks",
      "Motor homes", "Combination short-haul trucks", "Combination long-haul trucks",
      "Trucks and buses", "Gas stations"
    )
  )

  testthat::expect_equal(
    sort(unique(epa_onroad_emissions_compile$fuel_type)),
    c(
      "Compressed natural gas (CNG)", "Diesel", "Electric", "Ethanol (E-85)",
      "Gasoline"
    )
  )
})
