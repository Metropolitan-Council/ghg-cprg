testthat::test_that("Correct number of calibration zone sets", {
  source(file.path(here::here(), "_transportation/data-raw/_calculate_emissions.R"))

  vmt <- readRDS(file.path(here::here(), "_transportation/data/county_vmt_emissions.RDS")) %>%
    select(1:11)

  epa_moves <- readRDS(file.path(here::here(), "_transportation/data/epa_moves.RDS"))


  emiss <- calculate_emissions(
    vmt,
    epa_moves
  )


  testthat::expect_equal(
    names(emiss),
    c(
      "analysis_name", "mode_of_travel", "year", "vehicle_type",
      "vehicle_weight", "vehicle_weight_label", "zone", "vmt_same",
      "vmt_origin", "vmt_destination", "vmt_total", "moves_year", "co2",
      "co2_co2_equivalent", "ch4", "n2o", "total_co2", "total_ch4",
      "total_n2o", "total_co2_w_equiv", "emissions_metric_tons_co2e"
    )
  )

  testthat::expect_equal(
    unique(emiss$zone),
    c(
      "Anoka", "Carver", "Chisago", "Dakota", "Hennepin", "Pierce",
      "Ramsey", "Scott", "Sherburne", "St. Croix", "Washington"
    )
  )

  testthat::expect_equal(
    unique(emiss$vehicle_weight),
    structure(1:3, levels = c("Passenger", "Medium", "Heavy"), class = c(
      "ordered",
      "factor"
    ))
  )


  # expect Hennepin county to have the highest emissions
  emiss %>%
    filter(emissions_metric_tons_co2e == max(emissions_metric_tons_co2e)) %>%
    magrittr::extract2("zone") %>%
    testthat::expect_equal("Hennepin")



  # expect Pierce county to have the *lowest* VMT
  emiss %>%
    filter(emissions_metric_tons_co2e == min(emissions_metric_tons_co2e)) %>%
    magrittr::extract2("zone") %>%
    testthat::expect_equal("Pierce")
})
