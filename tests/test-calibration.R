testthat::test_that("state DOT VMT", {
  commercial_calibration_points <- readRDS(file.path(here::here(), "_transportation/data/commercial_calibration_points.RDS"))

  testthat::expect_equal(nrow(commercial_calibration_points), 27)

  # minnesota
  # checked against https://arcg.is/mvfH40
  commercial_calibration_points %>%
    filter(sequence_number == "6784") %>%
    extract2("current_volume") %>%
    testthat::expect_equal(39152)


  commercial_calibration_points %>%
    filter(sequence_number == "9912") %>%
    extract2("current_volume") %>%
    testthat::expect_equal(19458)

  # ratios checked against what is in StreetLight
  # CPRG_Freight_Calibration_GateEdit_Reclass
  commercial_calibration_points %>%
    filter(sequence_number == "9912") %>%
    extract2("medium_commercial_ratio") %>%
    testthat::expect_equal(0.03)

  commercial_calibration_points %>%
    filter(sequence_number == "6783") %>%
    extract2("medium_commercial_ratio") %>%
    testthat::expect_equal(0.02)

  commercial_calibration_points %>%
    filter(sequence_number == "6783") %>%
    extract2("heavy_commercial_ratio") %>%
    testthat::expect_equal(0.02)

  # wisconsin
  # checked against https://arcg.is/0qrTXf
  commercial_calibration_points %>%
    filter(site_id == "550153") %>%
    extract2("current_volume") %>%
    round(digits = -2) %>%
    testthat::expect_equal(57500)

  commercial_calibration_points %>%
    filter(site_id == "550006") %>%
    extract2("current_volume") %>%
    round(digits = -2) %>%
    testthat::expect_equal(87200)

  # CPRG_Freight_Calibration_GateEdit_Reclass


  commercial_calibration_points %>%
    filter(site_id == "550006") %>%
    extract2("heavy_commercial_ratio") %>%
    testthat::expect_equal(0.11)

  commercial_calibration_points %>%
    filter(site_id == "470102") %>%
    extract2("heavy_commercial_ratio") %>%
    testthat::expect_equal(0.04)
})
