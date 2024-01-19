testthat::test_that("Correct number of calibration zone sets", {
  commercial_calibration_lines <- readRDS(file.path(here::here(), "_transportation/data/commercial_calibration_lines.RDS"))

  testthat::expect_equal(nrow(commercial_calibration_lines), 27)
})
