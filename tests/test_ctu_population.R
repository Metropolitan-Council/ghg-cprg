testthat::test_that("CTU population is as expected", {
  ctu_population <- readRDS(file.path(here::here(), "_meta/data/ctu_population.RDS"))
  
  testthat::expect_equal(
    unique(ctu_population$geoid),
    c("27003", "27019", "27037", "27053", "27123", "27139", "27163")
    )
  
  
})
