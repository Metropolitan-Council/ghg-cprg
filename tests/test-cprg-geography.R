testthat::test_that("County data is as expected", {
  cprg_county <- readRDS(file.path(here::here(), "_meta/data/cprg_county.RDS"))

  testthat::expect_equal(nrow(cprg_county), 11)
  testthat::expect_equal(
    cprg_county$NAME,
    c(
      "Scott", "Chisago", "Dakota", "Anoka", "Ramsey", "Washington",
      "Sherburne", "Hennepin", "Carver", "Pierce", "St. Croix"
    )
  )
  testthat::expect_equal(names(cprg_county), c(
    "STATE", "STATEFP", "COUNTYFP", "GEOID",
    "NAME", "NAMELSAD",
    "geometry"
  ))
  testthat::expect_equal(cprg_county$STATE %>% unique(), c("Minnesota", "Wisconsin"))
})

testthat::test_that("CTU data is as expected", {
  cprg_ctu <- readRDS(file.path(here::here(), "_meta/data/cprg_ctu.RDS"))

  testthat::expect_equal(nrow(cprg_ctu), 289)
  testthat::expect_equal(names(cprg_ctu), c(
    "CTU_NAME", "CTU_CLASS", "COUNTY_NAM",
    "STATEFP", "STATE",
    "GNIS_FEATU", "geometry", "GEOID"
  ))

  testthat::expect_equal(cprg_ctu$STATE %>% unique(), c("Minnesota", "Wisconsin"))
})
