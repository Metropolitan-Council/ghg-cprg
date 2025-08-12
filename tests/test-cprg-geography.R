testthat::test_that("County data is as expected", {
  cprg_county <- readRDS(file.path(here::here(), "_meta/data/cprg_county.RDS"))
  ctu_co_crosswalk <- readRDS(file.path(here::here(), "_meta/data/geog_crosswalk.RDS"))

  testthat::expect_equal(nrow(cprg_county), 11)
  testthat::expect_equal(
    cprg_county$county_name,
    c(
      "Scott", "Chisago", "Dakota", "Anoka", "Ramsey", "Washington",
      "Sherburne", "Hennepin", "Carver", "Pierce", "St. Croix"
    )
  )
  testthat::expect_equal(
    names(cprg_county),
    c(
      "geoid", "county_name", "county_name_full", "state_name", "statefp",
      "state_abb", "cprg_area", "geometry"
    )
  )
  testthat::expect_equal(cprg_county$state_name %>% unique(), c("Minnesota", "Wisconsin"))
})

testthat::test_that("CTU data is as expected", {
  cprg_ctu <- readRDS(file.path(here::here(), "_meta/data/cprg_ctu.RDS"))
  ctu_co_crosswalk <- readRDS(file.path(here::here(), "_meta/data/geog_crosswalk.RDS"))

  testthat::expect_equal(nrow(cprg_ctu), 288)
  testthat::expect_equal(names(cprg_ctu), c(
    "ctu_name", "ctu_class", "county_name", "state_name", "statefp",
    "state_abb", "geoid_wis", "gnis", "cprg_area", "geometry", "ctu_id",
    "thrive_designation", "imagine_designation"
  ))

  testthat::expect_equal(cprg_ctu$state_name %>% unique(), c("Minnesota", "Wisconsin"))
  testthat::expect_equal(nrow(filter(ctu_co_crosswalk, is.na(geog_unit_id.parent))), 0)
})
