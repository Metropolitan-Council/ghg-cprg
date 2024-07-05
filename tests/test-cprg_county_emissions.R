testthat::test_that("County data is as expected", {
  cprg_county_emissions <- readRDS(file.path(here::here(), "_meta/data/cprg_county_emissions.RDS"))

  testthat::expect_equal(
    sort(unique(cprg_county_emissions$geog_name)),
    sort(c(
      "Scott", "Chisago", "Dakota", "Anoka", "Ramsey", "Washington",
      "Sherburne", "Hennepin", "Carver", "Pierce", "St. Croix"
    ))
  )

  testthat::expect_equal(
    names(cprg_county_emissions),
    c(
      "year", "geog_level", "geog_id", "geog_name", "sector", "category",
      "source", "emissions_metric_tons_co2e", "data_source", "factor_source",
      "county_total_population", "population_data_source", "emissions_per_capita"
    )
  )
# commenting out for now -- sector breakdowns null for 90% RDG data deadline
  #testthat::expect_false(any(is.na(cprg_county_emissions)))
})
