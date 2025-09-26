testthat::test_that("County emissions data is as expected", {
  cprg_county_emissions <- readRDS(file.path(here::here(), "_meta/data/cprg_county_emissions.RDS"))

  testthat::expect_equal(
    sort(unique(cprg_county_emissions$county_name)),
    sort(c(
      "Scott", "Chisago", "Dakota", "Anoka", "Ramsey", "Washington",
      "Sherburne", "Hennepin", "Carver", "Pierce", "St. Croix", "MSP Airport"
    ))
  )

  testthat::expect_equal(
    names(cprg_county_emissions),
    c(
      "emissions_year", "geog_level", "geoid", "county_name", "sector", "category",
      "source", "value_emissions", "unit_emissions", "data_source", "factor_source",
      "county_total_population", "population_data_source", "emissions_per_capita"
    )
  )

  testthat::expect_equal(
    unique(cprg_county_emissions$sector),
    c("Transportation", "Residential", "Commercial", "Industrial", "Waste", "Agriculture", "Natural Systems")
  )



  testthat::expect_equal(
    unique(cprg_county_emissions$emissions_year),
    c(
      2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014,
      2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 1990, 1991, 1992,
      1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
      2004, 2023
    )
  )


  # temporarily comment out, since we expect NAs for 2005 sector level numbers
  # testthat::expect_false(any(is.na(cprg_county_emissions)))
})
