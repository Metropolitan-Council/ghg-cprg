testthat::test_that("County emissions data is as expected", {
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
      "year", "geog_level", "geoid", "geog_name", "sector", "category",
      "source", "emissions_metric_tons_co2e", "data_source", "factor_source",
      "county_total_population", "population_data_source", "emissions_per_capita"
    )
  )

  testthat::expect_equal(
    unique(cprg_county_emissions$sector),
    c("Transportation", "Energy", "Waste", "Nature")
  )

  testthat::expect_equal(
    unique(levels(cprg_county_emissions$source)),
    c(
      "Gasoline fueled vehicles",
      "Diesel fueled vehicles", "Other fueled vehicles", "Landfill",
      "Waste to energy", "Recycling", "Organics", "Wastewater", "Electricity",
      "Natural gas", "Propane", "Kerosene", "Urban grassland", "Urban tree",
      "Grassland", "Tree", "Wetland"
    )
  )


  testthat::expect_equal(
    unique(cprg_county_emissions$year),
    c(
      2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011,
      2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022
    )
  )


  # temporarily comment out, since we expect NAs for 2005 sector level numbers
  # testthat::expect_false(any(is.na(cprg_county_emissions)))
})
