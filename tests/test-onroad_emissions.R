testthat::test_that("Compiled and summarized onroad emissions", {
  onroad_emissions <- readRDS(file.path(here::here(), "_transportation/data/onroad_emissions.RDS"))
  
  testthat::expect_equal(
    names(onroad_emissions),
    c("emissions_year", "data_source", "geoid", "county_name", "vehicle_weight_label", 
      "vehicle_fuel_label", "category", "emissions_metric_tons_co2e", 
      "vehicle_types_included", "fuel_types_included", "scc6_included", 
      "moves_edition")
  )
  
  testthat::expect_equal(
    unique(onroad_emissions$data_source),
    c("EQUATES", "National Emissions Inventory", "Air Emissions Modeling")
  )
  
  testthat::expect_equal(
    unique(onroad_emissions$vehicle_weight_label),
    structure(1:3, levels = c("Passenger", "Buses", "Trucks"), class = c("ordered", 
                                                                         "factor")))
  
  testthat::expect_equal(
    unique(onroad_emissions$category),
    c("Passenger vehicles", "Buses", "Trucks"))
  
  testthat::expect_equal(
    unique(onroad_emissions$vehicle_fuel_label),
    structure(c(1L, 2L, 4L), levels = c("Gasoline", "Diesel", "Non-Diesel", 
                                        "Other"), class = c("ordered", "factor")))
  
})

testthat::test_that("Highest and lowest year of emissions is as expected", {
  onroad_emissions <- readRDS(file.path(here::here(), "_transportation/data/onroad_emissions.RDS"))
  
  # check that the highest year of emissions matches Hennepin County, 
  # as expected
  onroad_maximum_year_county_vehicle <- onroad_emissions %>% 
    filter(emissions_metric_tons_co2e == max(emissions_metric_tons_co2e))
  
  testthat::expect_equal(
    onroad_maximum_year_county_vehicle$emissions_year,
    2007
  )
  
  testthat::expect_equal(
    onroad_maximum_year_county_vehicle$county_name,
    "Hennepin"
  )
  
  testthat::expect_equal(
    onroad_maximum_year_county_vehicle$category,
    "Passenger vehicles"
  )
  
  testthat::expect_equal(
    as.character(onroad_maximum_year_county_vehicle$vehicle_fuel_label),
    "Gasoline"
  )
  
  # check that the highest year of emissions matches Hennepin County, 
  # as expected
  onroad_minimum_year_county_vehicle <- onroad_emissions %>% 
    filter(emissions_metric_tons_co2e > 0) %>% 
    filter(emissions_metric_tons_co2e == min(emissions_metric_tons_co2e))
  
  testthat::expect_equal(
    onroad_minimum_year_county_vehicle$emissions_year,
    2016
  )
  
  testthat::expect_equal(
    onroad_minimum_year_county_vehicle$county_name,
    "Hennepin"
  )
  
  testthat::expect_equal(
    onroad_minimum_year_county_vehicle$category,
    "Buses"
  )
  
  testthat::expect_equal(
    as.character(onroad_minimum_year_county_vehicle$vehicle_fuel_label),
    "Other"
  )
})



testthat::test_that("Time series is as expected", {
  onroad_emissions <- readRDS(file.path(here::here(), "_transportation/data/onroad_emissions.RDS"))
  
  onroad_17on <-  onroad_emissions %>% 
    filter(emissions_year >= 2017) %>% 
    group_by(emissions_year, geoid, county_name, vehicle_weight_label) %>% 
    summarize(emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e, na.rm = T)) %>% 
    pivot_wider(names_from = emissions_year,
                values_from = emissions_metric_tons_co2e)
  
  
  # 2020 emissions were higher than 2019 for a few categories
  onroad_17on %>% 
    filter(`2020` > `2019`) %>% 
    ungroup() %>% 
    select(vehicle_weight_label) %>% 
    unique() %>% 
    extract2("vehicle_weight_label") %>% 
    as.character() %>% 
    testthat::expect_equal(c("Buses", "Trucks"))
  
  
  onroad_17on %>% 
    filter(`2020` > `2019`) %>% 
    ungroup() %>% 
    nrow() %>% 
    expect_equal(18)
  
  
  # but passsenger vehicles should be lower in 2020 than in 2019
  onroad_17on %>% 
    filter(`2020` > `2019`,
           vehicle_weight_label == "Passenger") %>% 
    nrow() %>% 
    testthat::expect_equal(0)
  
})


