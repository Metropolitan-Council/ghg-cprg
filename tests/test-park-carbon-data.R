testthat::test_that("Park carbon data values are as expected", {
  cprg_county <- readRDS(file.path(here::here(), "_meta/data/cprg_county.RDS"))
  cprg_area <- cprg_county %>%
    mutate(area_sq_km = sf::st_area(cprg_county) %>% units::set_units("km^2") %>%
      as.numeric())


  park_carbon <- readRDS(file.path(here::here(), "_nature/data/park_landcover_sequestration_2021.RDS"))
  parks <- readRDS(file.path(here::here(), "_nature/data/regional_parks_shape.RDS"))
  carbon_stock <- readRDS(file.path(here::here(), "_meta/data/cprg_county_carbon_stock.RDS"))
  county_emissions <- readRDS(file.path(here::here(), "_meta/data/cprg_county_emissions.RDS"))

  carbon_stock_area <- left_join(
    carbon_stock %>%
      group_by(county_name) %>%
      summarize(value_emissions = sum(value_emissions)),
    cprg_area,
    by = c("county_name" = "county_name")
  ) %>%
    mutate(stock_per_area = value_emissions / area_sq_km)


  cprg_7 <- vect(cprg_county %>% filter(!county_name %in% c("Sherburne", "Chisago", "St. Croix", "Pierce")))
  county_seq_7 <- county_emissions %>% filter(county_name %in% cprg_7$county_name, sector == "Natural Systems",
                                              emissions_year == 2021)
  county_stock_7 <- carbon_stock_area %>% filter(geog_name %in% cprg_7$county_name)


  park_area_total <- sum(expanse(parks, unit = "km")) # 307.684
  cprg_7_area <- sum(expanse(cprg_7, unit = "km")) # 7711.262
  park_area_ratio <- sum(expanse(parks, unit = "km")) / sum(expanse(cprg_7, unit = "km")) # 3.99%

  testthat::expect_equal(park_area_total, 307.684, tolerance = 0.01)
  testthat::expect_equal(cprg_7_area, 7711.262, tolerance = 0.01)
  testthat::expect_equal(park_area_ratio * 100, 3.99, tolerance = 0.01)



  park_area <- data.frame(agency = parks$AgencyMana, area = expanse(parks, unit = "km")) %>%
    group_by(agency) %>%
    summarize(area = sum(area))

  park_seq_ratio <- sum(park_carbon$sequestration_potential) / sum(county_seq_7$value_emissions) # 5.56%
  park_stock_ratio <- sum(park_carbon$stock_potential) / sum(county_stock_7$emissions_metric_tons_co2e) # 7.19%

  testthat::expect_equal(park_seq_ratio * 100, 6.77, tolerance = 0.01)
  testthat::expect_equal(park_stock_ratio * 100, 7.19, tolerance = 0.01)
})
