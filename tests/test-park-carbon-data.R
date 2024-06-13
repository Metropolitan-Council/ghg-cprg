testthat::test_that("Park carbon data values are as expected",{
  
  cprg_county <- readRDS(file.path(here::here(),"_meta/data/cprg_county.RDS"))
  cprg_area <- cprg_county %>%
    mutate(area_sq_km = sf::st_area(cprg_county) %>% units::set_units("km^2") %>%
             as.numeric())
  
  
  park_carbon <- readRDS(file.path(here::here(), "_nature/data/park_landcover_sequestration_2021.RDS"))
  parks <- readRDS(file.path(here::here(), "_nature/data/regional_parks_shape.RDS"))
  carbon_stock <- readRDS(file.path(here::here(), "_meta/data/cprg_county_carbon_stock.RDS"))
  county_emissions <- readRDS(file.path(here::here(), "_meta/data/cprg_county_emissions.RDS"))
  
  carbon_stock_area <- left_join(
    carbon_stock %>%
      group_by(geog_name) %>%
      summarize(emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e)),
    cprg_area,
    by = c("geog_name" = "NAME")
  ) %>%
    mutate(stock_per_area = emissions_metric_tons_co2e / area_sq_km)
  
  
  cprg_7 <- vect(cprg_county %>% filter(!NAME %in% c("Sherburne", "Chisago", "St. Croix", "Pierce")))
  county_seq_7 <- county_emissions %>% filter(geog_name %in% cprg_7$NAME, sector == "Nature")
  county_stock_7 <- carbon_stock_area %>% filter(geog_name %in% cprg_7$NAME)
  
  
  park_area_total <- sum(expanse(parks, unit = "km")) # 307.684
  cprg_7_area <- sum(expanse(cprg_7, unit = "km")) # 7711.262
  park_area_ratio <- sum(expanse(parks, unit = "km")) / sum(expanse(cprg_7, unit = "km")) # 3.99%
  
  testthat::expect_equal(park_area_total, 307.684, tolerance = 0.01)
  testthat::expect_equal(cprg_7_area, 7711.262, tolerance = 0.01)
  testthat::expect_equal(park_area_ratio * 100, 3.99, tolerance = 0.01)
  
  
  
  park_area <- data.frame(agency = parks$AgencyMana, area = expanse(parks, unit = "km")) %>%
    group_by(agency) %>%
    summarize(area = sum(area))
  
  park_seq_ratio <- sum(park_carbon$sequestration_potential) / sum(county_seq_7$emissions_metric_tons_co2e) # 5.56%
  park_stock_ratio <- sum(park_carbon$stock_potential) / sum(county_stock_7$emissions_metric_tons_co2e) # 7.19%
  
  testthat::expect_equal(park_seq_ratio * 100, 5.56, tolerance = 0.01)
  testthat::expect_equal(park_stock_ratio * 100, 7.19, tolerance = 0.01)
  
})
