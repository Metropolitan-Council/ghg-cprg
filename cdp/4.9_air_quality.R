# [explain]

criteria_pollutants <- read.csv(file.path(
  here::here(), 
  "cdp/MN_CriteriaPollutants_ValidDV_Summary.csv"))

cprg_geography <- readRDS("_meta/data/cprg_county.RDS") %>% 
  filter(
    NAME %in% met_counties
  ) %>%
  st_union() %>%
  sf::as_Spatial()

pollutants_sf <- criteria_pollutants %>% 
  st_as_sf(coords = c(
    "Longitude1",
    "Latitude1"
  ),
  crs = st_crs(cprg_geography)
  ) 
pollutants_spatial <- pollutants_sf %>%
  sf::as_Spatial()
  
pollutants_metro <- sp::over(pollutants_spatial, cprg_geography)


pollutants_sf <- pollutants_sf %>% 
  st_drop_geometry() %>% 
  mutate(
    in_area = pollutants_metro
  ) %>% 
  filter(
    in_area == 1
  )

calc_avg_pollution <- function(name){
  pollutants_sf %>% 
    filter(
      Pollutant.and.Averaging.Time == name,
      DV.Year == 2022
    ) %>% 
    group_by(Standards.Unit) %>% 
    summarize(
      avg_pollutant = mean(Max..DV)
    )
}

n20_annual <- calc_avg_pollution("Nitrogen dioxide: annual")
pm10_daily <- calc_avg_pollution("PM10: daily")
pm2.5_annual <- calc_avg_pollution("PM2.5: annual background for modeling-  smoke/dust removed")
fine_particles_annual <- calc_avg_pollution("Fine particles: annual")
