# [explain]
source("R/_load_pkgs.R")
met_counties <- c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington")

criteria_pollutants <- read.csv(file.path(
  here::here(), 
  "cdp/MN_CriteriaPollutants_ValidDV_Summary.csv"))

cprg_geography <- readRDS("_meta/data/cprg_county.RDS") %>% 
  filter(
    county_name %in% met_counties
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

no2_annual <- calc_avg_pollution("Nitrogen dioxide: annual")
# convert from ppb to ug_m3, using avg temp of 15 deg C
# equation from https://www.apis.ac.uk/unit-conversion?ppb_ug=ppb&value=7.4&pollutant=NO2&m_weight=&select_temp=0&input_temp=15&Submit=Calculate+#jump
temp <- 8.3
# ignoring the conversion for barometric pressure because mpls hovers around sea level average
no2_ug_m3 <- no2_annual$avg_pollutant * 46.01 * (273/(temp+273)*(1/22.41))
pm10_daily <- calc_avg_pollution("PM10: daily")
pm2.5_annual <- calc_avg_pollution("PM2.5: annual background for modeling-  smoke/dust removed")
fine_particles_annual <- calc_avg_pollution("Fine particles: annual")
