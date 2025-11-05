source("R/_load_pkgs.R")
source("R/cprg_colors.R")

remotes::install_github("Metropolitan-Council/ghg.ccap@ccap-graphics")



# New values as of Oct 31, 2025 -------------------------------------------
current_wetlands_9co <- lc_county %>%
  filter(inventory_year == 2022) %>%
  # filter for the 9 Minnesota counties only
  filter(!(geog_name %in% c("St. Croix County", "Pierce County"))) %>%
  filter(land_cover_type == "Wetland") %>%
  summarize(actual_wetland_area_km2 = sum(area))

current_wetlands_wi <- lc_county %>%
  filter(inventory_year == 2022) %>%
  # filter for the 9 Minnesota counties only
  filter(geog_name %in% c("St. Croix County", "Pierce County")) %>%
  filter(land_cover_type == "Wetland") %>%
  summarize(actual_wetland_area_km2 = sum(area))




# Define the input path
inpath_wetlands_high_priority <- 
  paste0(here::here(), "/_nature/data-raw/restorable_wetlands_gdb/RestorableWetlands_CCAP.gdb")

wetlands_high_priority <- sf::st_read(inpath_wetlands_high_priority, layer = "RestorableWetlands_CCAP") %>%
  sf::st_transform(., crs_use)

wetlands_high_priority <- wetlands_high_priority %>% sf::st_make_valid()

wetlands_tibble <- wetlands_high_priority %>%
  sf::st_drop_geometry() %>%
  as_tibble() 


restorable_wetlands_9co <- wetlands_tibble %>%
  mutate(
    # convert acres to km2 
    area_km2 = ACRES * 0.00404686
  ) %>% 
  summarize(
    restorable_wetland_area_km2 = sum(area_km2, na.rm = TRUE)
  )





current_wetlands_9co
current_wetlands_wi
restorable_wetlands_9co


pct_increase_in_wetland_area <- 
  (restorable_wetlands_9co$restorable_wetland_area_km2 / current_wetlands_9co$actual_wetland_area_km2) * 100 
## 34% increase in wetland area for the 9-county region, what is that overall for the 11 county region?

pct_increase_in_wetland_area_11co <- 
  (restorable_wetlands_9co$restorable_wetland_area_km2 / 
     (current_wetlands_9co$actual_wetland_area_km2 + current_wetlands_wi$actual_wetland_area_km2)) * 100

saveRDS(
  pct_increase_in_wetland_area_11co,
  "_meta/data-raw/projections/pct_increase_in_wetland_area_11co.RDS"
)



