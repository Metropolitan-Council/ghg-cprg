# wastewater service areas -----
# pulled directly from MN GeoCommons
# https://gisdata.mn.gov/dataset/us-mn-state-metc-util-sanitary-sewersheds

ww_raw <- councilR::import_from_gpkg("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/util_sanitary_sewersheds/gpkg_util_sanitary_sewersheds.zip")
cprg_county <- readRDS(file.path(here::here(), "_meta/data/cprg_county.RDS"))
source(file.path(here::here(), "R/_leaflet_helpers.R"))

ww_service_areas <- ww_raw %>% 
  filter(ServiceSta == "Served",
         !is.na(WWTP)) %>% 
  sf::st_make_valid() %>% 
  group_by(ServiceSta) %>%
  summarize(geom = st_union(geom)) %>% 
  sf::st_make_valid() %>% 
  sf::st_simplify(dTolerance = 2)

saveRDS(ww_service_areas, "_waste/data-raw/wastewater/metc_service_area.RDS")
# 
# council_leaflet() %>% 
#   addPolygons(
#     data = cprg_county %>%
#       sf::st_transform(4326) %>% 
#       st_union(),
#     fill = FALSE,
#     color = "gray",
#     group = "CPRG area",
#     popup = "CPRG county area"
#   ) %>%
#   addPolygons(
#     data = ww_service_areas,
#     group = "MetC wastewater service area",
#     weight = 1,
#     popup =  "MetC wastewater service area") %>% 
#   leaflet::addLayersControl(
#     overlayGroups = c(
#       "CPRG area",
#       "MetC wastewater service area"
#     )
#   )
# 
