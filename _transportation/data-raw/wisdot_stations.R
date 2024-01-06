# Fetch Wisconsin station passenger/medium/heavy ratios and assign
# to appropriate line segment
source("R/_load_pkgs.R")
source("R/_leaflet_helpers.R")
library(osmdata)

wis_stations <- readxl::read_xlsx("_transportation/data-raw/wisdot/class.xlsx",
  sheet = 3
)
wis_station_ratios <- wis_stations %>%
  filter(COUNTY %in% c(
    "St. Croix",
    "Pierce"
  )) %>%
  select(
    TRAF_SITEID, LOCATION, COUNTY, YEAR, MAX_YEAR,
    DURATION,
    COUNT_TYPE, AADT, AADTT, LIGHT_DUTY_VEHICLES,
    SINGLE_UNIT_TRUCKS, COMBO_UNIT_TRUCKS,
    TOTAL_TRUCKS
  ) %>%
  # we only want the most recent data
  # collected on continuous detectors/locations
  filter(
    MAX_YEAR >= 2017,
    DURATION == "Continuous",
    YEAR == MAX_YEAR
  ) %>%
  mutate(
    passenger = LIGHT_DUTY_VEHICLES / 100,
    medium_duty = SINGLE_UNIT_TRUCKS / 100,
    heavy_duty = COMBO_UNIT_TRUCKS / 100
  ) %>%
  select(TRAF_SITEID, LOCATION, YEAR,
    passenger, medium_duty, heavy_duty,
    current_volume = AADT
  ) %>%
  clean_names() %>%
  # make sure the percentages sum to about 1
  mutate(total_pct = sum(passenger, medium_duty, heavy_duty))

# fetch station locations data
wis_station_locations <- sf::read_sf("_transportation/data-raw/wisdot/Traffic_Counts.geojson") %>%
  # only get our chosen sites
  filter(SITE_ID %in% wis_station_ratios$traf_siteid) %>%
  select(SITE_ID, LOCATION_DESCRIPTION, COUNTY, REGION) %>%
  st_transform(4326)


# fetch road data from OpenStreetMaps
# I tried using tigris::roads(), but the polylines were too long
# and overlapped with each other.
# StL does best with shorter segments

# you may need to disconnect from the Council VPN to run opq()
wis_osm <- osmdata::opq(bbox = st_bbox(wis_station_locations)) %>%
  add_osm_feature(key = "highway") %>%
  osmdata::osmdata_sf()

wis_roads <- wis_osm$osm_lines %>%
  filter(!highway %in% c("cycleway"))

wis_roads_nearest <- wis_roads[sf::st_nearest_feature(
  wis_station_locations,
  wis_roads
), ] %>%
  select(osm_id, name, ref, highway)

# double check visually
leaflet() %>%
  leaflet::addMapPane(name = "Carto Positron", zIndex = 430) %>%
  leaflet::addProviderTiles("CartoDB.PositronOnlyLabels",
    options = leaflet::leafletOptions(pane = "Carto Positron"),
    group = "Carto Positron"
  ) %>%
  leaflet::addProviderTiles("CartoDB.PositronNoLabels",
    group = "Carto Positron"
  ) %>%
  leaflet::addCircleMarkers(
    data = wis_station_locations
  ) %>%
  leaflet::addPolylines(
    data = wis_roads_nearest
  )

wi_station_lines <- wis_station_locations %>%
  mutate(osm_id = wis_roads_nearest$osm_id) %>%
  sf::st_drop_geometry() %>%
  left_join(wis_roads_nearest, by = "osm_id") %>%
  st_as_sf()

wi_stations_ratios_aadt <- wi_station_lines %>%
  left_join(wis_station_ratios,
    by = c("SITE_ID" = "traf_siteid")
  ) %>%
  select(
    site_id = SITE_ID, year,
    passenger, medium_duty, heavy_duty,
    current_volume
  )

# export
saveRDS(wi_stations_ratios_aadt, paste0("_transportation/data-raw/wisdot/wi_stations_ratios.RDS"))

# save metadata
saveRDS(wis_osm$meta, "_transportation/data-raw/wisdot/wi_osm_meta.RDS")
