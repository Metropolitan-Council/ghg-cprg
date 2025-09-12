source("R/_load_pkgs.R")
# compile MN and WI commercial point and line datasets for upload to StreetLight
# Used to calibrate StreetLight Index to actual truck counts for medium and heavy
# duty traffic.
#
# lines -----
mn_stations_lines <- readRDS("_transportation/data-raw/mndot/mn_stations_ratios.RDS") %>%
  mutate(
    name = paste0("MN_", sequence_n, "_", continuous_number),
    year = as.numeric(year)
  ) %>%
  select(name, year, passenger, medium_duty, heavy_duty, current_volume, geometry = geom)

wi_stations_lines <- readRDS("_transportation/data-raw/wisdot/wi_stations_ratios.RDS") %>%
  mutate(name = paste0("WI_", site_id)) %>%
  select(name, year, passenger, medium_duty, heavy_duty, current_volume, geometry)

commercial_calibration_lines <- bind_rows(
  mn_stations_lines,
  wi_stations_lines
) %>%
  mutate(
    is_pass = 1,
    is_bidi = 1,
    calibration_type = "AADT",
    personal_traffic_ratio = round(passenger, 2),
    medium_commercial_ratio = round(medium_duty, 2),
    heavy_commercial_ratio = round(heavy_duty, 2)
  ) %>%
  # streetlight requires that all the ratios total up to EXACTLY 1
  # we will start by adjusting the personal traffic ratio +/- 0.01
  # then adjust the medium duty +/- 0.01 until we get a total of 1
  rowwise() %>%
  mutate(total = sum(
    personal_traffic_ratio,
    medium_commercial_ratio,
    heavy_commercial_ratio
  )) %>%
  mutate(personal_traffic_ratio = case_when(
    total == 1 ~ personal_traffic_ratio,
    total > 1 ~ personal_traffic_ratio - 0.01,
    total < 1 ~ personal_traffic_ratio + 0.01
  )) %>%
  mutate(total = sum(
    personal_traffic_ratio,
    medium_commercial_ratio,
    heavy_commercial_ratio
  )) %>%
  mutate(medium_commercial_ratio = case_when(
    total == 1 ~ medium_commercial_ratio,
    total > 1 ~ medium_commercial_ratio - 0.01,
    total < 1 ~ medium_commercial_ratio + 0.01
  )) %>%
  mutate(total = sum(
    personal_traffic_ratio,
    medium_commercial_ratio,
    heavy_commercial_ratio
  ))


# points -----

mn_stations_points <- readRDS("_transportation/data-raw/mndot/mn_stations_ratios_points.RDS") %>%
  mutate(
    name = paste0("MN_", sequence_n, "_", continuous_number),
    year = as.numeric(year)
  ) %>%
  sf::st_transform(4326) %>%
  select(name, year, continuous_number, sequence_number, geometry = geom)


wi_stations_points <- readRDS("_transportation/data-raw/wisdot/wi_stations_ratios_points.RDS") %>%
  mutate(name = paste0("WI_", site_id)) %>%
  st_as_sf()

wi_stations_points$geometry

commercial_calibration_points <- bind_rows(
  mn_stations_points,
  wi_stations_points
) %>%
  select(name, site_id, year, continuous_number, sequence_number) %>%
  left_join(commercial_calibration_lines %>%
    st_drop_geometry() %>%
    select(
      -passenger,
      -medium_duty,
      -heavy_duty,
      -total,
      -is_pass,
      -is_bidi,
      -calibration_type
    ))


commercial_calibration_meta <- tribble(
  ~Column, ~Class, ~Description,
  "name", class(commercial_calibration_points$name), "Unique identifier",
  "site_id", class(commercial_calibration_points$site_id), "WisDOT site identifier",
  "year", class(commercial_calibration_points$year), "AADT year",
  "continuous_number", class(commercial_calibration_points$continuous_number), "MnDOT site identfier",
  "sequence_number", class(commercial_calibration_points$sequence_number), "MnDOT site identifier",
  "geometry", class(commercial_calibration_points$geometry)[[1]], "Simple feature geometry",
  "current_volume", class(commercial_calibration_points$current_volume), "AADT (vehicles)",
  "personal_traffic_ratio", class(commercial_calibration_points$personal_traffic_ratio), "Proportion of vehicles that are personal/light-duty",
  "medium_commercial_ratio", class(commercial_calibration_points$medium_commercial_ratio), "Proportion of vehicles that are medium-duty",
  "heavy_commercial_ratio", class(commercial_calibration_points$heavy_commercial_ratio), "Proportion of vehicles that are heavy-duty"
)

saveRDS(commercial_calibration_points, "_transportation/data/commercial_calibration_points.RDS")
saveRDS(commercial_calibration_lines, "_transportation/data/commercial_calibration_lines.RDS")
saveRDS(commercial_calibration_meta, "_transportation/data/commercial_calibration_meta.RDS")
