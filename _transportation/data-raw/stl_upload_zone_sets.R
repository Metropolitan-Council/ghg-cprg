# run once
# Upload county and city geographies to StreetLight, using their API
source("R/_load_pkgs.R")
library(streetlightR)

# fetch login email
login_email <- keyring::key_get("StL_email")

# set key for session
streetlightR::streetlight_api_key(key = keyring::key_get("StreetLightAPI"))


# counties -----
cprg_counties <- readRDS("R/data/cprg_county.RDS")

# create zone name, passthrough = TRUE
stl_counties <- cprg_counties %>%
  mutate(
    name = NAME,
    is_pass = 0
  ) %>%
  select(name, is_pass)

# upload counties
upload_zone_set(
  login_email = login_email,
  geom_type = "polygon",
  zones = stl_counties,
  zone_set_name = "CPRG_Counties_NP",
)


# ctus -----
cprg_ctu <- readRDS("R/data/cprg_ctu.RDS")

# create zone name, passthrough = TRUE
stl_ctu <- cprg_ctu %>%
  mutate(
    name = paste0(CTU_NAME, "_", CTU_CLASS, "_", COUNTY_NAM),
    is_pass = 0
  ) %>%
  select(name, is_pass)

# double check that zone names are unique
nrow(unique(stl_ctu)) == nrow(cprg_ctu)

# upload
upload_zone_set(
  login_email = login_email,
  geom_type = "polygon",
  zones = stl_ctu,
  zone_set_name = "CPRG_CTUs_NP",
)


# freight calibration zone set -----

mn_stations_lines <- readRDS("_transportation/data-raw/mndot/mn_stations_ratios.RDS") %>%
  mutate(
    name = paste0("MN_", sequence_n, "_", continuous_number),
    year = as.numeric(year)
  ) %>%
  select(name, year, passenger, medium_duty, heavy_duty, current_volume, geometry = geom)

wi_stations_lines <- readRDS("_transportation/data-raw/wisdot/wi_stations_ratios.RDS") %>%
  mutate(name = paste0("WI_", site_id)) %>%
  select(name, year, passenger, medium_duty, heavy_duty, current_volume, geometry)

freight_calibration_lines <- bind_rows(
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

saveRDS(freight_calibration_lines, "_transportation/data-raw/freight_calibration_lines.RDS")

# one last thing we need is to calculate the angle or  direction
# https://stackoverflow.com/questions/72276233/calculate-the-angle-between-two-sf-points-in-r-to-calculate-the-direction-of-roa

# take a radian numeric value and return a degree, modulated to be 0 to 359
# StL requires direction to be a numeric from 0 to 359
to_degrees <- function(rad) {
  ((rad * 180) / (pi)) %% 359
}

freight_calibration_line_upload <- freight_calibration_lines %>%
  # transform into sfnetwork object
  sfnetworks::as_sfnetwork() %>%
  # activate edges
  sfnetworks::activate("edges") %>%
  # calculate direction
  mutate(direction = sfnetworks::edge_azimuth() %>%
    as.numeric() %>%
    to_degrees()) %>%
  st_as_sf() %>%
  select(name, is_pass, direction, is_bidi,
    calibration_type,
    calibration_value = current_volume,
    personal_traffic_ratio, medium_commercial_ratio,
    heavy_commercial_ratio
  )


# upload zone set
upload_zone_set(
  login_email = login_email,
  geom_type = "line",
  zones = freight_calibration_line_upload,
  zone_set_name = "CPRG_Freight_Calibration_GateEdit",
  with_calibration = TRUE
)

# once this was uploaded, I manually edited the auto-generated gates
# so they cover OpenStreetMaps line segments in both directions
