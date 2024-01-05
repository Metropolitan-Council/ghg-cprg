# create StL analyses ------
source("R/_load_pkgs.R")
library(streetlightR)

# fetch login email
login_email <- keyring::key_get("StL_email")

# set key for session
streetlightR::streetlight_api_key(key = keyring::key_get("StreetLightAPI"))


county21 <- create_streetlight_analysis(
  login_email = login_email,
  analysis_name = "CPRG_County_OD_NP_2021",
  analysis_type = "OD_Analysis",
  origin_zone_set = "CPRG_Counties_NP",
  destination_zone_set = "CPRG_Counties_NP",
  travel_mode_type = "All_Vehicles",
  output_type = "volume",
  trip_attributes = TRUE,
  date_ranges = list(
    start_date = "01/01/2021",
    end_date = "12/31/2021"
  ),
  tags = c(
    "streetlightR",
    "CPRG"
  )
)

# save analysis identifying information
saveRDS(county21, "_transportation/data-raw/analysis_runs/county21.RDS")


ctu21 <- create_streetlight_analysis(
  login_email = login_email,
  analysis_name = "CPRG_CTU_OD_NP_2021",
  analysis_type = "OD_Analysis",
  origin_zone_set = "CPRG_CTUs_NP",
  destination_zone_set = "CPRG_CTUs_NP",
  travel_mode_type = "All_Vehicles",
  output_type = "volume",
  trip_attributes = TRUE,
  date_ranges = list(
    start_date = "01/01/2021",
    end_date = "12/31/2021"
  ),
  tags = c(
    "streetlightR",
    "CPRG"
  )
)
# save analysis identifying information
saveRDS(ctu21, "_transportation/data-raw/analysis_runs/ctu21.RDS")

# first submitted with all passthrough zones, and this was not a good configuration
# (CPRG_CTU_OD_2021 and CPRG_County_OD_2021)

# submitted Tuesday 12/26/23, around 2pm


county21_truck <- create_streetlight_analysis(
  login_email = login_email,
  analysis_name = "CPRG_County_OD_NP_Truck_2021",
  analysis_type = "OD_Analysis",
  origin_zone_set = "CPRG_Counties_NP",
  destination_zone_set = "CPRG_Counties_NP",
  travel_mode_type = "Truck",
  output_type = "index",
  trip_attributes = TRUE,
  traveler_attributes = TRUE,
  date_ranges = list(
    start_date = "01/01/2021",
    end_date = "12/31/2021"
  ),
  tags = c(
    "streetlightR",
    "CPRG"
  )
)

# save analysis identifying information
saveRDS(county21_truck, "_transportation/data-raw/analysis_runs/county21_truck.RDS")
