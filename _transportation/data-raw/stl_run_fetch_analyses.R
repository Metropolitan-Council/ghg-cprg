# run once
# Upload county and city geographies to StreetLight, using their API
source("R/_load_pkgs.R")
library(streetlightR)

# fetch login email
login_email <- keyring::key_get("StL_email")

# set key for session
streetlightR::streetlight_api_key(key = keyring::key_get("StreetLightAPI"))


county21 <- create_streetlight_analysis(
  login_email = login_email,
  analysis_name = "CPRG_County_OD_2021",
  analysis_type = "OD_Analysis",
  origin_zone_set = "CPRG_Counties",
  destination_zone_set = "CPRG_Counties",
  travel_mode_type = "All_Vehicles",
  output_type = "volume",
  trip_attributes = TRUE,
  date_ranges = list(
    start_date = "01/01/2021",
    end_date = "12/31/2021"
  ),
  tags = c("streetlightR",
           "CPRG")
)

county21



ctu21 <- create_streetlight_analysis(
  login_email = login_email,
  analysis_name = "CPRG_CTU_OD_2021",
  analysis_type = "OD_Analysis",
  origin_zone_set = "CPRG_CTUs",
  destination_zone_set = "CPRG_CTUs",
  travel_mode_type = "All_Vehicles",
  output_type = "volume",
  trip_attributes = TRUE,
  date_ranges = list(
    start_date = "01/01/2021",
    end_date = "12/31/2021"
  ),
  tags = c("streetlightR",
           "CPRG")
)

ctu21
