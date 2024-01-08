source("R/_load_pkgs.R")
library(streetlightR)

# fetch login email
login_email <- keyring::key_get("StL_email")

# set key for session
streetlightR::streetlight_api_key(key = keyring::key_get("StreetLightAPI"))


# fetch results -----

county21 <- readRDS("_transportation/data-raw/analysis_runs/county21.RDS")
ctu21 <- readRDS("_transportation/data-raw/analysis_runs/ctu21.RDS")

## county -----
county_status <- check_analysis_status(county21$name) %>%
  httr2::resp_body_json()


county21_data <- purrr::map(
  unlist(county_status$analyses[[1]]$metrics),
  function(x) {
    Sys.sleep(5)
    streetlightR::get_analysis_data(
      analysis_name = county21$name,
      metric = eval(x)
    ) %>%
      mutate(
        analysis_name = county21$name,
        metric_group = x
      ) %>%
      select(
        analysis_name, metric_group,
        everything()
      )
  }
)

names(county21_data) <- unlist(county_status$analyses[[1]]$metrics)

saveRDS(
  county21_data,
  "_transportation/data-raw/analysis_runs/county21_data.RDS"
)


## CTU -----
ctu_status <- check_analysis_status(ctu21$name) %>%
  httr2::resp_body_json()


ctu21_data <- purrr::map(
  unlist(ctu_status$analyses[[1]]$metrics),
  function(x) {
    Sys.sleep(5)
    streetlightR::get_analysis_data(
      analysis_name = ctu21$name,
      metric = eval(x)
    ) %>%
      mutate(
        analysis_name = ctu21$name,
        metric_group = x
      ) %>%
      select(
        analysis_name, metric_group,
        everything()
      )
  }
)


names(ctu21_data) <- unlist(ctu_status$analyses[[1]]$metrics)

saveRDS(
  ctu21_data,
  "_transportation/data-raw/analysis_runs/ctu21_data.RDS"
)

## truck ----

county21_truck <- readRDS("_transportation/data-raw/analysis_runs/county21_truck.RDS")


county21_truck_status <- check_analysis_status(county21_truck$name) %>%
  httr2::resp_body_json()


county21_truck_data <- purrr::map(
  unlist(county21_truck_status$analyses[[1]]$metrics),
  function(x) {
    Sys.sleep(5)
    streetlightR::get_analysis_data(
      analysis_name = county21_truck$name,
      metric = eval(x)
    ) %>%
      mutate(
        analysis_name = county21_truck$name,
        metric_group = x
      ) %>%
      select(
        analysis_name, metric_group,
        everything()
      )
  }
)


names(county21_truck_data) <- unlist(county21_truck_status$analyses[[1]]$metrics)

saveRDS(
  county21_truck_data,
  "_transportation/data-raw/analysis_runs/county21_truck_data.rds"
)



## truck calibrated ----

county21_truck_calib <- readRDS("_transportation/data-raw/analysis_runs/county21_truck_calib.RDS")


county21_truck_calib_status <- check_analysis_status(county21_truck_calib$name) %>%
  httr2::resp_body_json()


county21_truck_calib_data <- purrr::map(
  unlist(county21_truck_calib_status$analyses[[1]]$metrics),
  function(x) {
    Sys.sleep(5)
    streetlightR::get_analysis_data(
      analysis_name = county21_truck_calib$name,
      metric = eval(x)
    ) %>%
      mutate(
        analysis_name = county21_truck_calib$name,
        metric_group = x
      ) %>%
      select(
        analysis_name, metric_group,
        everything()
      )
  }
)


names(county21_truck_calib_data) <- unlist(county21_truck_calib_status$analyses[[1]]$metrics)

saveRDS(
  county21_truck_calib_data,
  "_transportation/data-raw/analysis_runs/county21_truck_calib_data.rds"
)

## truck single-factor calibrated ----

county21_truck_sfcalib <- "CPRG_County_OD_NP_Truck_SFCalib_2021"


county21_truck_calib_status <- check_analysis_status(county21_truck_sfcalib) %>%
  httr2::resp_body_json()

county21_truck_calib_status


county21_truck_calib_data <- purrr::map(
  unlist(county21_truck_calib_status$analyses[[1]]$metrics),
  function(x) {
    Sys.sleep(5)
    streetlightR::get_analysis_data(
      analysis_name = county21_truck_calib$name,
      metric = eval(x)
    ) %>%
      mutate(
        analysis_name = county21_truck_calib$name,
        metric_group = x
      ) %>%
      select(
        analysis_name, metric_group,
        everything()
      )
  }
)


names(county21_truck_calib_data) <- unlist(county21_truck_calib_status$analyses[[1]]$metrics)

saveRDS(
  county21_truck_calib_data,
  "_transportation/data-raw/analysis_runs/county21_truck_sfcalib_data.rds"
)
