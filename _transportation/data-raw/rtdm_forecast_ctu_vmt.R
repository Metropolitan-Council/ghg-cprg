# using outputs from our regional travel demand model,
# create a VMT forecast for each CTU and county in the region.
# The core work for this data exists in another repository.
# Contact us for more detail.
#
source("R/_load_pkgs.R")
ctu_population_meta <- read_rds("_meta/data/ctu_population_meta.RDS")
cprg_county_meta <- read_rds("_meta/data/cprg_county_meta.RDS")

# annualization factor castigliegoCarbonFreeBoston2019
annualization_factor <- 340

ctu_vmt_forecast <- readRDS("_transportation/data-raw/metc_travel_model/ctu_vmt_forecast.RDS") %>%
  mutate(vmt_year = ifelse(vmt_year == 2025, 2023, as.numeric(vmt_year))) %>%
  mutate(
    annualization_factor = annualization_factor,
    network_vmt_annual = network_vmt * annualization_factor,
    network_truck_annual = network_truck_vmt * annualization_factor,
    network_passenger_annual = network_passenger_vmt * annualization_factor
  )

county_vmt_forecast <- readRDS("_transportation/data-raw/metc_travel_model/county_vmt_forecast.RDS") %>%
  mutate(vmt_year = ifelse(vmt_year == 2025, 2023, as.numeric(vmt_year))) %>%
  mutate(
    annualization_factor = annualization_factor,
    network_vmt_annual = network_vmt * annualization_factor,
    network_truck_annual = network_truck_vmt * annualization_factor,
    network_passenger_annual = network_passenger_vmt * annualization_factor
  )


# create metadata tables
ctu_forecast_meta <- ctu_population_meta %>%
  filter(Column %in% names(ctu_vmt_forecast)) %>%
  bind_rows(
    tribble(
      ~Column, ~Class, ~Description,
      "ctu_name_full", class(ctu_vmt_forecast$ctu_name_full), "CTU name and class",
      "vmt_year", class(ctu_vmt_forecast$vmt_year), "VMT forecast year",
      "network_passenger_vmt", class(ctu_vmt_forecast$network_passenger_vmt), "Passenger vehicle-miles traveled, daily",
      "network_truck_vmt", class(ctu_vmt_forecast$network_truck_vmt), "Truck vehicle-miles traveled, daily",
      "network_vmt", class(ctu_vmt_forecast$network_vmt), "Total vehicle-miles traveled, daily",
      "annualization_factor", class(ctu_vmt_forecast$annualization_factor), "Factor used to expand daily VMT to annual VMT. Source: Carbon Free Boston Study, 2019",
      "network_vmt_annual", class(ctu_vmt_forecast$network_vmt_annual), "Total vehicle-miles traveled, annual",
      "network_passenger_annual", class(ctu_vmt_forecast$network_passenger_annual), "Passenger vehicle-miles traveled, annual",
      "network_truck_annual", class(ctu_vmt_forecast$network_truck_annual), "Truck vehicle-miles traveled, annual",
    )
  ) %>%
  arrange(match(Column, names(ctu_vmt_forecast)))


county_vmt_forecast_meta <- cprg_county_meta %>%
  filter(Column %in% names(county_vmt_forecast)) %>%
  bind_rows(
    ctu_forecast_meta %>%
      filter(Column %in% names(county_vmt_forecast))
  ) %>%
  bind_rows(
    tribble(
      ~Column, ~Class, ~Description,
      "county_id", class(county_vmt_forecast$county_id), "Three digit county identifier",
    )
  ) %>%
  arrange(match(Column, names(county_vmt_forecast)))



saveRDS(ctu_vmt_forecast, "_transportation/data/rtdm_forecast_ctu.RDS")
saveRDS(ctu_forecast_meta, "_transportation/data/rtdm_forecast_ctu_meta.RDS")

saveRDS(county_vmt_forecast, "_transportation/data/rtdm_forecast_county.RDS")
saveRDS(county_vmt_forecast_meta, "_transportation/data/rtdm_forecast_county_meta.RDS")
