source("R/_load_pkgs.R")
# takes traffic ratios used for calibrating StreetLight and finds the average
# proportional breakdown for passenger, medium, and heavy duty traffic.
#
## read in traffic ratios
# traffic_ratios <- readRDS(paste0("_transportation/data-raw/mndot/most_recent_yearly_volume_percentage_by_class.RDS"))


# from correspondence with MnDOT about urban/rural default values
# used to estimate HCAADT on local roads, county roads, and CSAHs.
traffic_ratios <- tibble::tribble(
  ~VEHICLE.TYPE, ~URBAN, ~RURAL,
  "2AX-6TIRE SU", "1.52%", "3.17%",
  "3AX+ SU", "0.46%", "1.64%",
  "3AX TST", "0.09%", "0.28%",
  "4AX TST", "0.12%", "0.50%",
  "5AX+ TST", "0.89%", "3.26%",
  "TR TR, BUSES", "0.47%", "1.20%",
  "TWIN TRAILERS", "0.02%", "0.02%"
) %>%
  clean_names() %>%
  mutate(vehicle_type_cdp = case_when(
    vehicle_type %in% c("2AX-6TIRE SU",
                        "3AX+ SU") ~ "Medium Goods Vehicles",
    TRUE ~ "Heavy Goods Vehicles"
  )) %>%
  group_by(vehicle_type_cdp) %>%
  summarize(urban = urban %>%
    stringr::str_remove("%") %>%
    as.numeric() %>%
    sum() / 100) %>%
  ungroup()


cdp_freight <- traffic_ratios %>%
  pivot_wider(
    names_from = vehicle_type_cdp,
    values_from = urban
  ) %>%
  # summarize(
  #   `Medium Goods Vehicles` = mean(medium_duty),
  #   `Heavy Goods Vehicles` = mean(heavy_duty)
  # ) %>%
  summarize(
    `Medium Goods Freight` = `Medium Goods Vehicles` / (`Medium Goods Vehicles` + `Heavy Goods Vehicles`),
    `Heavy Goods Freight` = `Heavy Goods Vehicles` / (`Medium Goods Vehicles` + `Heavy Goods Vehicles`)
  )

cdp_freight %>% kable(format = "markdown")
