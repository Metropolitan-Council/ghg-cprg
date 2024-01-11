source("R/_load_pkgs.R")
source("_transportation/data-raw/_calculate_vmt.R")
source("_transportation/data-raw/_calculate_emissions.R")

county21_data <- readRDS("_transportation/data-raw/analysis_runs/county21_data.RDS")
county21_truck <- readRDS("_transportation/data-raw/analysis_runs/county21_truck_calib_data.rds")
epa_moves <- readRDS("_transportation/data/epa_moves.RDS")

vehicle_miles <- bind_rows(
  calculate_vmt(county21_data, class = "passenger"),
  calculate_vmt(county21_truck, class = "commercial")
)

vehicle_emissions <- vehicle_miles %>%
  calculate_emissions(emissions_factors = epa_moves) %>%
  mutate(year = 2021) %>%
  select(
    analysis_name,
    mode_of_travel,
    year,
    vehicle_type,
    vehicle_weight,
    zone,
    everything()
  )


vehicle_emissions %>%
  group_by(zone) %>%
  summarize(total_emis = sum(emissions_tonnes_co2e))

names(vehicle_emissions)
vehicle_emissions_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "analysis_name", class(vehicle_emissions$analysis_name), "StreetLight analysis identifier",
    "mode_of_travel", class(vehicle_emissions$mode_of_travel), "StreetLight travel mode and calculation method",
    "year", class(vehicle_emissions$year), "Emissions estimation year",
    "vehicle_type", class(vehicle_emissions$vehicle_type), "\"passenger\" or \"commercial\"",
    "vehicle_weight", class(vehicle_emissions$vehicle_weight), "\"Passenger\", \"Medium\", or \"Heavy\"",
    "zone", class(vehicle_emissions$zone), "County name",
    "vmt_same", class(vehicle_emissions$vmt_same), "Vehicle miles traveled for trips beginning and ending in the given county",
    "vmt_origin", class(vehicle_emissions$vmt_origin), "Vehicle miles traveled for trips starting in the given county",
    "vmt_destination", class(vehicle_emissions$vmt_destination), "Vehcile miles traveled for trips ending in the given county",
    "vmt_total", class(vehicle_emissions$vmt_total), "Total vehicle miles traveled for the given county (vmt_same + vmt_origin + vmt_destination)",
    "moves_year", class(vehicle_emissions$moves_year), "EPA MOVES model run year",
    "total_co2", class(vehicle_emissions$total_co2), "Total grams of CO~2~  attributed to the given county",
    "total_ch4", class(vehicle_emissions$total_ch4), "Total grams of CH~4~  attributed to the given county",
    "total_n2o", class(vehicle_emissions$total_n2o), "Total grams of N~2~O  attributed to the given county",
    "total_co2_w_equiv", class(vehicle_emissions$total_co2_w_equiv), "Total grams of CO~2~ and CO~2~ equivalent attributed to the given county",
    "emissions_tonnes_co2e", class(vehicle_emissions$emissions_tonnes_co2e), "Total metric tons CO~2~ and CO~2~ equivalent attributed to the given county"
  )

saveRDS(vehicle_emissions, "_transportation/data/county_vmt_emissions.RDS")
saveRDS(vehicle_emissions_meta, "_transportation/data/county_vmt_emissions_meta.RDS")
