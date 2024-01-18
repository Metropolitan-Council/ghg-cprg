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
  mutate(
    year = 2021,
    vehicle_weight = factor(vehicle_weight, levels = c(
      "Passenger",
      "Medium",
      "Heavy"
    )),
    vehicle_weight_label = case_when(
      vehicle_weight == "Passenger" ~ "Light-duty",
      TRUE ~ paste0(vehicle_weight, "-duty") %>%
        factor(levels = c(
          "Light-duty",
          "Medium-duty",
          "Heavy-duty"
        ))
    )
  ) %>%
  select(
    analysis_name,
    mode_of_travel,
    year,
    vehicle_type,
    vehicle_weight,
    vehicle_weight_label,
    zone,
    everything()
  ) %>%
  # we are not including heavy duty/long-haul trucks in our calculation
  # because their actual trip origin and destinations are likely to be outside
  # the region
  filter(vehicle_weight != "Heavy")


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
    "vehicle_weight_label", class(vehicle_emissions$vehicle_weight_label), "\"Light-duty\", \"Medium-duty\", or \"Heavy-duty\"",
    "zone", class(vehicle_emissions$zone), "County name",
    "vmt_same", class(vehicle_emissions$vmt_same), "Annual vehicle miles traveled for trips beginning and ending in the given county",
    "vmt_origin", class(vehicle_emissions$vmt_origin), "Annual vehicle miles traveled for trips starting in the given county",
    "vmt_destination", class(vehicle_emissions$vmt_destination), "Annual vehcile miles traveled for trips ending in the given county",
    "vmt_total", class(vehicle_emissions$vmt_total), "Total annual vehicle miles traveled for the given county (vmt_same + vmt_origin + vmt_destination)",
    "moves_year", class(vehicle_emissions$moves_year), "EPA MOVES model run year",
    "total_co2", class(vehicle_emissions$total_co2), "Annual total grams of CO~2~  attributed to the given county",
    "total_ch4", class(vehicle_emissions$total_ch4), "Annual total grams of CH~4~  attributed to the given county",
    "total_n2o", class(vehicle_emissions$total_n2o), "Annual total grams of N~2~O  attributed to the given county",
    "total_co2_w_equiv", class(vehicle_emissions$total_co2_w_equiv), "Annual total grams of CO~2~ and CO~2~ equivalent attributed to the given county",
    "emissions_tonnes_co2e", class(vehicle_emissions$emissions_tonnes_co2e), "Annual total metric tons CO~2~ and CO~2~ equivalent attributed to the given county"
  )

saveRDS(vehicle_emissions, "_transportation/data/county_vmt_emissions.RDS")
saveRDS(vehicle_emissions_meta, "_transportation/data/county_vmt_emissions_meta.RDS")
