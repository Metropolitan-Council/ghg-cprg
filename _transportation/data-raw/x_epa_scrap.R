# EPA NEI scraps
#

# scc/smoke/MOVES tables -----
# https://www.cmascenter.org/smoke/documentation/3.6/html/ch02s08s04.html

## fuel types
fuel_types <- tibble::tribble(
  ~MOVES.Fuel.Type, ~Description,
  "01", "Gasoline",
  "02", "Diesel",
  "03", "Compressed Natural Gas (CNG)",
  "04", "Liquefied Petroleum Gas (LPG)",
  "05", "Ethanol (E-85)",
  "09", "Electricity"
) %>%
  clean_names()

fuel_types_agg <- tibble::tribble(
  ~SCC, ~Example.of.aggregated.Fuel.Type.Description,         ~MOVES.Fuel.Type,
  "x1",                       "All non-diesel fuels",     "01; 03; 04; 05; 09",
  "x2",            "All gasoline and ethanol blends",                 "01; 05",
  "x3",                           "All fossil fuels",     "01; 02; 03; 04; 05",
  "00",                                  "All fuels", "01; 02; 03; 04; 05; 09"
) %>%
  clean_names() %>%
  select(
    fuel_type_agg = scc,
    fuel_type_agg_desc = 2,
    3
  )

## vehicle types
vehicle_types <- tibble::tribble(
  ~MOVES.Vehicle.Type, ~Description,
  11L, "Motorcycle",
  21L, "Passenger Car",
  31L, "Passenger Truck",
  32L, "Light Commercial Truck",
  41L, "Intercity Bus",
  42L, "Transit Bus",
  43L, "School Bus",
  51L, "Refuse Truck",
  52L, "Single Unit Short-haul Truck",
  53L, "Single Unit Long-haul Truck",
  54L, "Motor Home",
  61L, "Combination Short-haul Truck",
  62L, "Combination Long-haul Truck"
) %>%
  clean_names()


vehicle_types_agg <- tibble::tribble(
  ~SCC, ~Example.of.aggregated.Vehicle.Type.Description,                                  ~MOVES.Vehicle.Type,
  "30",                             "Light Duty Trucks",                                             "31; 32",
  "40",                                         "Buses",                                         "41; 42; 43",
  "70",               "All Heavy Duty Trucks and Buses",                 "41; 42; 43; 51; 52; 53; 54; 61; 62",
  "71",                         "All Heavy Duty Trucks",                             "51; 52; 53; 54; 61; 62",
  "72",                        "All Combination Trucks",                                             "61; 62",
  "80",                          "All Trucks and Buses",         "31; 32; 41; 42; 43; 51; 52; 53; 54; 61; 62",
  "81",                       "All Trucks except Buses",                     "31; 32; 51; 52; 53; 54; 61; 62",
  "00",                                  "All Vehicles", "11; 21; 31; 32; 41; 42; 43; 51; 52; 53; 54; 61; 62"
) %>%
  clean_names() %>%
  select(
    vehicle_type_agg = scc,
    vehicle_type_agg_desc = 2,
    3
  )


# road types

road_types <- tibble::tribble(
  ~MOVES.Road.Type, ~Road.Type.Description,
  "01", "Off-Network",
  "02", "Rural Restricted Access",
  "03", "Rural Unrestricted Access",
  "04", "Urban Restricted Access",
  "05", "Urban Unrestricted Access",
  "06", "Rural Restricted without Ramps",
  "07", "Urban Restricted without Ramps",
  "08", "Rural Restricted only Ramps",
  "09", "Urban Restricted only Ramps"
) %>%
  clean_names()

road_types_agg <- tibble::tribble(
  ~SCC, ~Example.of.aggregated.Road.Type.Description, ~MOVES.Road.Type,
  "70", "Freeway", "02; 04",
  "71", "freeway except ramps", "06; 07",
  "72", "Ramps", "08; 09",
  "80", "Non-Freeway", "03; 05",
  "90", "All On-network", "02; 03; 04; 05",
  "00", "All on and off-network", "01; 02; 03; 04; 05"
) %>%
  clean_names() %>%
  select(
    road_type_agg = scc,
    road_type_agg_desc = 2,
    3
  )



## process types

process_types <- tibble::tribble(
  ~MOVES.Process, ~Description,
  "01", "Running Exhaust",
  "02", "Start Exhaust",
  "09", "Brakewear",
  "10", "Tirewear",
  "11", "Evaporative Permeation",
  "12", "Evaporative Fuel Vapor Venting",
  "13", "Evaporative Fuel Leaks",
  "15", "Crankcase Running Exhaust",
  "16", "Crankcase Start Exhaust",
  "17", "Crankcase Extended Idle Exhaust",
  "18", "Refueling Displacement Vapor Loss",
  "19", "Refueling Spillage Loss",
  "90", "Extended Idle Exhaust",
  "91", "Auxiliary Power Exhaust",
  "99", "Well-to-Pum>Well-to-Pumpp"
) %>%
  clean_names()


process_types_agg <- tibble::tribble(
  ~SCC, ~Example.of.aggregated.Process.Type.Description, ~MOVES.Process.Type,
  "50", "All Exhaust", "01; 02; 15; 16; 17; 90; 91",
  "51", "All Exhaust except Hotelling", "01; 02; 15; 16",
  "52", "All hotelling exhaust", "17; 90; 91",
  "53", "All Extended Idle Exhaust", "17; 90",
  "60", "All Evaporative and Refueling", "11; 12; 13; 18; 19",
  "61", "All Evaporative except Refueling", "11; 12; 13",
  "61", "All Evaporative except Refueling", "11; 12; 13",
  "62", "All Refueling", "18; 19",
  "63", "All Evaporative except Permeation and Refueling", "12; 13",
  "70", "All Exhaust and Evaporative and Refueling", "01; 02; 11; 12; 13; 15; 16; 17; 18; 19; 90; 91",
  "71", "All Exhaust and Evaporative except Refueling", "01; 02; 11; 12; 13; 15; 16; 17; 90; 91",
  "72", "All Exhaust and Evaporative except Refueling and Hotelling", "01; 02; 11; 12; 13; 15; 16",
  "80", "All Exhaust and Evaporative and Brake and Tire Wear except Refueling", "01; 02; 9; 10; 11; 12; 13; 15; 16; 17; 90; 91",
  "81", "All Exhaust and Evaporative and Brake and Tire Wear except Refueling and Hotelling", "01; 02; 9; 10; 11; 12; 13; 15; 16; 17",
  "00", "All Processes", "01; 02; 9; 10; 11; 12; 13; 15; 16; 17; 18; 19; 90; 91"
) %>%
  clean_names() %>%
  select(
    process_type_agg = scc,
    process_type_agg_desc = 2,
    3
  )



## combine all

scc_moves_smoke <-
  list(
    fuel_types = list(
      fuel_types = fuel_types,
      fuel_types_agg = fuel_types_agg
    ),
    vehicle_types = list(
      vehicle_types = vehicle_types,
      vehicle_types_agg = vehicle_types_agg
    ),
    road_types = list(
      road_types = road_types,
      road_types_agg = road_types_agg
    ),
    process_types = list(
      process_types = process_types,
      process_types_agg = process_types_agg
    )
  )

rm(
  fuel_types, fuel_types_agg,
  vehicle_types, vehicle_types_agg,
  road_types, road_types_agg,
  process_types, process_types_agg
)



# process epa equates
source("R/_load_pkgs.R")
source("R/global_warming_potential.R")
source("R/download_read_table.R")
source("_transportation/data-raw/epa_source_classification_codes.R")
source("_transportation/data-raw/epa_nei_envirofacts.R")

nei_vmt <- readRDS("_transportation/data-raw/epa/nei/nei_vmt.RDS")

equates <- read_rds("_transportation/data-raw/epa/air_emissions_modeling/EQUATES/equates_mn_wi.RDS")
equates_cprg <- read_rds("_transportation/data-raw/epa/air_emissions_modeling/EQUATES/equates_cprg.RDS") %>%
  select(-equates_path) %>%
  left_join(
    scc_equates,
    join_by(scc, scc6)
  )


equates_cprg_summary <- equates_cprg %>%
  group_by(
    geoid, county_name, cprg_area,
    poll, calc_year,
  ) %>%
  summarize(
    emissions_short_tons = sum(emissions_short_tons),
    .groups = "keep"
  ) %>%
  mutate(ann_value_grams = emissions_short_tons %>%
           units::as_units("short_ton") %>%
           units::set_units("gram") %>%
           as.numeric()) %>%
  select(-emissions_short_tons) %>%
  pivot_wider(
    names_from = poll,
    values_from = ann_value_grams
  ) %>%
  clean_names() %>%
  rowwise() %>%
  mutate(
    co2_co2_equivalent =
      sum(co2, (ch4 * gwp$ch4), na.rm = T),
    emissions_metric_tons_co2e = co2_co2_equivalent / 1000000
  )


epa_nei %>%
  filter(vehicle_group == "On-Road") %>%
  rowwise() %>%
  mutate(
    co2_co2_equivalent =
      sum(total_co2, (total_ch4 * gwp$ch4), na.rm = T),
    emissions_metric_tons_co2e = co2_co2_equivalent / 1000000
  ) %>%
  ungroup() %>%
  group_by(geoid, county_name, nei_inventory_year) %>%
  summarize(
    total_ch4 = sum(total_ch4),
    total_co2 = sum(total_co2),
    total_n2o = sum(total_n2o),
    total_co2_w_equiv = sum(total_co2_w_equiv),
    emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e)
  ) %>%
  filter(county_name == "Hennepin") %>%
  View()

# visualize over time -----

equates_cprg %>%
  filter(poll == "CO2") %>%
  filter(
    fuel_type != "Compressed Natural Gas (CNG)",
    scc_level_six != "Brake and Tire Wear"
  ) %>%
  ggplot() +
  aes(
    x = calc_year,
    y = ann_value,
    color = alt_vehicle_type
  ) +
  geom_jitter(
    alpha = 0.5
  )

equates_cprg %>%
  group_by(
    geoid, county_name, calc_year, poll, fuel_type,
    scc_level_three
  ) %>%
  summarize(ann_value = sum(ann_value)) %>%
  filter(geoid == "27053") %>%
  View()
