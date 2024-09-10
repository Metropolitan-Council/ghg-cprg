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
