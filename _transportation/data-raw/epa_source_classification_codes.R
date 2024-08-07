
# scc/smoke/MOVES tables -----

## fuel types 
fuel_types <- tibble::tribble(
  ~MOVES.Fuel.Type,                    ~Description,
  "01",                      "Gasoline",
  "02",                        "Diesel",
  "03",  "Compressed Natural Gas (CNG)",
  "04", "Liquefied Petroleum Gas (LPG)",
  "05",                "Ethanol (E-85)",
  "09",                   "Electricity"
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
  select(fuel_type_agg = scc,
         fuel_type_agg_desc = 2,
         3)



## vehicle types 
vehicle_types <- tibble::tribble(
  ~MOVES.Vehicle.Type,                   ~Description,
  11L,                   "Motorcycle",
  21L,                "Passenger Car",
  31L,              "Passenger Truck",
  32L,       "Light Commercial Truck",
  41L,                "Intercity Bus",
  42L,                  "Transit Bus",
  43L,                   "School Bus",
  51L,                 "Refuse Truck",
  52L, "Single Unit Short-haul Truck",
  53L,  "Single Unit Long-haul Truck",
  54L,                   "Motor Home",
  61L, "Combination Short-haul Truck",
  62L,  "Combination Long-haul Truck"
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
  select(vehicle_type_agg = scc,
         vehicle_type_agg_desc = 2,
         3)


# road types 

road_types <- tibble::tribble(
  ~MOVES.Road.Type,                     ~Road.Type.Description,
  "01",                    "Off-Network",
  "02",        "Rural Restricted Access",
  "03",      "Rural Unrestricted Access",
  "04",        "Urban Restricted Access",
  "05",      "Urban Unrestricted Access",
  "06", "Rural Restricted without Ramps",
  "07", "Urban Restricted without Ramps",
  "08",    "Rural Restricted only Ramps",
  "09",    "Urban Restricted only Ramps"
) %>% 
  clean_names()

road_types_agg <- tibble::tribble(
  ~SCC, ~Example.of.aggregated.Road.Type.Description, ~MOVES.Road.Type,
  "70",                                    "Freeway",           "02; 04",
  "71",                       "freeway except ramps",           "06; 07",
  "72",                                      "Ramps",           "08; 09",
  "80",                                "Non-Freeway",           "03; 05",
  "90",                             "All On-network",     "02; 03; 04; 05",
  "00",                     "All on and off-network",  "01; 02; 03; 04; 05"
) %>% 
  clean_names() %>% 
  select(road_type_agg = scc,
         road_type_agg_desc = 2,
         3)



## process types 

process_types <- tibble::tribble(
  ~MOVES.Process,                        ~Description,
  "01",                   "Running Exhaust",
  "02",                     "Start Exhaust",
  "09",                         "Brakewear",
  "10",                          "Tirewear",
  "11",            "Evaporative Permeation",
  "12",    "Evaporative Fuel Vapor Venting",
  "13",            "Evaporative Fuel Leaks",
  "15",         "Crankcase Running Exhaust",
  "16",           "Crankcase Start Exhaust",
  "17",   "Crankcase Extended Idle Exhaust",
  "18", "Refueling Displacement Vapor Loss",
  "19",           "Refueling Spillage Loss",
  "90",             "Extended Idle Exhaust",
  "91",           "Auxiliary Power Exhaust",
  "99",         "Well-to-Pum>Well-to-Pumpp"
) %>% 
  clean_names()


process_types_agg <- tibble::tribble(
  ~SCC,                                      ~Example.of.aggregated.Process.Type.Description,                                   ~MOVES.Process.Type,
  "50",                                                                        "All Exhaust",                            "01; 02; 15; 16; 17; 90; 91",
  "51",                                                       "All Exhaust except Hotelling",                                        "01; 02; 15; 16",
  "52",                                                              "All hotelling exhaust",                                          "17; 90; 91",
  "53",                                                          "All Extended Idle Exhaust",                                              "17; 90",
  "60",                                                      "All Evaporative and Refueling",                                  "11; 12; 13; 18; 19",
  "61",                                                   "All Evaporative except Refueling",                                          "11; 12; 13",
  "61",                                                   "All Evaporative except Refueling",                                          "11; 12; 13",
  "62",                                                                      "All Refueling",                                              "18; 19",
  "63",                                    "All Evaporative except Permeation and Refueling",                                              "12; 13",
  "70",                                          "All Exhaust and Evaporative and Refueling",        "01; 02; 11; 12; 13; 15; 16; 17; 18; 19; 90; 91",
  "71",                                       "All Exhaust and Evaporative except Refueling",                "01; 02; 11; 12; 13; 15; 16; 17; 90; 91",
  "72",                         "All Exhaust and Evaporative except Refueling and Hotelling",                            "01; 02; 11; 12; 13; 15; 16",
  "80",               "All Exhaust and Evaporative and Brake and Tire Wear except Refueling",         "01; 02; 9; 10; 11; 12; 13; 15; 16; 17; 90; 91",
  "81", "All Exhaust and Evaporative and Brake and Tire Wear except Refueling and Hotelling",                 "01; 02; 9; 10; 11; 12; 13; 15; 16; 17",
  "00",                                                                      "All Processes", "01; 02; 9; 10; 11; 12; 13; 15; 16; 17; 18; 19; 90; 91"
) %>% 
  clean_names() %>% 
  select(process_type_agg = scc,
         process_type_agg_desc = 2,
         3)



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

rm(fuel_types, fuel_types_agg,
   vehicle_types, vehicle_types_agg,
   road_types, road_types_agg,
   process_types, process_types_agg)



# scc tables by year ----
scc_codes20 <- read_xlsx("_transportation/data-raw/epa/nei/2020NEI/onroad_activity_data_SCC_descriptions.xlsx",
                         sheet = 1,
                         col_types = "text"
) %>%
  clean_names() %>% 
  mutate(calc_year = "2020") %>% 
  mutate(scc_level_one = category,
         scc_level_two = fuel_type,
         scc_level_three = vehicle_type,
         scc_level_four = road_type) %>% 
  select(calc_year, scc, 
         starts_with("scc")) %>% 
  tidyr::separate_wider_position(
    scc, widths = c(
      "mobile_source" = 2,
      "fuel_type" = 2,
      "vehicle_type" = 2,
      "road_type" = 2,
      "process_type" = 2
    ), 
    cols_remove = FALSE
  )



scc_codes14 <- read_xlsx("_transportation/data-raw/epa/nei/2014NEI/2014v1_EICtoEPA_SCCmapping.xlsx",
                         sheet = 3,
                         col_types = "text") %>% 
  clean_names() %>% 
  mutate(calc_year = "2014") %>% 
  select(calc_year, scc, 
         starts_with("scc")) %>% 
  tidyr::separate_wider_position(
    scc, widths = c(
      "mobile_source" = 2,
      "fuel_type" = 2,
      "vehicle_type" = 2,
      "road_type" = 2,
      "process_type" = 2
    ), 
    cols_remove = FALSE
  )


scc_codes11 <- read_xlsx("_transportation/data-raw/epa/nei/2011NEI/MOVES2014_SMOKE_SCCs_with_descriptions.xlsx",
          sheet = 2,
          col_types = "text") %>% 
  clean_names() %>% 
  tidyr::separate_wider_delim(scc_desc, delim = ";",
                              names = c(
                              "scc_level_one",
                              "scc_level_two",
                              "scc_level_three",
                              "scc_level_four"
                              )) %>% 
  mutate(scc_process = stringr::str_split(
    scc_level_four, 
    pattern = ":", simplify = TRUE)[,2]) %>% 
  mutate(calc_year = "2011") %>% 
  tidyr::separate_wider_position(
    scc, widths = c(
      "mobile_source" = 2,
      "fuel_type" = 2,
      "vehicle_type" = 2,
      "road_type" = 2,
      "process_type" = 2
    ), 
    cols_remove = FALSE
  )
  

scc_codes11_alt <- read_xlsx("_transportation/data-raw/epa/nei/2011NEI/MOVES2014_SCC_List_v8.xlsx",
                         sheet = 2,
                         col_types = "text") %>% 
  clean_names() %>% 
  mutate(calc_year = "2011",
         scc_level_one = "Mobile Sources",
         scc_level_two = paste0(sourcetypename, " - ", fueltypedesc),
         scc_level_three = sourcetypename,
         scc_level_four = roaddesc
  ) %>% 
  select(calc_year, scc, 
         starts_with("scc")) %>% 
  tidyr::separate_wider_position(
    scc, widths = c(
      "mobile_source" = 2,
      "fuel_type" = 2,
      "vehicle_type" = 2,
      "road_type" = 2,
      "process_type" = 2
    ), 
    cols_remove = FALSE
  ) %>% 
  unique()


scc_codes11_alt %>% 
  mutate(process_group_id = case_when(
    process_type %in% c("01", "02", "09",
                        "10", "11", "12", "13", "15", "16", 
                        "17", "18", "19", "90", "91"
) ~ "00",
TRUE ~ NA
  ))

scc_codes08 <- read_xlsx("_transportation/data-raw/epa/nei/2008NEI/scc_eissector_xwalk_2008neiv3.xlsx",
                         sheet = 2,
                         col_types = "text") %>% 
  clean_names() %>% 
  mutate(calc_year = "2008",
         scc = code) %>% 
  filter(stringr::str_starts(scc, "22")) %>% 
  select(calc_year, scc, 
         starts_with("scc")) %>% 
  mutate(scc_process = stringr::str_split(
    scc_level_four, 
    pattern = ":", simplify = TRUE)[,2]) %>% 
  tidyr::separate_wider_position(
    scc, widths = c(
      "mobile_source" = 2,
      "fuel_type" = 2,
      "vehicle_type" = 2,
      "road_type" = 2,
      "process_type" = 2
    ), 
    cols_remove = FALSE
  )



scc_complete <- read_csv("_transportation/data-raw/epa/SCCDownload-2024-0805-165321.csv",
                         col_types = "c") %>% 
  clean_names()

scc_mobile <- scc_complete %>% 
  filter(stringr::str_starts(scc, "22"),
         last_inventory_year %in% c(NA,
                                    "2020",
                                    "2017",
                                    "2014",
                                    "2011",
                                    "2008",
                                    "2005")) %>% 
  select(
    scc, data_category, map_to, status,
    last_inventory_year,
    starts_with("scc")
  ) %>% 
  tidyr::separate_wider_position(
    scc, widths = c(
      "mobile_source" = 2,
      "fuel_type" = 2,
      "vehicle_type" = 2,
      "road_type" = 2,
      "process_type" = 2
    ), 
    cols_remove = FALSE
  ) %>% 
  mutate(
    # 2005 listed the road type and process type
    # in a different order than other years
    # for some reason
    scc_split_1 = stringr::str_split(
      scc_level_four, 
      pattern = ":", simplify = TRUE)[,1] %>% 
      str_trim(),
    scc_split_2 = stringr::str_split(
      scc_level_four, 
      pattern = ":", simplify = TRUE)[,2] %>% 
      str_trim(),
    scc_road_type = case_when(
      scc_split_1 %in% c( "Rural Interstate", 
                          "Rural Other Principal Arterial",
                          "Rural Minor Arterial", "Rural Major Collector", 
                          "Rural Minor Collector", 
                          "Rural Local", "Urban Interstate",
                          "Urban Other Freeways and Expressways", 
                          "Urban Other Principal Arterial", 
                          "Urban Minor Arterial", "Urban Collector", 
                          "Urban Local", "Parking Area",
                          "All Road Types") ~ 
                           scc_split_1,
      TRUE ~ scc_split_2),
    scc_process = case_when(
      
      scc_split_1 %in% c( "Rural Interstate", 
                          "Rural Other Principal Arterial",
                          "Rural Minor Arterial", "Rural Major Collector", 
                          "Rural Minor Collector", 
                          "Rural Local", "Urban Interstate",
                          "Urban Other Freeways and Expressways", 
                          "Urban Other Principal Arterial", 
                          "Urban Minor Arterial", "Urban Collector", 
                          "Urban Local", "Parking Area",
                          "All Road Types") ~ scc_split_2,
      TRUE ~ scc_split_1
    )) %>% 
  select(-scc_split_1, -scc_split_2)

# compare scc tables -----

scc_codes08 %>% 
  bind_rows(scc_codes11) %>% 
  select(calc_year, process_type) %>% 
  unique() %>% 
  left_join(scc_moves_smoke$process_types$process_types_agg,
            by = c("process_type" = "scc")) %>% 
  left_join(scc_moves_smoke$process_types$process_types,
            by = c("process_type" = "moves_process")) %>% View


anti_join(
  scc_mobile,
  scc_codes08
)


