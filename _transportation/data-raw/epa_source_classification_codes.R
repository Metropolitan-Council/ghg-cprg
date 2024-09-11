source("R/_load_pkgs.R")

# scc onroad only -----
# onroad only, broad SCC descriptons
scc6_desc <- read_xlsx("_transportation/data-raw/epa/air_emissions_modeling/2022v1/2022v1 onroad comparisons 22-26-32-38 10aug2024.xlsx",
                       col_types = "text",
                       sheet = 4
) %>%
  clean_names() %>%
  select(scc6, scc6_desc) %>%
  unique() %>%
  bind_rows(
    tibble(
      scc6 = "220354",
      scc6_desc = "Compressed natural gas (CNG); Motor homes"
    )
  )

# these are used specifically for onroad SCCs
# in VMT and other supplementary calculations
scc_onroad <- readxl::read_xlsx("_transportation/data-raw/epa/onroad_activity_data_SCC_descriptions.xlsx",
                                col_types = "text"
) %>%
  clean_names() %>%
  mutate(scc6 = stringr::str_sub(scc, 1, 6)) %>%
  left_join(scc6_desc) %>%
  mutate(
    fuel_type_detail = stringr::str_split(
      fuel_type,
      pattern = "-",
      simplify = TRUE
    )[, 2] %>%
      stringr::str_trim(),
    fuel_type_detail = ifelse(fuel_type_detail == "Ethanol (E",
                              "Ethanol (E-85)",
                              fuel_type_detail
    )
  ) %>%
  mutate(scc6_desc = ifelse(is.na(scc6_desc),
                            paste0(
                              fuel_type_detail, "; ",
                              str_to_sentence(vehicle_type)
                            ),
                            scc6_desc
  ))

# scc  NEI complete -----
# these are used in the official NEI

scc6_desc_manual <- read.csv("_transportation/data-raw/epa/nei/scc6_descriptions_all.csv",
                             colClasses = "character"
)

scc_complete_road <- read_csv("_transportation/data-raw/epa/SCCDownload-2024-0812-144242.csv",
                              col_types = "c"
) %>%
  clean_names() %>%
  filter(data_category %in% c("Onroad", "Nonroad")) %>%
  select(
    scc, data_category, map_to, status,
    last_inventory_year, sector,
    starts_with("scc")
  ) %>%
  tidyr::separate_wider_position(
    scc,
    widths = c(
      "mobile_source" = 2,
      "fuel_type" = 2,
      "vehicle_type" = 2,
      "road_type" = 2,
      "process_type" = 2
    ),
    cols_remove = FALSE
  ) %>%
  mutate(
    # various years listed the road type and process type
    # differently, for some reason
    scc_split_1 = stringr::str_split(
      scc_level_four,
      pattern = ":", simplify = TRUE
    )[, 1] %>%
      str_trim(),
    scc_split_2 = stringr::str_split(
      scc_level_four,
      pattern = ":", simplify = TRUE
    )[, 2] %>%
      str_trim(),
    scc_road_type = case_when(
      scc_split_1 %in% c(
        "Rural Interstate",
        "Rural Other Principal Arterial",
        "Rural Minor Arterial", "Rural Major Collector",
        "Rural Minor Collector",
        "Rural Local", "Urban Interstate",
        "Urban Other Freeways and Expressways",
        "Urban Other Principal Arterial",
        "Urban Minor Arterial", "Urban Collector",
        "Urban Local", "Parking Area",
        "All Road Types"
      ) ~
        scc_split_1,
      TRUE ~ scc_split_2
    ),
    scc_process = case_when(
      scc_split_1 %in% c(
        "Rural Interstate",
        "Rural Other Principal Arterial",
        "Rural Minor Arterial", "Rural Major Collector",
        "Rural Minor Collector",
        "Rural Local", "Urban Interstate",
        "Urban Other Freeways and Expressways",
        "Urban Other Principal Arterial",
        "Urban Minor Arterial", "Urban Collector",
        "Urban Local", "Parking Area",
        "All Road Types"
      ) ~ scc_split_2,
      TRUE ~ scc_split_1
    )
  ) %>%
  rowwise() %>%
  mutate(
    fuel_type_detect = case_when(
      scc_level_two %in% c(
        "Highway Vehicles - Liquefied Petroleum Gas (LPG)",
        "Off-highway Vehicle LPG",
        "LPG"
      ) ~ "Liquefied petroleum gas (LPG)",
      scc_level_two %in% c(
        "Highway Vehicles - Compressed Natural Gas (CNG)",
        "Off-highway Vehicle CNG",
        "CNG"
      ) ~ "Compressed natural gas (CNG)",
      scc_level_three == "LPG" ~ "Liquefied petroleum gas (LPG)",
      str_detect(scc_level_three, "Gasoline") | str_detect(scc_level_two, "Gasoline") ~ "Gasoline",
      str_detect(scc_level_three, "Diesel") | str_detect(scc_level_two, "Diesel") ~ "Diesel",
      str_detect(scc_level_two, "Electricity") | str_detect(scc_level_three, "Electricity") ~ "Electric",
      str_detect(scc_level_two, "Ethanol") | str_detect(scc_level_three, "Ethanol") ~ "Ethanol (E-85)"
    )
  ) %>%
  select(-scc_split_1, -scc_split_2) %>%
  mutate(scc6 = stringr::str_sub(scc, 1, 6)) %>%
  mutate(
    scc_new = case_when(
      is.na(map_to) | map_to %in% c(
        "None",
        "NA"
      ) ~ scc,
      TRUE ~ map_to
    ),
    scc6_new = str_sub(scc_new, 1, 6)
  ) %>%
  left_join(
    scc6_desc_manual,
    join_by(scc_level_one, scc_level_two, scc_level_three, fuel_type_detect, scc6_new)
  )


# scc EQUATES


# https://dep.nj.gov/wp-content/uploads/airplanning/app-4-4-2016-2023-nj-modeling-inventory-statewide-5-13-24.xlsx
# listen, this is the best I could find using google.com
if (!file.exists("_transportation/data-raw/epa/air_emissions_modeling/EQUATES/app-4-4-2016-2023-nj-modeling-inventory-statewide-5-13-24.xlsx")) {
  download.file("https://dep.nj.gov/wp-content/uploads/airplanning/app-4-4-2016-2023-nj-modeling-inventory-statewide-5-13-24.xlsx",
                destfile = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/app-4-4-2016-2023-nj-modeling-inventory-statewide-5-13-24.xlsx"
  )
}

scc_equates <- readxl::read_xlsx(
  "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/app-4-4-2016-2023-nj-modeling-inventory-statewide-5-13-24.xlsx",
  sheet = 2,
  col_types = "text"
) %>%
  clean_names() %>%
  select(label, scc, scc_description) %>%
  unique() %>%
  # filter(scc %in% equates_cprg$scc) %>%
  tidyr::separate_wider_delim(
    cols = scc_description,
    delim = ";",
    names = c(
      "scc_level_one",
      "scc_level_two",
      "scc_level_three",
      "scc_level_four"
    ),
    too_many = "merge",
    cols_remove = FALSE
  ) %>%
  tidyr::separate_wider_delim(
    cols = scc_level_four,
    delim = ":",
    names = c(
      "scc_level_five",
      "scc_level_six"
    ),
    too_few = "align_start",
    too_many = "merge",
    cols_remove = FALSE
  ) %>%
  mutate(across(starts_with("scc"), str_trim)) %>%
  mutate(
    fuel_type = stringr::str_split(
      scc_level_two,
      pattern = "-",
      simplify = TRUE
    )[, 2] %>%
      stringr::str_trim(),
    fuel_type = ifelse(fuel_type == "Ethanol (E",
                       "Ethanol (E-85)",
                       fuel_type
    )
  ) %>%
  select(
    label, scc, scc_description,
    scc_level_one,
    scc_level_two, scc_level_three,
    scc_level_four,
    scc_level_five, scc_level_six,
    fuel_type
  ) %>%
  unique() %>%
  mutate(scc6 = stringr::str_sub(scc, 1, 6)) %>%
  left_join(scc6_desc,
            by = "scc6"
  ) %>%
  mutate(
    alt_vehicle_type =
      case_when(
        scc_level_three %in% c(
          "Single Unit Short-haul Trucks",
          "Single Unit Long-haul Trucks",
          "Refuse Trucks",
          "Combination Short-haul Trucks",
          "Combination Long-haul Trucks"
        ) ~ "Commercial trucks",
        scc_level_three %in% c(
          "Transit Buses",
          "Intercity Buses",
          "School Buses"
        ) ~ "Buses",
        scc_level_three %in% c("Motorcycles", "Motor Homes") ~ "Other",
        TRUE ~ scc_level_three
      )
  )



# scc combine -----
# create a combined SCC index for use across all data sources
# including NEI, EQUATES, and air emissions modeling platforms
scc_combine <- scc_complete_road %>% 
  select(scc6, scc6_desc, scc6_desc_broad, scc6_desc_manual) %>% 
  unique() %>% 
  bind_rows(scc_equates %>% 
              select(scc6, scc6_desc) %>% 
              unique()) %>% 
  bind_rows(scc6_desc_manual %>%
              filter(scc6_new %in% c("220200", "220932")) %>%
              select(scc6 = scc6_new, scc6_desc, scc6_desc_manual, alt_mode,
                     alt_mode_truck)) %>%
  unique() %>% 
  mutate(scc6_desc = ifelse(is.na(scc6_desc), scc6_desc_manual, scc6_desc)) %>% 
  filter(!is.na(scc6_desc)) %>% 
  mutate(scc6_desc = 
           case_when(
             scc6 %in% c("220100",
                         "223007",
                         "220107") ~ "Diesel; Trucks and buses",
             TRUE ~ scc6_desc
           )) %>% 
  mutate(
    fuel_type = stringr::str_split(
      scc6_desc,
      pattern = ";",
      simplify = TRUE
    )[, 1] %>% 
      str_trim(),
    vehicle_type = stringr::str_split(
      scc6_desc,
      pattern = ";",
      simplify = TRUE
    )[, 2] %>% 
      str_trim()
  ) %>% 
  select(scc6, scc6_desc, fuel_type, vehicle_type) %>% 
  unique() %>% 
  left_join(
    scc_complete_road %>%
      select(scc6_desc,
             alt_mode, alt_mode_truck) %>%
      unique()
  ) %>% 
  filter(str_detect(vehicle_type, "equipment", negate = TRUE)) %>% 
  mutate(alt_mode  = ifelse(vehicle_type %in% c("Gas stations",
                                                "Pleasure craft",
                                                "Trucks and buses"), vehicle_type,
                            alt_mode),
         
         alt_mode_truck  = ifelse(vehicle_type %in% c("Gas stations",
                                                      "Pleasure craft",
                                                      "Trucks and buses"), vehicle_type,
                                  alt_mode_truck)
  )
