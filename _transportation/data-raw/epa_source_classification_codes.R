source("R/_load_pkgs.R")
source("R/download_read_table.R")
# The U.S. EPA uses Source Classification Codes (SCCs) to classify different
#  types of activities that generate
#  emissions. Each SCC represents a unique source category-specific process or
#  function that emits air pollutants.
#  
#  SCCs have changed over time, with the biggest change occurring between 2008
#  and 2011 NEI years. This means it is impossible to compare any NEIs 2008 
#  and prior with 2011 and later. This is remedied by EQUATES.
#  
#  There are special SCCs for modeling mobile sources that are different from
#  those used for the NEI. 
#  
#  See _transportation/data-raw/epa/Re: Seeking guidance on consistent sccs.pdf
#  and the EPA introduction document, Zotero key usepaIntroductionSCCs2021
#  
#  This script reads in SCC dictionaries from various places and combines them 
#  into a single reference dataset (scc_combine.RDS), later used for compiling emissions.
#  
# scc6 -----
#  SCC6 (the first six digits of the SCC) is a shortened version that helps 
#  group together like vehicle and fuel types, regardless of road type or
#  process type.
scc6_desc <- download_read_table("https://gaftp.epa.gov/Air/emismod/2022/v1/reports/mobile/onroad/2022v1%20onroad%20comparisons%2022-26-32-38%2010aug2024.xlsx", 
                                 exdir = "_transportation/data-raw/epa/air_emissions_modeling/2022v1/",
                                 col_types = "text",
                                 sheet = 4) %>% 
  clean_names() %>%
  select(scc6, scc6_desc) %>%
  unique() %>%
  # add CNG motor homes code
  bind_rows(
    tibble(
      scc6 = "220354",
      scc6_desc = "Compressed natural gas (CNG); Motor homes"
    )
  )

# manual scc6 descriptions, compiled by Council staff
scc6_desc_manual <- read.csv("_transportation/data-raw/epa/nei/scc6_descriptions_all.csv",
                             colClasses = "character"
)

# scc onroad modeling -----
# scc_onroad are used specifically for modeling, on-road
scc_onroad <- download_read_table(
  "https://gaftp.epa.gov/Air/emismod/series/onroad_activity/onroad_activity_data_SCC_descriptions.xlsx",
  exdir = "_transportation/data-raw/epa/",
  col_types = "text") %>% 
  clean_names() %>%
  mutate(scc6 = stringr::str_sub(scc, 1, 6)) %>%
  # join with scc6 descriptions
  left_join(scc6_desc) %>%
  mutate(
    # manually split out fuel type details
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
  # if scc6_desc is NA, 
  # then construct it from the fuel and vehicle types
  mutate(scc6_desc = ifelse(is.na(scc6_desc),
                            paste0(
                              fuel_type_detail, "; ",
                              str_to_sentence(vehicle_type)
                            ),
                            scc6_desc
  ))

# scc NEI -----
# these are used in the official NEI
# downloaded directly from EPA SCC website:
# https://sor-scc-api.epa.gov/sccwebservices/sccsearch/
# contains onroad and nonroad, retired and active SCCs
scc_complete_road <- 
  download_read_table("https://sor-scc-api.epa.gov/sccwebservices/v1/SCC?format=CSV&sortFacet=scc%20level%20one&filename=SCCDownload-2024-0812-144242.csv",
                      exdir = "_transportation/data-raw/epa/",
                      col_types = "c") %>% 
  clean_names() %>%
  filter(data_category %in% c("Onroad", "Nonroad")) %>%
  select(
    scc, data_category, map_to, status,
    last_inventory_year, sector,
    starts_with("scc")
  ) %>%
  # break out SCC into its
  # individual components:
  # fuel, vehicle, road, and proces types
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
    # in different orders
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
    # clean up fuel type labels
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
    # for retired SCCs that have a specified active SCC to 
    # map to, replace the original SCC with the map to SCC
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
    # join with manual descriptions
    scc6_desc_manual,
    join_by(scc_level_one, scc_level_two, scc_level_three, fuel_type_detect, scc6_new)
  )


# scc EQUATES -----
# best available dataset was found in the New Jersey state website
# https://dep.nj.gov/wp-content/uploads/airplanning/app-4-4-2016-2023-nj-modeling-inventory-statewide-5-13-24.xlsx
# download_read_table doesn't seem to work with this link
# and you may need to download it manually by going to the URL through your 
# web browser
if (!file.exists("_transportation/data-raw/epa/air_emissions_modeling/EQUATES/app-4-4-2016-2023-nj-modeling-inventory-statewide-5-13-24.xlsx")) {
  download.file("https://dep.nj.gov/wp-content/uploads/airplanning/app-4-4-2016-2023-nj-modeling-inventory-statewide-5-13-24.xlsx",
                destfile = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/app-4-4-2016-2023-nj-modeling-inventory-statewide-5-13-24.xlsx",
                mode = "wb"
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



# Combine all SCCs -----
# create a combined SCC index for use across all data sources
# including NEI, EQUATES, and air emissions modeling platforms
scc_combine <- scc_complete_road %>%
  select(scc6, scc6_desc, scc6_desc_broad, scc6_desc_manual) %>%
  unique() %>%
  bind_rows(scc_equates %>%
              select(scc6, scc6_desc) %>%
              unique()) %>%
  bind_rows(
    scc6_desc_manual %>%
      filter(scc6_new %in% c("220200", "220932")) %>%
      select(
        scc6 = scc6_new, scc6_desc, scc6_desc_manual, alt_mode,
        alt_mode_truck
      )) %>%
  unique() %>%
  # if no official scc6_desc exists, use the manually designated one
  mutate(scc6_desc = ifelse(is.na(scc6_desc), scc6_desc_manual, scc6_desc)) %>%
  filter(!is.na(scc6_desc)) %>%
  mutate(
    scc6_desc =
      case_when(
        scc6 %in% c(
          "220100",
          "223007",
          "220107"
        ) ~ "Diesel; Trucks and buses",
        TRUE ~ scc6_desc
      )
  ) %>%
  mutate(
    # break out and clean fuel and vehicle types
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
    # join back with NEI SCCs
    scc_complete_road %>%
      select(
        scc6_desc,
        alt_mode, alt_mode_truck
      ) %>%
      unique()
  ) %>%
  # remove any "equipment" vehicle types
  filter(str_detect(vehicle_type, "equipment", negate = TRUE)) %>%
  mutate(
    # specify outputs for gas stations, pleasure craft, trucks and buses
    alt_mode = ifelse(vehicle_type %in% c(
      "Gas stations",
      "Pleasure craft",
      "Trucks and buses"
    ), vehicle_type,
    alt_mode
    ),
    alt_mode_truck = ifelse(vehicle_type %in% c(
      "Gas stations",
      "Pleasure craft",
      "Trucks and buses"
    ), vehicle_type,
    alt_mode_truck
    )
  )

# create metadata
scc_combine_meta <- tibble::tribble(
  ~"Column", ~"Class", ~"Description",
  "scc6", class(scc_combine$scc6), "Source classification code (SCC), first six digits only. Includes retired SCCs",
  "scc6_desc", class(scc_combine$scc6_desc), "SCC description",
  "fuel_type", class(scc_combine$fuel_type), "Vehicle fuel type",
  "vehicle_type", class(scc_combine$vehicle_type), "Vehicle type",
  "alt_mode", class(scc_combine$alt_mode), "Alternate vehicle type",
  "alt_mode_truck", class(scc_combine$alt_mode_truck), "Alternate vehicle type, grouping truck and bus categories"
)


saveRDS(scc_combine, "_transportation/data/scc_combine.RDS")
saveRDS(scc_combine_meta, "_transportation/data/scc_combine_meta.RDS")
