# EPA NEI scraps
# This script is not meant to be run from top to bottom, but 
# used as a junkyard for code that may be useful in the future, but 
# is obsolete now. 

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


#
# == Comparison of EQUATES emissions totals other EPA emissions data ==
# Emissions_comparison_EQUATES_vs_EPA_Trends_version_20220210.csv provides annual total emissions (US short tons) summed over the Lower 48 from three data sources:
#   - Feb 10, 2022 version of the EPA's Air Pollution Emissions Trends data https://www.epa.gov/air-emissions-inventories/air-pollutant-emissions-trends-data (Note: Only the most recent version of the trends data are available on this website)
#   - EQUATES INV annual summaries for the Tier 1 source categories used in the Trends data
#   - Annual summaries from some of EPA's previous emissions modeling platforms for the Tier 1 source categories used in the Trends data
#   NOTE this does not contain CO2

equates_epa_comps <- read.csv("_transportation/data-raw/epa/air_emissions_modeling/EQUATES/Dataverse/Emissions_comparison_EQUATES_vs_EPA_Trends_version_20220210.csv") %>%
  clean_names() %>%
  pivot_longer(4:19,
    names_to = "inventory_year",
    values_to = "emissions_short_tons"
  ) %>%
  filter(
    source_category %in% c(
      "HIGHWAY VEHICLES",
      "OFF-HIGHWAY"
    )
  ) %>%
  mutate(
    inventory_year = stringr::str_remove(inventory_year, "x") %>%
      as.numeric(),
    emissions_ton = emissions_short_tons %>%
      units::as_units("short_ton") %>%
      units::set_units("ton") %>%
      as.numeric()
  )


equates_national <- fread("_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_M3_bySCC/EQUATES_M3_bySCC_annual.csv",
  colClasses = "character"
) %>%
  pivot_longer(3:19) %>%
  mutate(value = as.numeric(value)) %>%
  clean_names()


# deep cuts from NEI
source("R/download_read_table.R")
source("_meta/data-raw/county_geography.R")
source("R/_load_pkgs.R")
source("_transportation/data-raw/epa_source_classification_codes.R")
dot_vmt <- readRDS("_transportation/data/dot_vmt.RDS")
options(timeout = 130)

# 2020 NEI -----
# ## find which counties were used as representatives for all others  -----
if (!file.exists("_transportation/data-raw/epa/nei/2020NEI/2020_Representative_Counties_Analysis_20220720.xlsx")) {
  rep_counties_raw <- download_read_table("https://gaftp.epa.gov/Air/nei/2020/doc/supporting_data/onroad/2020_Representative_Counties_Analysis_20220720.xlsx",
    exdir = "_transportation/data-raw/epa/nei/",
    sheet = 2,
    col_types = "text"
  ) %>%
    clean_names() %>%
    filter(county_id %in% county_geography$geoid)
} else {
  rep_counties_raw <- readxl::read_excel("_transportation/data-raw/epa/nei/2020NEI/2020_Representative_Counties_Analysis_20220720.xlsx",
    sheet = 2,
    col_types = "text"
  ) %>%
    clean_names() %>%
    filter(county_id %in% county_geography$geoid)
}

counties_light <- county_geography %>%
  select(geoid, state_name, county_name, cprg_area)


rep_counties <- counties_light %>%
  left_join(rep_counties_raw,
    by = c("geoid" = "county_id")
  ) %>%
  left_join(counties_light,
    by = c("x2020_final_rep_county" = "geoid")
  ) %>%
  mutate(self_represented = county_name.x == county_name.y) %>%
  select(geoid,
    county_name = county_name.x,
    represented_by = county_name.y,
    cprg_area = cprg_area.x,
    self_represented
  )

## onroad activity data -----

scc_codes <- read_xlsx("_transportation/data-raw/epa/nei/2020NEI/onroad_activity_data_SCC_descriptions.xlsx",
  sheet = 1,
  col_types = "text"
) %>%
  clean_names()

# download and unzip if it doesn't already exist
# All three data types are in FF10 format for SMOKE and are a combination of
#   EPA estimates, agency submittals, and corrections:
# 1.Vehicle population by county and SCC covering every county in the U.S.,
# 2.VMT annual and monthly by county and SCC covering every county in the U.S., and
# 3.Hoteling hours annual and monthly by county covering every county in the U.S.
#   including hours of extended idle and hours of auxiliary power units for
#   combination long-haul trucks only.
# 4.Off-network idle hours by county and SCC.
# 5.Starts by county and SCC.
if (!file.exists("_transportation/data-raw/epa/nei/2020NEI/2020NEI_onroad/inputs/onroad/VMT_2020NEI_full_monthly_run3_09jan2023_v0.csv")) {
  download.file("https://gaftp.epa.gov/Air/nei/2020/doc/supporting_data/onroad/2020NEI_onroad_activity_final_20230112.zip",
    destfile = "_transportation/data-raw/epa/nei/2020NEI/2020NEI_onroad_activity_final_20230112.zip"
  )
  unzip(
    zipfile = "_transportation/data-raw/epa/nei/2020NEI/2020NEI_onroad_activity_final_20230112.zip",
    exdir = "_transportation/data-raw/epa/nei/2020NEI/",
    overwrite = TRUE
  )
}


onroad_input_colnames <- c(
  "country_cd", "region_cd", "tribal_code", "census_tract_cd",
  "shape_id", "scc", "CD", "MSR", "activity_type", "ann_parm_value",
  "calc_year", "date_updated", "data_set_id", "jan_value",
  "feb_value", "mar_value", "apr_value", "may_value",
  "jun_value", "jul_value", "aug_value", "sep_value",
  "oct_value", "nov_value", "dec_value", "comment"
)

vmt <- data.table::fread("_transportation/data-raw/epa/nei/2020NEI/2020NEI_onroad/inputs/onroad/VMT_2020NEI_full_monthly_run3_09jan2023_v0.csv",
  skip = 16,
  header = FALSE,
  colClasses = "character",
  col.names = onroad_input_colnames
) %>%
  filter(region_cd %in% county_geography$geoid) %>%
  left_join(counties_light,
    by = c("region_cd" = "geoid")
  ) %>%
  left_join(scc_codes, by = c("scc" = "scc")) %>%
  mutate(across(ends_with("value"), as.numeric)) %>%
  rowwise() %>%
  select(-tribal_code, -census_tract_cd)

hoteling <- data.table::fread("_transportation/data-raw/epa/nei/2020NEI/2020NEI_onroad/inputs/onroad/HOTELING_2020NEI_monthly_23nov2022_v4.csv",
  skip = 15,
  header = FALSE,
  colClasses = "character",
  col.names = onroad_input_colnames
) %>%
  filter(region_cd %in% county_geography$geoid) %>%
  left_join(counties_light,
    by = c("region_cd" = "geoid")
  ) %>%
  left_join(scc_codes, by = c("scc" = "scc")) %>%
  mutate(across(ends_with("value"), as.numeric)) %>%
  rowwise()


starts <- data.table::fread("_transportation/data-raw/epa/nei/2020NEI/2020NEI_onroad/inputs/onroad/STARTS_2020NEI_full_monthly_run3_09jan2023_v0.csv",
  skip = 15,
  colClasses = "character",
  col.names = onroad_input_colnames
) %>%
  filter(region_cd %in% county_geography$geoid) %>%
  left_join(counties_light,
    by = c("region_cd" = "geoid")
  ) %>%
  left_join(scc_codes, by = c("scc" = "scc")) %>%
  mutate(across(ends_with("value"), as.numeric)) %>%
  rowwise()


oni <- data.table::fread("_transportation/data-raw/epa/nei/2020NEI/2020NEI_onroad/inputs/onroad/ONI_2020NEI_full_monthly_run3_09jan2023_v0.csv",
  skip = 16,
  colClasses = "character",
  col.names = onroad_input_colnames
) %>%
  filter(region_cd %in% county_geography$geoid) %>%
  left_join(counties_light,
    by = c("region_cd" = "geoid")
  ) %>%
  left_join(scc_codes, by = c("scc" = "scc")) %>%
  mutate(across(ends_with("value"), as.numeric)) %>%
  rowwise()

# vehicle population
vpop <- data.table::fread("_transportation/data-raw/epa/nei/2020NEI/2020NEI_onroad/inputs/onroad/VPOP_2020NEI_full_20220729_07oct2022_v1.csv",
  skip = 27,
  colClasses = "character",
  col.names = onroad_input_colnames
) %>%
  filter(region_cd %in% county_geography$geoid) %>%
  left_join(counties_light,
    by = c("region_cd" = "geoid")
  ) %>%
  left_join(scc_codes, by = c("scc" = "scc")) %>%
  mutate(across(ends_with("value"), as.numeric)) %>%
  rowwise()

## temperatures -----
# The temperature and relative humidity bins for running MOVES to create
# the full range of emissions factors necessary to run SMOKEMOVES and the
# ZMH files used to run MOVES. Generated by running the SMOKE Met4moves program.
download.file("https://gaftp.epa.gov/Air/nei/2020/doc/supporting_data/onroad/2020NEI_RepCounty_Temperatures.zip",
  destfile = "_transportation/data-raw/epa/nei/2020NEI/2020NEI_RepCounty_Temperatures.zip"
)


## MOVES run specifications -----
# download.file("https://gaftp.epa.gov/Air/nei/2020/doc/supporting_data/onroad/CDBs_for_rep_counties/2020_RepCounty_Runspecs.zip",
#               "_transportation/data-raw/epa/nei/2020_RepCounty_Runspecs.zip"
# )
# unzip("_transportation/data-raw/epa/nei/2020_RepCounty_Runspecs.zip",
#       exdir = "_transportation/data-raw/epa/nei/2020_RepCounty_Runspecs/",
#     overwrite = TRUE)

## speed and distance -----

# download.file("https://gaftp.epa.gov/Air/nei/2020/doc/supporting_data/onroad/2020NEI_spdist.zip",
#               "_transportation/data-raw/epa/nei/2020NEI_spdist.zip"
# )

## compare VMT data ------

vmt_summary <- vmt %>%
  mutate(
    geoid = region_cd,
    year = calc_year
  ) %>%
  filter(cprg_area == TRUE) %>%
  group_by(NAME, cprg_area, geoid, year) %>%
  summarize(ann_parm_value = sum(ann_parm_value, na.rm = T))

# close-ish? Ramsey County has the biggest disparity, for some reason
dot_vmt %>%
  right_join(vmt_summary,
    by = c(
      "geoid", "cprg_area",
      "year"
    )
  ) %>%
  mutate(
    vmt_diff = annual_vmt - ann_parm_value,
    vmt_pct_diff = (vmt_diff / annual_vmt)
  ) %>%
  arrange(vmt_pct_diff)

# compare with streetlight VMT results
readRDS("_transportation/data/county_vmt_emissions.RDS") %>%
  group_by(zone, year) %>%
  summarize(vmt_total = sum(vmt_total)) %>%
  arrange(-vmt_total)

# 2017 NEI -----

# download_read_table("https://gaftp.epa.gov/air/nei/2017/doc/supporting_data/onroad/2017_Representative_Counties_Analysis_20191220.xlsx",
#                     exdir = "_transportation/data-raw/epa/nei/2017NEI/",
#                     sheet = 2)

download.file("https://gaftp.epa.gov/air/nei/2017/doc/supporting_data/onroad/MOVES_Input_DBs.zip",
  destfile = "_transportation/data-raw/epa/nei/2017NEI/MOVES_Input_DBs.zip"
)



vmt17 <- data.table::fread("_transportation/data-raw/epa/nei/2017NEI/2017NEI_onroad_activity_final/VMT_2017NEI_final_from_CDBs_month_redist_27mar2020_v3.csv",
  skip = 16,
  header = FALSE,
  colClasses = "character",
  col.names = onroad_input_colnames
) %>%
  filter(region_cd %in% county_geography$geoid) %>%
  left_join(counties_light,
    by = c("region_cd" = "geoid")
  ) %>%
  left_join(scc_codes, by = c("scc" = "scc")) %>%
  mutate(across(ends_with("value"), as.numeric)) %>%
  rowwise() %>%
  select(-tribal_code, -census_tract_cd)


vmt17_summary <- vmt17 %>%
  mutate(
    geoid = region_cd,
    year = calc_year
  ) %>%
  filter(cprg_area == TRUE) %>%
  group_by(NAME, cprg_area, geoid, year) %>%
  summarize(ann_parm_value = sum(ann_parm_value, na.rm = T))


# 2017 VMT differences are MUCH smaller (practically negligible)
dot_vmt %>%
  right_join(vmt17_summary,
    by = c(
      "geoid", "cprg_area",
      "year"
    )
  ) %>%
  mutate(
    vmt_diff = annual_vmt - ann_parm_value,
    vmt_pct_diff = (vmt_diff / annual_vmt)
  ) %>%
  arrange(-annual_vmt)


# EPA NEI pre-2008 data -----
# # !IMPORTANT! This script is left as is for potential future use.
# Note that this 2005 data does NOT include CO2 or note distinct
# GHGs.
#
# NEI pre-2008 data were downloaded individually from ___
# Downloads are in .mdb (Microsoft Access) format, which is not easily opened
# on macOS.
# Each download was opened in MS Access and each Table was
# exported as a text file (.txt)
# These were then transferred to our MS Teams/OneDrive location
# and then transferred to local machine

source("R/_load_pkgs.R")

# Source Classification Codes (SCCs) categories -----
# quick download from https://sor-scc-api.epa.gov/sccwebservices/sccsearch/
scc_codes <- data.table::fread("_meta/data-raw/epa/SCCDownload-2024-0718-132502.csv",
                               header = TRUE,
                               colClasses = "character"
) %>%
  clean_names()

scc_onroad_county <- data.table::fread(
  "_meta/data-raw/epa/nei_2005/SCC onroad/SCC County.txt",
  sep = ",",
  header = TRUE,
  colClasses = c(
    "character",
    "character",
    "character",
    "numeric"
  )
) %>%
  clean_names()

scc_nonroad <- data.table::fread(
  "_meta/data-raw/epa/nei_2005/SCC nonroad/SCC County 1.txt",
  sep = ",",
  header = TRUE,
  colClasses = c(
    "character",
    "character",
    "character",
    "numeric"
  )
) %>%
  clean_names()

scc_point <- data.table::fread(
  "_meta/data-raw/epa/nei_2005/SCC point/POINT05_V2_SCC_SUMMARY_COUNTY.txt",
  sep = ",",
  header = TRUE,
  colClasses = c(
    rep("character", 13),
    "numeric"
  )
) %>%
  clean_names()

scc_nonpoint <- data.table::fread("_meta/data-raw/epa/nei_2005/SCC nonpoint/NONPOINT05_V2_SCC_SUMMARY_COUNTY.txt",
                                  sep = ",",
                                  header = TRUE,
                                  colClasses = c(
                                    rep("character", 12),
                                    "numeric",
                                    rep("character", 2)
                                  )
) %>%
  clean_names()

# by tiers -----
tier1_county <- data.table::fread("_meta/data-raw/epa/nei_2005/Tier1/Tier1 County.txt",
                                  sep = ",",
                                  header = TRUE,
                                  colClasses = c(
                                    rep("character", 9),
                                    "numeric"
                                  )
) %>%
  clean_names()

tier2_county <- data.table::fread(
  "_meta/data-raw/epa/nei_2005/Tier2/Tier2 County (00-30).txt",
  sep = ",",
  header = TRUE,
  colClasses = c(
    rep("character", 12),
    "numeric"
  )
) %>%
  rbind(
    data.table::fread(
      "_meta/data-raw/epa/nei_2005/Tier2/Tier2 County (31-78).txt",
      sep = ",",
      header = TRUE,
      colClasses = c(
        rep("character", 12),
        "numeric"
      )
    )
  ) %>%
  clean_names()

tier3_county <- data.table::fread(
  "_meta/data-raw/epa/nei_2005/Tier3/Tier3 County (00-30).txt",
  sep = ",",
  header = TRUE,
  colClasses = c(
    rep("character", 14),
    "numeric"
  )
) %>%
  rbind(data.table::fread(
    "_meta/data-raw/epa/nei_2005/Tier3/Tier3 County (31-78).txt",
    sep = ",",
    header = TRUE,
    colClasses = c(
      rep("character", 14),
      "numeric"
    )
  )) %>%
  clean_names()

# filter to our states, review -----

# get tier descriptions

scc_pollutant_desc <-
  scc_onroad_county %>%
  select(pollutant_code) %>%
  bind_rows(scc_nonroad)

tier_desc <- tier3_county %>%
  select(
    tier1_code, tier1_description,
    tier2_code, tier2_description,
    tier3_code, tier3_description
  ) %>%
  unique()

pollutant_desc <- tier3_county %>%
  select(
    pollutant_code,
    pollutant_code_description
  ) %>%
  bind_rows(tier1_county %>%
              select(pollutant_code, pollutant_code_description)) %>%
  bind_rows(tier2_county %>%
              select(pollutant_code, pollutant_code_description)) %>%
  unique()


mn_wi_scc_onroad <- scc_onroad_county %>%
  filter(state_county_fips %in% county_geography$GEOID) %>%
  left_join(
    county_geography %>%
      select(GEOID, NAME, STATE) %>%
      sf::st_drop_geometry(),
    by = c("state_county_fips" = "GEOID")
  ) %>%
  left_join(scc_codes, by = "scc") %>%
  mutate(
    nei_inventory_year = 2005,
    GEOID = state_county_fips
  )

mn_wi_scc_nonroad <- scc_nonroad %>%
  filter(state_county_fips %in% county_geography$GEOID) %>%
  left_join(
    county_geography %>%
      select(GEOID, NAME, STATE) %>%
      sf::st_drop_geometry(),
    by = c("state_county_fips" = "GEOID")
  ) %>%
  left_join(scc_codes, by = "scc") %>%
  mutate(
    nei_inventory_year = 2005,
    GEOID = state_county_fips
  )

sectors

onroad <- mn_wi_scc_onroad %>%
  left_join(pollutant_desc,
            by = "pollutant_code"
  ) %>%
  select(
    nei_inventory_year,
    NAME, GEOID, scc,
    pollutant_code,
    pollutant_code_description,
    data_category,
    sector,
    starts_with("scc"),
    starts_with("tier"),
    annual_emissions
  )


nei_county_multi_year


# this was ultimately a moot exercise in comparing the data sources
# As noted in the on-road documentation for the NEI (@usepa2020NationalEmissions2023),
# NEI is bottom up,
# GHG Inventory is top-down (starting from fuel consumption,
# which is then apportioned to vehicle and fuel types).
# They cannot be reconciled, regardless of whether they are aggregated to the state level.

source("_transportation/data-raw/epa_nei_envirofacts.R")
source("_transportation/data-raw/epa_ghg_inventory.R")

county_proportions <- read_rds("_meta/data/cprg_county_proportions.RDS")

county_proportions_summary <- county_proportions %>%
  group_by(STATE, year) %>%
  summarize(region_proportion_of_state_pop = sum(county_proportion_of_state_pop))

state_ipcc_summary <- ipcc_transportation %>%
  group_by(inventory_year) %>%
  summarize(state_emissions = sum(emissions_metric_tons_co2e))

state_economic_summary <- state_economic %>%
  filter(
    # inventory_year %in% epa_nei$nei_inventory_year,
    sector_group == "Transportation",
    `Sector/Source` %in% c(
      "Mobile Combustion",
      "CO2 from Fossil Fuel Combustion",
      "Non-Energy Use of Fuels"
    )
  ) %>%
  group_by(inventory_year) %>%
  summarize(state_emissions = sum(emissions_metric_tons_co2e, na.rm = T))

# nei_summary <- epa_nei %>%
#   mutate(nei_inventory_year = as.character(nei_inventory_year)) %>%
#   left_join(
#     cprg_county %>%
#       mutate(state = STATE) %>%
#       sf::st_drop_geometry(),
#     by = c("county_name" = "NAME")
#   ) %>%
#   group_by(nei_inventory_year) %>%
#   summarize(
#     region_emissions = sum(emissions_metric_tons_co2e, na.rm = T),
#     .groups = "keep"
#   )

nei_state_summary <- nei_state_emissions %>%
  mutate(nei_inventory_year = as.character(nei_inventory_year)) %>%
  group_by(nei_inventory_year) %>%
  summarize(
    region_emissions = sum(emissions_metric_tons_co2e, na.rm = T),
    .groups = "keep"
  ) %>%
  ungroup()

inventory_comp <- state_economic_summary %>%
  left_join(nei_summary,
            by = c("state",
                   "inventory_year" = "nei_inventory_year"
            )
  ) %>%
  left_join(county_proportions_summary, by = c(
    "inventory_year" = "year",
    "state" = "STATE"
  )) %>%
  # ungroup() %>%
  mutate(regional_proportion = region_emissions / state_emissions)


state_economic_summary

plot_ly(
  name = "Inventory",
  type = "scatter",
  mode = "lines+markers",
  data = state_ipcc_summary %>%
    filter(inventory_year >= 2008),
  x = ~inventory_year,
  y = ~state_emissions
) %>%
  add_trace(
    name = "NEI",
    type = "scatter",
    inherit = FALSE,
    mode = "lines+markers",
    data = nei_state_summary,
    x = ~nei_inventory_year,
    y = ~region_emissions,
    line = list(
      dash = "dot"
    )
  ) %>%
  plotly_layout(
    main_title = "Significant differences in NEI and Inventory underlying datasets",
    subtitle = "MN + WI state-level summary"
  )
