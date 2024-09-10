# EQUATES: EPA’s Air QUAlity TimE Series Project
# https://gaftp.epa.gov/Air/emismod/MOVES3/CONUS/
# EPA scientists have developed a set of modeled meteorology, emissions,
# air quality and pollutant deposition spanning the years 2002 through 2019.
# Modeled datasets cover the Conterminous U.S. (CONUS) at a 12km horizontal grid
#  spacing and the Northern Hemisphere at a 108km grid spacing using the Weather
#  Research and Forecasting (WRF) model version 4.1.1 for simulating weather
#  conditions and EPA’s Community Multiscale Air Quality (CMAQ) model version
#  5.3.2 for air quality modeling. New hemispheric and North American emissions
#  inventories were developed using, to the extent possible, consistent input
#  data and methods across all years, including emissions from motor vehicles,
#  fires, and oil and gas sources.

source("R/_load_pkgs.R")
source("_meta/data-raw/county_geography.R")

if (!file.exists("_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2002/inputs/onroad_inv_diesel/diesel_MYR_2002_SMOKE_MOVES_MOVES3_AQstyle_06jan2021_v0.csv")) {
  cli::cli_warn("These files are hefty")

  download.file("https://gaftp.epa.gov/Air/emismod/MOVES3/CONUS/EQUATES_2002_inventory_onroad_01jun2021.zip",
    destfile = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2002_inventory_onroad_23jun2021.zip"
  )


  download.file("https://gaftp.epa.gov/Air/emismod/MOVES3/CONUS/EQUATES_2005_inventory_onroad_23jun2021.zip",
    destfile = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2005_inventory_onroad_23jun2021.zip"
  )

  download.file("https://gaftp.epa.gov/Air/emismod/MOVES3/CONUS/EQUATES_2008_inventory_onroad_11may2021.zip",
    destfile = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2008_inventory_onroad_11may2021.zip"
  )

  download.file("https://gaftp.epa.gov/Air/emismod/MOVES3/CONUS/EQUATES_2011_inventory_onroad_01jun2021.zip",
    destfile = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2011_inventory_onroad_23jun2021.zip"
  )

  download.file("https://gaftp.epa.gov/Air/emismod/MOVES3/CONUS/EQUATES_2014_inventory_onroad_01mar2021.zip",
    destfile = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2014_inventory_onroad_01mar2021.zip"
  )

  download.file("https://gaftp.epa.gov/Air/emismod/MOVES3/CONUS/EQUATES_2017_inventory_onroad_inv_25jan2021.zip",
    destfile = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2017_inventory_onroad_inv_25jan2021.zip"
  )

  unzip(
    zipfile = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2002_inventory_onroad_01jun2021.zip",
    exdir = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/"
  )

  unzip(
    zipfile = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2005_inventory_onroad_23jun2021.zip",
    exdir = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/"
  )

  unzip(
    zipfile = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2008_inventory_onroad_11may2021.zip",
    exdir = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/"
  )

  unzip(
    zipfile = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2011_inventory_onroad_20apr2021.zip",
    exdir = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/"
  )

  unzip(
    zipfile = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2014_inventory_onroad_01mar2021.zip",
    exdir = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/"
  )

  unzip("_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2017_inventory_onroad_inv_25jan2021.zip",
    exdir = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/"
  )
}

read_equates <- function(equates_path) {
  data.table::fread(equates_path,
    skip = 19,
    header = FALSE,
    colClasses = "character",
    col.names = c(
      "country_cd", "region_cd", "tribal_code",
      "census_tract_cd", "shape_id", "scc", "emis_type",
      "poll", "ann_value", "ann_pct_red", "control_ids",
      "control_measures", "current_cost",
      "cumulative_cost", "projection_factor",
      "reg_codes", "calc_method", "calc_year",
      "date_updated", "data_set_id", "jan_value",
      "feb_value", "mar_value", "apr_value", "may_value",
      "jun_value", "jul_value", "aug_value", "sep_value",
      "oct_value", "nov_value", "dec_value", "jan_pctred",
      "feb_pctred", "mar_pctred", "apr_pctred", "may_pctred",
      "jun_pctred", "jul_pctred", "aug_pctred", "sep_pctred",
      "oct_pctred", "nov_pctred", "dec_pctred", "comment"
    )
  ) %>%
    dplyr::filter(
      region_cd %in% c(
        "27095", "27045", "27073", "27085", "27153", "27105", "27001",
        "27057", "27063", "27039", "27047", "27121", "27143", "27109",
        "27067", "27133", "27161", "27033", "27071", "27165", "27171",
        "27027", "27139", "27005", "27107", "27025", "27089", "27169",
        "27007", "27083", "27049", "27061", "27093", "27065", "27077",
        "27081", "27111", "27021", "27037", "27075", "27003", "27099",
        "27101", "27123", "27167", "27059", "27009", "27159", "27113",
        "27157", "27163", "27035", "27155", "27173", "27131", "27145",
        "27141", "27079", "27137", "27031", "27051", "27023", "27043",
        "27017", "27115", "27149", "27069", "27091", "27097", "27117",
        "27129", "27087", "27053", "27015", "27103", "27041", "27151",
        "27147", "27125", "27029", "27011", "27013", "27119", "27127",
        "27055", "27019", "27135", "55111", "55093", "55063", "55033",
        "55053", "55047", "55127", "55123", "55059", "55079", "55003",
        "55085", "55137", "55129", "55065", "55135", "55125", "55089",
        "55117", "55131", "55007", "55097", "55039", "55061", "55067",
        "55105", "55023", "55035", "55083", "55041", "55113", "55121",
        "55095", "55045", "55087", "55001", "55119", "55073", "55037",
        "55005", "55051", "55081", "55101", "55115", "55027", "55025",
        "55015", "55055", "55013", "55017", "55031", "55077", "55009",
        "55103", "55141", "55139", "55069", "55091", "55049", "55075",
        "55099", "55043", "55021", "55019", "55109", "55057", "55107",
        "55133", "55011", "55078", "55071", "55029"
      ),
      poll %in% c(
        "CH4", "N2O",
        "CO2", "NO", "NOX",
        "HFC", "VOC", "O3", "CO",
        "PM10-PRI", "PM25-PRI"
      ),
      emis_type %in% c("RPD", "RPV")
    ) %>%
    dplyr::mutate(dplyr::across(tidyr::ends_with("value"), as.numeric),
      scc6 = stringr::str_sub(scc, 1, 6),
      equates_path = equates_path,
      emissions_short_tons = ann_value
    ) %>%
    dplyr::select(
      -tribal_code, -census_tract_cd,
      -shape_id, -country_cd,
      -date_updated, -data_set_id,
      -cumulative_cost, -reg_codes,
      -ann_pct_red,
      -projection_factor, -calc_method,
      -control_measures, -control_ids,
      -tidyr::starts_with(tolower(month.abb))
    )
}

library(furrr)
# number of workers should match number of items in the
# vector
plan(strategy = future::multisession, workers = 12)

equates <-
  furrr::future_map(
    c(
      "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2002/inputs/onroad_inv_diesel/diesel_MYR_2002_SMOKE_MOVES_MOVES3_AQstyle_06jan2021_v0.csv",
      "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2002/inputs/onroad_inv_gas/gas_MYR_2002_SMOKE_MOVES_MOVES3_AQstyle_28may2021_nf_v1.csv",
      "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2005/inputs/onroad_inv_diesel/diesel_MYR_2005_SMOKE_MOVES_MOVES3_AQstyle_13jan2021_v0.csv",
      "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2005/inputs/onroad_inv_gas/gas_MYR_2005_SMOKE_MOVES_MOVES3_AQstyle_13jan2021_v0.csv",
      "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2008/inputs/onroad_inv_diesel/diesel_MYR_2008_SMOKE_MOVES_MOVES3_AQstyle_30oct2020_v0.csv",
      "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2008/inputs/onroad_inv_gas/gas_MYR_2008_SMOKE_MOVES_MOVES3_AQstyle_30oct2020_v0.csv",
      "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2011/inputs/onroad_inv_diesel/diesel_MYR_2011_SMOKE_MOVES_MOVES3_AQstyle_20apr2021_nf_v1.csv",
      "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2011/inputs/onroad_inv_gas/gas_MYR_2011_SMOKE_MOVES_MOVES3_AQstyle_20apr2021_nf_v1.csv",
      "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2014/inputs/onroad_inv_diesel/diesel_MYR_2014_SMOKE_MOVES_MOVES3_AQstyle_23nov2020_v0.csv",
      "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2014/inputs/onroad_inv_gas/gas_MYR_2014_SMOKE_MOVES_MOVES3_AQstyle_23nov2020_v0.csv",
      "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2017/inputs/onroad_inv_diesel/diesel_MYR_2017_SMOKE_MOVES_MOVES3_AQstyle_15dec2020_v0.csv",
      "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2017/inputs/onroad_inv_gas/gas_MYR_2017_SMOKE_MOVES_MOVES3_AQstyle_15dec2020_v0.csv"
    ),
    read_equates
  ) %>%
  bind_rows()



equates_cprg <- equates %>%
  mutate(geoid = region_cd) %>%
  left_join(counties_light)

saveRDS(equates, "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/equates_mn_wi.RDS")
saveRDS(equates_cprg, "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/equates_cprg.RDS")
