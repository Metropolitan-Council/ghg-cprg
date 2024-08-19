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

if(!file.exists("_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2002/inputs/onroad_inv_diesel/diesel_MYR_2002_SMOKE_MOVES_MOVES3_AQstyle_06jan2021_v0.csv")){
  cli::cli_warn("These files are hefty")
  
  download.file("https://gaftp.epa.gov/Air/emismod/MOVES3/CONUS/EQUATES_2002_inventory_onroad_01jun2021.zip",
                destfile = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2002_inventory_onroad_23jun2021.zip")
  
  
  download.file("https://gaftp.epa.gov/Air/emismod/MOVES3/CONUS/EQUATES_2005_inventory_onroad_23jun2021.zip",
                destfile = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2005_inventory_onroad_23jun2021.zip")
  
  download.file("https://gaftp.epa.gov/Air/emismod/MOVES3/CONUS/EQUATES_2008_inventory_onroad_11may2021.zip",
                destfile = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2008_inventory_onroad_11may2021.zip")
  
  download.file("https://gaftp.epa.gov/Air/emismod/MOVES3/CONUS/EQUATES_2011_inventory_onroad_01jun2021.zip",
                destfile = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2011_inventory_onroad_23jun2021.zip")
  
  download.file("https://gaftp.epa.gov/Air/emismod/MOVES3/CONUS/EQUATES_2014_inventory_onroad_01mar2021.zip",
                destfile = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2014_inventory_onroad_01mar2021.zip")
  
  download.file("https://gaftp.epa.gov/Air/emismod/MOVES3/CONUS/EQUATES_2017_inventory_onroad_inv_25jan2021.zip",
                destfile = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2017_inventory_onroad_inv_25jan2021.zip")
  
  unzip(zipfile = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2002_inventory_onroad_01jun2021.zip",
        exdir = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/")
  
  unzip(zipfile = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2005_inventory_onroad_23jun2021.zip",
        exdir = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/")
  
  unzip(zipfile = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2008_inventory_onroad_11may2021.zip",
        exdir = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/")
  
  unzip(zipfile = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2011_inventory_onroad_20apr2021.zip",
        exdir = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/")
  
  unzip(zipfile = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2014_inventory_onroad_01mar2021.zip",
        exdir = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/")
  
  unzip("_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2017_inventory_onroad_inv_25jan2021.zip",
        exdir = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/")
}

read_equates <- function(equates_path){
  data.table::fread(equates_path,
                    skip = 19,
                    header = FALSE,
                    colClasses = "character",
                    col.names =  c("country_cd","region_cd","tribal_code",
                                   "census_tract_cd","shape_id","scc","emis_type",
                                   "poll","ann_value","ann_pct_red","control_ids",
                                   "control_measures","current_cost",
                                   "cumulative_cost","projection_factor",
                                   'reg_codes',"calc_method","calc_year",
                                   "date_updated","data_set_id","jan_value",
                                   "feb_value","mar_value","apr_value","may_value",
                                   "jun_value","jul_value","aug_value","sep_value",
                                   'oct_value',"nov_value","dec_value","jan_pctred",
                                   "feb_pctred","mar_pctred","apr_pctred","may_pctred",
                                   "jun_pctred","jul_pctred","aug_pctred","sep_pctred",
                                   "oct_pctred","nov_pctred",'dec_pctred','comment')
  ) %>% 
    dplyr::filter(region_cd %in% county_geography$geoid,
                         poll %in% c("CH4", "N2O", "CO2",
                                     "PM10-PRI", "PM25-PRI")
    ) %>% 
    dplyr::left_join(counties_light, 
                     by = c("region_cd" = "geoid")) %>% 
    dplyr::mutate(across(ends_with("value"), as.numeric),
                  scc6 = str_sub(scc, 1, 6)) %>% 
    dplyr::select(-tribal_code, -census_tract_cd,
                  -shape_id, -country_cd,
                  -date_updated, -data_set_id,
                  -cumulative_cost, -reg_codes,
                  -ann_pct_red,
                  -projection_factor, -calc_method,
                  -control_measures, -control_ids,
                  -starts_with(tolower(month.abb))
    )
}

library(furrr)
plan(strategy = future::cluster)

equates <-
  furrr::future_map(c(
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
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_2017/inputs/onroad_inv_gas/gas_MYR_2017_SMOKE_MOVES_MOVES3_AQstyle_15dec2020_v0.csv"),
    
    read_equates) %>%
  bind_rows()



equates_cprg <- equates %>% 
  filter(poll %in% c("CH4", "N2O", "CO2", "PM10-PRI", "PM25-PRI"),
         emis_type %in% c("RPD", "RPV"))

saveRDS(equates, "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/equates_mn_wi.RDS")
saveRDS(equates_cprg, "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/equates_cprg.RDS")
