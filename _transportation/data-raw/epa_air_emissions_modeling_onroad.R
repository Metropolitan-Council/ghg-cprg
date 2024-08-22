# the EPA publishes data for years in between NEI releases
# these are modeled estimates based on a variety of sources
source("R/_load_pkgs.R")
source("_meta/data-raw/county_geography.R")
source("_transportation/data-raw/epa_source_classification_codes.R")


# fetch data from years 2018-2020, which should align with EQUATES

if(!file.exists("_transportation/data-raw/epa/air_emissions_modeling/2022v1/2022hc_cb6_22m/inputs/onroad/2022hc_onroad_SMOKE_MOVES_MOVES4_forAQ_27jun2024_v0.csv")){
  
  # 2020
  download.file("https://gaftp.epa.gov/Air/emismod/2020/2020emissions/2020ha2_onroad_SMOKE-MOVES_emissions_FF10_22sep2023.zip",
                "_transportation/data-raw/epa/air_emissions_modeling/2020/2020ha2_onroad_SMOKE-MOVES_emissions_FF10_22sep2023.zip")
  
  download.file("https://gaftp.epa.gov/Air/emismod/2019/2019emissions/2019ge_onroad_SMOKE-MOVES_emissions_FF10_allHAPs_30nov2021_v0.zip",
                "")
  
  download.file("https://gaftp.epa.gov/Air/emismod/2018/v2/2018emissions/2018gc_onroad_SMOKE-MOVES_emissions_FF10_fullHAP_29sep2021.zip",
                "_transportation/data-raw/epa/air_emissions_modeling/2018/2018gc_onroad_SMOKE-MOVES_emissions_FF10_fullHAP_29sep2021.zip")
  
  
  download.file("https://gaftp.epa.gov/Air/emismod/2017/2017emissions/2017gb_onroad_SMOKE-MOVES_emissions_FF10_29jun2020.zip")
  
  
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
    dplyr::filter(region_cd %in% c("27095", "27045", "27073", "27085", "27153", "27105", "27001", 
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
                                   "55133", "55011", "55078", "55071", "55029"), 
                  poll %in% c("CH4", "N2O",
                              "CO2", "NO", "NOX",
                              "HFC", "VOC", "O3", "CO",
                              "PM10-PRI", "PM25-PRI"),
                  emis_type %in% c("RPD", "RPV")
    ) %>% 
    dplyr::mutate(dplyr::across(tidyr::ends_with("value"), as.numeric),
                  scc6 = stringr::str_sub(scc, 1, 6),
                  equates_path = equates_path,
                  emissions_short_tons = ann_value) %>% 
    dplyr::select(-tribal_code, -census_tract_cd,
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
plan(strategy = future::multisession, workers = 4)


emis_modeled_onroad <- 
  furrr::future_map(
    c(
      # "_transportation/data-raw/epa/air_emissions_modeling/2014/2014fd_cb6_14j/inputs/onroad/2014fd_nata_onroad_SMOKE_MOVES_MOVES2014a_AQstyle_06feb2018_v0.csv",
      # "_transportation/data-raw/epa/air_emissions_modeling/2015/2015fd_cb6_15j/inputs/onroad/2015fd_onroad_SMOKE_MOVES_MOVES2014a_AQstyle_28sep2018_nf_v1.csv",
      # "_transportation/data-raw/epa/air_emissions_modeling/2016/2016gf_16j 2/inputs/onroad/2016gf_onroad_SMOKE_MOVES_MOVES3_30jun2022_v0.csv",
      # "_transportation/data-raw/epa/air_emissions_modeling/2017/2017gb_17j 2/inputs/onroad/2017gb_nata_onroad_SMOKE_MOVES_NATAstyle_14may2020_v0.csv",
      # "_transportation/data-raw/epa/air_emissions_modeling/2018/2018gc_cb6_18j/inputs/onroad/2018gc_SMOKE_MOVES_MOVES3_AQ_fullHAP_29sep2021_v1.csv",
      "_transportation/data-raw/epa/air_emissions_modeling/2019/2019ge_cb6_19k/inputs/onroad/2019ge_SMOKE_MOVES_MOVES3_AQ_fullHAP_30nov2021_v0.csv",
      "_transportation/data-raw/epa/air_emissions_modeling/2020/2020ha2_cb6_20k/inputs/onroad/2020ha2_onroad_SMOKE_MOVES_MOVES3_HAPCAP_style_30may2023_v0.csv",
      "_transportation/data-raw/epa/air_emissions_modeling/2021/2021hb_cb6_21k 2/inputs/onroad/2021hb_onroad_SMOKE_MOVES_MOVES4_hapcap_04dec2023_v0.csv",
      "_transportation/data-raw/epa/air_emissions_modeling/2022v1/2022hc_cb6_22m/inputs/onroad/2022hc_onroad_SMOKE_MOVES_MOVES4_forAQ_27jun2024_v0.csv"),
    read_equates) %>% 
  bind_rows()


saveRDS(emis_modeled_onroad, "_transportation/data-raw/epa/air_emissions_modeling/onroad_mn_wi_19_22.RDS")


rm(emis_modeled_onroad)

plan(strategy = future::multisession, workers = 4)


emis_modeled_onroad <- 
  furrr::future_map(
    c(
      # "_transportation/data-raw/epa/air_emissions_modeling/2014/2014fd_cb6_14j/inputs/onroad/2014fd_nata_onroad_SMOKE_MOVES_MOVES2014a_AQstyle_06feb2018_v0.csv",
      "_transportation/data-raw/epa/air_emissions_modeling/2015/2015fd_cb6_15j/inputs/onroad/2015fd_onroad_SMOKE_MOVES_MOVES2014a_AQstyle_28sep2018_nf_v1.csv",
      "_transportation/data-raw/epa/air_emissions_modeling/2016/2016gf_16j 2/inputs/onroad/2016gf_onroad_SMOKE_MOVES_MOVES3_30jun2022_v0.csv",
      "_transportation/data-raw/epa/air_emissions_modeling/2017/2017gb_17j 2/inputs/onroad/2017gb_nata_onroad_SMOKE_MOVES_NATAstyle_14may2020_v0.csv",
      "_transportation/data-raw/epa/air_emissions_modeling/2018/2018gc_cb6_18j/inputs/onroad/2018gc_SMOKE_MOVES_MOVES3_AQ_fullHAP_29sep2021_v1.csv"),
      # "_transportation/data-raw/epa/air_emissions_modeling/2019/2019ge_cb6_19k/inputs/onroad/2019ge_SMOKE_MOVES_MOVES3_AQ_fullHAP_30nov2021_v0.csv",
      # "_transportation/data-raw/epa/air_emissions_modeling/2020/2020ha2_cb6_20k/inputs/onroad/2020ha2_onroad_SMOKE_MOVES_MOVES3_HAPCAP_style_30may2023_v0.csv",
      # "_transportation/data-raw/epa/air_emissions_modeling/2021/2021hb_cb6_21k 2/inputs/onroad/2021hb_onroad_SMOKE_MOVES_MOVES4_hapcap_04dec2023_v0.csv",
      # "_transportation/data-raw/epa/air_emissions_modeling/2022v1/2022hc_cb6_22m/inputs/onroad/2022hc_onroad_SMOKE_MOVES_MOVES4_forAQ_27jun2024_v0.csv"),
    read_equates) %>% 
  bind_rows()


saveRDS(emis_modeled_onroad, "_transportation/data-raw/epa/air_emissions_modeling/onroad_mn_wi_15_28.RDS")
