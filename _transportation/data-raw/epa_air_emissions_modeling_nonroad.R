# the EPA publishes data for years in between NEI releases
# these are modeled estimates based on a variety of sources
source("R/_load_pkgs.R")
source("_meta/data-raw/county_geography.R")
source("_transportation/data-raw/epa_source_classification_codes.R")


# fetch data from years 2018-2020, which should align with EQUATES

if (!file.exists("_transportation/data-raw/epa/air_emissions_modeling/2022v1/2022hc_cb6_22m 2/inputs/nonroad/nonroad_ff10_2022hc_MOVES_ROC_AE6_13mar2024_v1.csv")) {
  download.file(
    "https://gaftp.epa.gov/Air/emismod/2020/2020emissions/2020ha2_nonroad_inventory_22sep2023.zip",
    "_transportation/data-raw/epa/air_emissions_modeling/2020/2020ha2_nonroad_inventory_22sep2023.zip"
  )

  download.file(
    "https://gaftp.epa.gov/Air/emismod/2021/2021emissions/nonroad_inventory_2021hb_24may2024.zip",
    "_transportation/data-raw/epa/air_emissions_modeling/2021/nonroad_inventory_2021hb_24may2024.zip"
  )

  download.file(
    "https://gaftp.epa.gov/Air/emismod/2022/v1/2022emissions/nonroad_inventory_2022hc_17jul2024.zip",
    "_transportation/data-raw/epa/air_emissions_modeling/2022v1/nonroad_inventory_2022hc_17jul2024 (1).zip"
  )

  download.file(
    "https://gaftp.epa.gov/Air/emismod/2019/2019emissions/2019ge_nonroad_inventory_10mar2023.zip",
    "_transportation/data-raw/epa/air_emissions_modeling/2019/2019ge_nonroad_inventory_10mar2023.zip"
  )

  download.file(
    "https://gaftp.epa.gov/Air/emismod/2018/v2/2018emissions/2018gg_inventory_nonroad_04apr2023.zip",
    "_transportation/data-raw/epa/air_emissions_modeling/2018/2018gg_inventory_nonroad_04apr2023.zip"
  )
}


read_emismod_nonroad <- function(equates_path) {
  data.table::fread(equates_path,
    skip = 31,
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
      )
    ) %>%
    dplyr::mutate(dplyr::across(tidyr::ends_with("value"), as.numeric),
      scc6 = stringr::str_sub(scc, 1, 6),
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


emis_modeled_nonroad <-
  purrr::map(
    c(
      "_transportation/data-raw/epa/air_emissions_modeling/2016/2016gf_16j/inputs/nonroad/2016fj_v2platform_nonroad_from_MOVES_aggSCC_27apr2021_nf_v1.csv",
      "_transportation/data-raw/epa/air_emissions_modeling/2017/2017gb_17j/inputs/nonroad/2017nei_nonroad_from_MOVES2014b_aggSCC_15apr2020_nf_v6.csv",
      "_transportation/data-raw/epa/air_emissions_modeling/2018/2018gg_18j/inputs/nonroad/2018platform_nonroad_from_MOVES_aggSCC_08oct2021_v2.csv",
      "_transportation/data-raw/epa/air_emissions_modeling/2019/2019ge_cb6_19k 2/inputs/nonroad/2019ge_nonroad_from_MOVES_aggSCC_29oct2021_v1.csv",
      "_transportation/data-raw/epa/air_emissions_modeling/2020/2020ha2_cb6_20k 2/inputs/nonroad/2020NEI_nonroad_from_MOVES_aggSCC_10may2023_v4.csv",
      "_transportation/data-raw/epa/air_emissions_modeling/2021/2021hb_cb6_21k 3/inputs/nonroad/nonroad_ff10_2021hb_MOVES_ROC_AE6_10nov2023_nf_v2.csv",
      "_transportation/data-raw/epa/air_emissions_modeling/2022v1/2022hc_cb6_22m 2/inputs/nonroad/nonroad_ff10_2022hc_MOVES_ROC_AE6_13mar2024_v1.csv"
    ),
    read_emismod_nonroad
  ) %>%
  bind_rows()



saveRDS(emis_modeled_nonroad, "_transportation/data-raw/epa/air_emissions_modeling/nonroad_mn_wi.RDS")
