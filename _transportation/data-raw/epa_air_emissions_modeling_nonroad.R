# the EPA publishes data for years in between NEI releases
# these are modeled estimates based on a variety of sources
tictoc::tic(msg = "Air emissions modeling nonroad processing")

source("R/_load_pkgs.R")
source("_meta/data-raw/county_geography.R")
source("_transportation/data-raw/_read_smoke_ff10.R")
scc_combine <- readRDS("_transportation/data/scc_combine.RDS")

library(furrr)
# check that we have all the necessary files -----
if(any(purrr::map(
  c(
    "_transportation/data-raw/epa/air_emissions_modeling/2016/2016gf_16j/inputs/nonroad/2016fj_v2platform_nonroad_from_MOVES_aggSCC_27apr2021_nf_v1.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2017/2017gb_17j/inputs/nonroad/2017nei_nonroad_from_MOVES2014b_aggSCC_15apr2020_nf_v6.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2018/2018gg_18j/inputs/nonroad/2018platform_nonroad_from_MOVES_aggSCC_08oct2021_v2.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2019/2019ge_cb6_19k/inputs/nonroad/2019ge_nonroad_from_MOVES_aggSCC_29oct2021_v1.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2020/2020ha2_cb6_20k/inputs/nonroad/2020NEI_nonroad_from_MOVES_aggSCC_10may2023_v4.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2021/2021hb_cb6_21k/inputs/nonroad/nonroad_ff10_2021hb_MOVES_ROC_AE6_10nov2023_nf_v2.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2022v1/2022hc_cb6_22m/inputs/nonroad/nonroad_ff10_2022hc_MOVES_ROC_AE6_13mar2024_v1.csv"
  ),
  file_exists) == FALSE)){
  cli::cli_abort(c(
    "Required datasets unavailable",
    "*" = "Consult documentation for more information",
    "*" = "{.file _transportation/data-raw/epa/README_epa_downloads.html}"
  ))
}

# read and save -----
plan(strategy = future::multisession, workers = 7)

furrr::future_map(
  c(
    "_transportation/data-raw/epa/air_emissions_modeling/2016/2016gf_16j/inputs/nonroad/2016fj_v2platform_nonroad_from_MOVES_aggSCC_27apr2021_nf_v1.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2017/2017gb_17j/inputs/nonroad/2017nei_nonroad_from_MOVES2014b_aggSCC_15apr2020_nf_v6.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2018/2018gg_18j/inputs/nonroad/2018platform_nonroad_from_MOVES_aggSCC_08oct2021_v2.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2019/2019ge_cb6_19k/inputs/nonroad/2019ge_nonroad_from_MOVES_aggSCC_29oct2021_v1.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2020/2020ha2_cb6_20k/inputs/nonroad/2020NEI_nonroad_from_MOVES_aggSCC_10may2023_v4.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2021/2021hb_cb6_21k/inputs/nonroad/nonroad_ff10_2021hb_MOVES_ROC_AE6_10nov2023_nf_v2.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2022v1/2022hc_cb6_22m/inputs/nonroad/nonroad_ff10_2022hc_MOVES_ROC_AE6_13mar2024_v1.csv"
  ),
  read_smoke_ff10,
  out_directory = "_transportation/data-raw/epa/air_emissions_modeling/air_emissions_modeling_mn_wi/nonroad/"
) 

# read back in, combine, and save -----
nonroad_mn_wi <- purrr::map(list.files("_transportation/data-raw/epa/air_emissions_modeling/air_emissions_modeling_mn_wi/nonroad/", full.names = TRUE),
                            readRDS) %>% 
  bind_rows()

saveRDS(nonroad_mn_wi, "_transportation/data-raw/epa/air_emissions_modeling/nonroad_mn_wi.RDS")

tictoc::toc()
