# the EPA publishes data for years in between NEI releases
# these are modeled estimates based on a variety of sources
tictoc::tic(msg = "Air emissions modeling onroad processing")

source("R/_load_pkgs.R")
source("_meta/data-raw/county_geography.R")
source("_transportation/data-raw/_read_smoke_ff10.R")
scc_combine <- readRDS("_transportation/data/scc_combine.RDS")
library(furrr)

# check that we have all the necessary files -----
if(any(purrr::map(
  c(
    "_transportation/data-raw/epa/air_emissions_modeling/2011/2011el_cb6v2_v6_11g/inputs/onroad/2011el_onroad_SMOKE_MOVES_MOVES2014a_forOTAQ_31aug2016_v1_part1.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2014/2014fd_cb6_14j/inputs/onroad/2014fd_nata_onroad_SMOKE_MOVES_MOVES2014a_AQstyle_06feb2018_v0.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2015/2015fd_cb6_15j/inputs/onroad/2015fd_onroad_SMOKE_MOVES_MOVES2014a_AQstyle_28sep2018_nf_v1.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2016/2016gf_16j/inputs/onroad/2016gf_onroad_SMOKE_MOVES_MOVES3_30jun2022_v0.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2017/2017gb_17j/inputs/onroad/2017gb_nata_onroad_SMOKE_MOVES_NATAstyle_14may2020_v0.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2018/2018gc_cb6_18j/inputs/onroad/2018gc_SMOKE_MOVES_MOVES3_AQ_fullHAP_29sep2021_v1.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2019/2019ge_cb6_19k/inputs/onroad/2019ge_SMOKE_MOVES_MOVES3_AQ_fullHAP_30nov2021_v0.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2020/2020ha2_cb6_20k/inputs/onroad/2020ha2_onroad_SMOKE_MOVES_MOVES3_HAPCAP_style_30may2023_v0.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2021/2021hb_cb6_21k/inputs/onroad/2021hb_onroad_SMOKE_MOVES_MOVES4_hapcap_04dec2023_v0.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2022v1/2022hc_cb6_22m/inputs/onroad/2022hc_onroad_SMOKE_MOVES_MOVES4_forAQ_27jun2024_v0.csv"
  ),
  file_exists) == FALSE)){
  cli::cli_abort(c(
    "Required datasets unavailable",
    "*" = "Consult documentation for more information",
    "*" = "{.file _transportation/data-raw/epa/README_epa_downloads.html}"
  ))
}

# read and save -----

# number of workers should match number of items in the vector

plan(strategy = future::multisession, workers = 4)

furrr::future_map(
  c(
    "_transportation/data-raw/epa/air_emissions_modeling/2019/2019ge_cb6_19k/inputs/onroad/2019ge_SMOKE_MOVES_MOVES3_AQ_fullHAP_30nov2021_v0.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2020/2020ha2_cb6_20k/inputs/onroad/2020ha2_onroad_SMOKE_MOVES_MOVES3_HAPCAP_style_30may2023_v0.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2021/2021hb_cb6_21k/inputs/onroad/2021hb_onroad_SMOKE_MOVES_MOVES4_hapcap_04dec2023_v0.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2022v1/2022hc_cb6_22m/inputs/onroad/2022hc_onroad_SMOKE_MOVES_MOVES4_forAQ_27jun2024_v0.csv"
  ),
  read_smoke_ff10,
  n_skip_rows = 19,
  out_directory = "_transportation/data-raw/epa/air_emissions_modeling/air_emissions_modeling_mn_wi/onroad/"
)

plan(strategy = future::multisession, workers = 6)

furrr::future_map(
  c(
    "_transportation/data-raw/epa/air_emissions_modeling/2011/2011el_cb6v2_v6_11g/inputs/onroad/2011el_onroad_SMOKE_MOVES_MOVES2014a_forOTAQ_31aug2016_v1_part1.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2014/2014fd_cb6_14j/inputs/onroad/2014fd_nata_onroad_SMOKE_MOVES_MOVES2014a_AQstyle_06feb2018_v0.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2015/2015fd_cb6_15j/inputs/onroad/2015fd_onroad_SMOKE_MOVES_MOVES2014a_AQstyle_28sep2018_nf_v1.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2016/2016gf_16j/inputs/onroad/2016gf_onroad_SMOKE_MOVES_MOVES3_30jun2022_v0.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2017/2017gb_17j/inputs/onroad/2017gb_nata_onroad_SMOKE_MOVES_NATAstyle_14may2020_v0.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2018/2018gc_cb6_18j/inputs/onroad/2018gc_SMOKE_MOVES_MOVES3_AQ_fullHAP_29sep2021_v1.csv"
  ),
  read_smoke_ff10,
  n_skip_rows = 19,
  out_directory = "_transportation/data-raw/epa/air_emissions_modeling/air_emissions_modeling_mn_wi/onroad/"
) 

# read back in, combine, and save -----
onroad_mn_wi <- purrr::map(
  list.files("_transportation/data-raw/epa/air_emissions_modeling/air_emissions_modeling_mn_wi/onroad/", full.names = TRUE),
  readRDS) %>% 
  bind_rows()

saveRDS(onroad_mn_wi, "_transportation/data-raw/epa/air_emissions_modeling/onroad_mn_wi.RDS")
tictoc::toc()

# optional plotting

onroad_mn_wi %>% 
  mutate(geoid = region_cd) %>%
  left_join(counties_light) %>%
  filter(cprg_area == TRUE,
         emis_type == "RPD") %>% 
  left_join(scc_combine) %>% 
  group_by(calc_year, alt_mode_truck, poll) %>% 
  summarise(ann_value = sum(ann_value, na.rm = T)) %>% 
  # select(calc_year, alt_mode_truck, poll) %>% 
  unique() 
