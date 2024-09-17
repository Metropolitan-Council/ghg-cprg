
source("R/_load_pkgs.R")
source("_meta/data-raw/county_geography.R")
source("_transportation/data-raw/_read_smoke_ff10.R")
library(furrr)
tictoc::tic("NEI SMOKE flat file proccessing")

# check that we have all the necessary files -----
if(any(purrr::map(
  c(
    "_transportation/data-raw/epa/nei/2020NEI/SmokeFlatFile_ONROAD_20230330.csv",
    # 2017, 2014, and 2011 NEI references the air emissions modeling platforms for Smoke flat files
    "_transportation/data-raw/epa/air_emissions_modeling/2017/2017gb_17j/inputs/onroad/2017gb_nata_onroad_SMOKE_MOVES_NATAstyle_14may2020_v0.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2014/2014fd_cb6_14j/inputs/onroad/2014fd_nata_onroad_SMOKE_MOVES_MOVES2014a_AQstyle_06feb2018_v0.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2011/2011el_cb6v2_v6_11g/inputs/onroad/2011el_onroad_SMOKE_MOVES_MOVES2014a_forOTAQ_31aug2016_v1_part1.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2011/2011el_cb6v2_v6_11g/inputs/onroad/2011el_onroad_SMOKE_MOVES_MOVES2014a_forOTAQ_31aug2016_v1_part2.csv"
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
plan(strategy = future::multisession, workers = 5)

furrr::future_map(
  c(
    "_transportation/data-raw/epa/nei/2020NEI/SmokeFlatFile_ONROAD_20230330.csv",
    
    # 2017, 2014, and 2011 NEI references the air emissions modeling platforms for Smoke flat files
    # 2017 has two options
    "_transportation/data-raw/epa/nei/2017NEI/home/callen05/tmp/2017NEI_onroad_SMOKE_MOVES_NEIstyle_09mar2020_FIPS_27.csv",
    "_transportation/data-raw/epa/nei/2017NEI/home/callen05/tmp/2017NEI_onroad_SMOKE_MOVES_NEIstyle_09mar2020_FIPS_55.csv",
    # "_transportation/data-raw/epa/air_emissions_modeling/2017/2017gb_17j/inputs/onroad/2017gb_nata_onroad_SMOKE_MOVES_NATAstyle_14may2020_v0.csv",
    # 
    # 2014 has two options
    # "_transportation/data-raw/epa/nei/2014NEI/SmokeFlatFile_ONROAD_20160910.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2014/2014fd_cb6_14j/inputs/onroad/2014fd_nata_onroad_SMOKE_MOVES_MOVES2014a_AQstyle_06feb2018_v0.csv",
    
    # 2011 has two options
    # "_transportation/data-raw/epa/air_emissions_modeling/2011/2011ek_onroad_SMOKE_MOVES_MOVES2014a_forOTAQ_21jan2016_v2_part1.csv",
    # "_transportation/data-raw/epa/air_emissions_modeling/2011/2011ek_onroad_SMOKE_MOVES_MOVES2014a_forOTAQ_21jan2016_v2_part2.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2011/2011el_cb6v2_v6_11g/inputs/onroad/2011el_onroad_SMOKE_MOVES_MOVES2014a_forOTAQ_31aug2016_v1_part1.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2011/2011el_cb6v2_v6_11g/inputs/onroad/2011el_onroad_SMOKE_MOVES_MOVES2014a_forOTAQ_31aug2016_v1_part2.csv"
  ),
  read_smoke_ff10,
  n_skip_rows = 13,
  out_directory = "_transportation/data-raw/epa/nei/SmokeFlatFile_MN_WI//"
) 



# read back in, combine, and save -----
nei_smoke_ff <- purrr::map(
  list.files("_transportation/data-raw/epa/nei/SmokeFlatFile_MN_WI/", 
             full.names = TRUE),
  readRDS) %>% 
  bind_rows() %>% 
  unique()

saveRDS(nei_smoke_ff, "_transportation/data-raw/epa/nei/epa_nei_smoke_ff.RDS")
tictoc::toc()

