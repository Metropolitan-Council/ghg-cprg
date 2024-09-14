
source("R/_load_pkgs.R")
source("_meta/data-raw/county_geography.R")
source("_transportation/data-raw/_read_smoke_moves.R")
library(furrr)
tictoc::tic("NEI SMOKE flat file proccessing")

# check that we have all the necessary files -----
if(any(purrr::map(
  c("_transportation/data-raw/epa/nei/2020NEI/SmokeFlatFile_ONROAD_20230330.csv",
    "_transportation/data-raw/epa/nei/2017NEI/2017NEI_onroad_20200427/2017NEI_onroad_CONUS_noCalifornia.csv",
    "_transportation/data-raw/epa/nei/2014NEI/SmokeFlatFile_ONROAD_20160910.csv"
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
    "_transportation/data-raw/epa/nei/2020NEI/SmokeFlatFile_ONROAD_20230330.csv",
    "_transportation/data-raw/epa/nei/2017NEI/home/callen05/tmp/2017NEI_onroad_SMOKE_MOVES_NEIstyle_09mar2020_FIPS_27.csv",
    "_transportation/data-raw/epa/nei/2017NEI/home 2/callen05/tmp/2017NEI_onroad_SMOKE_MOVES_NEIstyle_09mar2020_FIPS_55.csv",
    # "_transportation/data-raw/epa/nei/2017NEI/2017NEI_onroad_20200427/2017NEI_onroad_CONUS_noCalifornia.csv",
    "_transportation/data-raw/epa/nei/2014NEI/SmokeFlatFile_ONROAD_20160910.csv"
  ),
  read_smoke_moves,
  n_skip_rows = 13,
  out_directory = "_transportation/data-raw/epa/nei/SmokeFlatFile_MN_WI//"
) 


# read back in, combine, and save -----
nei_smoke_ff <- purrr::map(
  list.files("_transportation/data-raw/epa/nei/SmokeFlatFile_MN_WI/", 
             full.names = TRUE),
  readRDS) %>% 
  bind_rows()

saveRDS(nei_smoke_ff, "_transportation/data-raw/epa/nei/epa_nei_smoke_ff.RDS")
tictoc::toc()

