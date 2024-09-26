# EQUATES: EPA’s Air QUAlity TimE Series Project
# https://gaftp.epa.gov/Air/emismod/MOVES3/CONUS/
# EPA scientists have developed a set of modeled meteorology, emissions,
# air quality and pollutant deposition spanning the years 2002 through 2019.

tictoc::tic(msg = "EQUATES processing")
source("R/_load_pkgs.R")
source("_meta/data-raw/county_geography.R")
source("_transportation/data-raw/_read_smoke_ff10.R")
library(furrr)

# check that we have all the necessary files -----
if (any(purrr::map(
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
  file_exists
) == FALSE)) {
  cli::cli_abort(c(
    "Required datasets unavailable",
    "*" = "Consult documentation for more information",
    "*" = "{.file _transportation/data-raw/epa/README_epa_downloads.html}"
  ))
}



# number of workers should match number of items in the vector
plan(strategy = future::multisession, workers = 12)

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
  read_smoke_ff10,
  out_directory = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/"
)

plan(strategy = future::multisession, workers = 12)


# read back in, combine, and save -----
equates <- purrr::map(
  list.files("_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/",
    recursive = FALSE,
    pattern = ".RDS",
    full.names = TRUE
  ),
  readRDS
) %>%
  bind_rows()

saveRDS(equates, "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/equates_mn_wi.RDS")

# save CMAS data warehouse versions -----
# these are all from the the CMAS Data Warehouse Google Drive
# See CMAS Forum post for more detail: https://forum.cmascenter.org/t/nitrous-oxide-n2o-availability-in-equates-county-level/5199/4
# VOC spec data do not contain any relevant pollutants and so are not included here
equates_cmas_data_warehouse <- c(
  "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/CMAS_Data_Warehouse/EQUATES_2002/inputs/onroad_inv_diesel//diesel_MYR_2002_SMOKE_MOVES_MOVES3_AQstyle_06jan2021_v0.csv",
  "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/CMAS_Data_Warehouse/EQUATES_2002/inputs/onroad_inv_gas//gas_MYR_2002_SMOKE_MOVES_MOVES3_AQstyle_28may2021_nf_v1.csv",
  "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/CMAS_Data_Warehouse/EQUATES_2005/inputs/onroad_inv_diesel//diesel_MYR_2005_SMOKE_MOVES_MOVES3_AQstyle_13jan2021_v0.csv",
  "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/CMAS_Data_Warehouse/EQUATES_2005/inputs/onroad_inv_gas/gas_MYR_2005_SMOKE_MOVES_MOVES3_AQstyle_13jan2021_v0.csv",
  "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/CMAS_Data_Warehouse/EQUATES_2008/inputs/onroad_inv_diesel//diesel_MYR_2008_SMOKE_MOVES_MOVES3_AQstyle_30oct2020_v0.csv",
  "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/CMAS_Data_Warehouse/EQUATES_2008/inputs/onroad_inv_gas//gas_MYR_2008_SMOKE_MOVES_MOVES3_AQstyle_30oct2020_v0.csv",
  "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/CMAS_Data_Warehouse/EQUATES_2011/inputs/onroad_inv_diesel//diesel_MYR_2011_SMOKE_MOVES_MOVES3_AQstyle_20apr2021_nf_v1.csv",
  "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/CMAS_Data_Warehouse/EQUATES_2011/inputs/onroad_inv_gas//gas_MYR_2011_SMOKE_MOVES_MOVES3_AQstyle_20apr2021_nf_v1.csv",
  "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/CMAS_Data_Warehouse/EQUATES_2014/inputs/onroad_inv_diesel//diesel_MYR_2014_SMOKE_MOVES_MOVES3_AQstyle_23nov2020_v0.csv",
  "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/CMAS_Data_Warehouse/EQUATES_2014/inputs/onroad_inv_gas//gas_MYR_2014_SMOKE_MOVES_MOVES3_AQstyle_23nov2020_v0.csv",
  "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/CMAS_Data_Warehouse/EQUATES_2017/inputs/onroad_inv_diesel//diesel_MYR_2017_SMOKE_MOVES_MOVES3_AQstyle_15dec2020_v0.csv",
  "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/CMAS_Data_Warehouse/EQUATES_2017/inputs/onroad_inv_gas//gas_MYR_2017_SMOKE_MOVES_MOVES3_AQstyle_15dec2020_v0.csv",
  "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/CMAS_Data_Warehouse/EQUATES_2019/inputs/onroad_inv_diesel//diesel_MYR_EQUATES_2019_SMOKE_MOVES_MOVES3_AQstyle_18feb2022_v0.csv",
  "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/CMAS_Data_Warehouse/EQUATES_2019/inputs/onroad_inv_gas//gas_MYR_EQUATES_2019_SMOKE_MOVES_MOVES3_AQstyle_18feb2022_v0.csv"
)

if (any(purrr::map(
  equates_cmas_data_warehouse,
  file_exists
) == FALSE)) {
  cli::cli_abort(c(
    "Required datasets unavailable",
    "*" = "Consult documentation for more information",
    "*" = "{.file _transportation/data-raw/epa/README_epa_downloads.html}"
  ))
}


plan(strategy = future::multisession, workers = 7)

furrr::future_map(
  equates_cmas_data_warehouse[1:7],
  read_smoke_ff10,
  out_directory = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/cmas_data_warehouse/"
)

plan(strategy = future::multisession, workers = 7)

furrr::future_map(
  equates_cmas_data_warehouse[8:14],
  read_smoke_ff10,
  out_directory = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/cmas_data_warehouse/"
)

# save all outputs
purrr::map(
  list.files("_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/cmas_data_warehouse/",
    full.names = TRUE,
    recursive = FALSE
  ),
  readRDS
) %>%
  bind_rows() %>%
  unique() %>%
  saveRDS("_transportation/data-raw/epa/air_emissions_modeling/EQUATES/equates_cmas_mn_wi.RDS")


tictoc::toc()


# verify identical for FTP and Google Drive -----
purrr::map2(
  .x = c(
    # diesel
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/cmas_data_warehouse/diesel_MYR_2002_SMOKE_MOVES_MOVES3_AQstyle_06jan2021_v0.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/cmas_data_warehouse/diesel_MYR_2005_SMOKE_MOVES_MOVES3_AQstyle_13jan2021_v0.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/cmas_data_warehouse/diesel_MYR_2008_SMOKE_MOVES_MOVES3_AQstyle_30oct2020_v0.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/cmas_data_warehouse/diesel_MYR_2011_SMOKE_MOVES_MOVES3_AQstyle_20apr2021_nf_v1.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/cmas_data_warehouse/diesel_MYR_2014_SMOKE_MOVES_MOVES3_AQstyle_23nov2020_v0.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/cmas_data_warehouse/diesel_MYR_2017_SMOKE_MOVES_MOVES3_AQstyle_15dec2020_v0.RDS",

    # gasoline
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/cmas_data_warehouse/gas_MYR_2002_SMOKE_MOVES_MOVES3_AQstyle_28may2021_nf_v1.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/cmas_data_warehouse/gas_MYR_2005_SMOKE_MOVES_MOVES3_AQstyle_13jan2021_v0.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/cmas_data_warehouse/gas_MYR_2008_SMOKE_MOVES_MOVES3_AQstyle_30oct2020_v0.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/cmas_data_warehouse/gas_MYR_2011_SMOKE_MOVES_MOVES3_AQstyle_20apr2021_nf_v1.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/cmas_data_warehouse/gas_MYR_2014_SMOKE_MOVES_MOVES3_AQstyle_23nov2020_v0.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/cmas_data_warehouse/gas_MYR_2017_SMOKE_MOVES_MOVES3_AQstyle_15dec2020_v0.RDS"
  ),
  .y = c(
    # diesel
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/diesel_MYR_2002_SMOKE_MOVES_MOVES3_AQstyle_06jan2021_v0.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/diesel_MYR_2005_SMOKE_MOVES_MOVES3_AQstyle_13jan2021_v0.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/diesel_MYR_2008_SMOKE_MOVES_MOVES3_AQstyle_30oct2020_v0.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/diesel_MYR_2011_SMOKE_MOVES_MOVES3_AQstyle_20apr2021_nf_v1.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/diesel_MYR_2014_SMOKE_MOVES_MOVES3_AQstyle_23nov2020_v0.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/diesel_MYR_2017_SMOKE_MOVES_MOVES3_AQstyle_15dec2020_v0.RDS",

    # gas
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/gas_MYR_2002_SMOKE_MOVES_MOVES3_AQstyle_28may2021_nf_v1.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/gas_MYR_2005_SMOKE_MOVES_MOVES3_AQstyle_13jan2021_v0.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/gas_MYR_2008_SMOKE_MOVES_MOVES3_AQstyle_30oct2020_v0.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/gas_MYR_2011_SMOKE_MOVES_MOVES3_AQstyle_20apr2021_nf_v1.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/gas_MYR_2014_SMOKE_MOVES_MOVES3_AQstyle_23nov2020_v0.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/gas_MYR_2017_SMOKE_MOVES_MOVES3_AQstyle_15dec2020_v0.RDS"
  ),
  function(file1, file2) {
    waldo::compare(
      readRDS(file1) %>%
        select(-file_location),
      readRDS(file2) %>%
        select(-file_location)
    )
  }
)
