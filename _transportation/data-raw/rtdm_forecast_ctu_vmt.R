# using outputs from our regional travel demand model,
# create a VMT forecast for each CTU and county in the region.
# The core work for this data exists in another repository. 
# Contact us for more detail.
# 
source("R/_load_pkgs.R")
cprg_ctu <- readRDS("_meta/data/cprg_ctu.RDS")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")

ctu_vmt_forecast <- readRDS("_transportation/data-raw/metc_travel_model/ctu_vmt_forecast.RDS")

county_vmt_forecast <- readRDS("_transportation/data-raw/metc_travel_model/county_vmt_forecast.RDS")
