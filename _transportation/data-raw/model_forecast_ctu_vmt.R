# using outputs from our regional travel demand model,
# create a VMT forecast for each CTU and county in the region.
# The core work for this data exists in another repository. 
# Contact us for more detail.
# 
source("R/_load_pkgs.R")

ctu_vmt_forecast <- readRDS("_transportation/data-raw/metc_travel_model/ctu_vmt_forecast.RDS")
county_vmt_forecast <- readRDS("_transportation/data-raw/metc_travel_model/county_vmt_forecast.RDS")

mndot_vmt_ctu <- readRDS("_transportation/data/mndot_vmt_ctu.RDS")
mndot_vmt_county <- readRDS("_transportation/data/dot_vmt.RDS") %>% 
  filter(data_source == "MnDOT")

# find the proportion of 
