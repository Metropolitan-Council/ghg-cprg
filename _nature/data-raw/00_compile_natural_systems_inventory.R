rm(list = ls())

# 01 ----------------------------------------------------------------------
# 01_compile_land_cover_carbon.R
# Compiles sequestration rates by land cover type from peer-reviewed literature
source("_nature/data-raw/01_compile_land_cover_carbon.R")

# 01_compile_waterways_carbon.R
# Compiles methane emissions factors by waterway type from MPCA estimates
source("_nature/data-raw/01_compile_waterways_carbon.R")



# 02 ----------------------------------------------------------------------
# 02_compile_nlcd_land_cover.R
# Takes annual NLCD images and estimates land area by land cover type
# NOTE: this is memory and time-intensive
source("_nature/data-raw/02_compile_nlcd_land_cover.R")

# 02_compile_nhd_waterways.R
# Use National Hydrography Dataset to compile waterways for the region
# Compile NHD waterways by County (this is slow)
source("_nature/data-raw/02_compile_nhd_waterways_byCounty.R")
# Compile NHD waterways by CTU (this is VERY slow)
source("_nature/data-raw/02_compile_nhd_waterways_byCTU.R")


# 03 ----------------------------------------------------------------------
# 03_compile_extrapolated_nlcd_land_cover.R
# Computes the area of Urban_Tree and Urban_Grassland as a function of 
# total developed area during periods where tree canopy data is not available
source("_nature/data-raw/03_compile_extrapolated_nlcd_land_cover.R")

# 03_compile_waterways_emissions_from_nhd.R
# Applies emissions factors to area estimates of waterway type to summarize
# CH4 emissions at the county and ctu scale
source("_nature/data-raw/03_compile_waterways_emissions_from_nhd.R")


# 04 ----------------------------------------------------------------------
# 04_compile_land_area_sequestration_from_nlcd.R
# Applies sequestration rates to area estimates of land cover type to summarize
# C sequestration at the county and ctu scale
source("_nature/data-raw/04_compile_land_area_sequestration_from_nlcd.R")




