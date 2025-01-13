rm(list = ls())

# 01_compile_land_cover_carbon.R
# Compiles sequestration rates by land cover type from peer-reviewed literature
source("_nature/data-raw/01_compile_land_cover_carbon.R")

# 02_compile_nlcd_land_cover.R
# Takes annual NLCD images and estimates land area by land cover type
# Note this is memory and time-intensive
source("_nature/data-raw/02_compile_nlcd_land_cover.R")

# 03_compile_land_area_sequestration_from_nlcd.R
# Applies sequestration rates to area estimates of land cover type to summarize
# C sequestration at the county and ctu scale
source("_nature/data-raw/03_compile_land_area_sequestration_from_nlcd.R")
