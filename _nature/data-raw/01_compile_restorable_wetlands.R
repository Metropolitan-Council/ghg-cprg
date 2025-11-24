rm(list = ls())
source("R/_load_pkgs.R")
source("R/cprg_colors.R")

overwrite_RDS <- TRUE


# load the county and ctu boundaries layer
cprg_county_orig <- readRDS("_meta/data/cprg_county.RDS")
cprg_ctu_orig <- readRDS("_meta/data/cprg_ctu.RDS")

# convert the counties and ctu vector to a terra object
cprg_county <- terra::vect(cprg_county_orig)
cprg_ctu <- terra::vect(cprg_ctu_orig)

# define CRS for using with other layers
crs_use <- terra::crs(cprg_county)
cprg_ctu <- terra::project(cprg_ctu, crs_use)

# turn your county and ctu layers into dataframes
cprg_county_df <- cprg_county %>%
  as.data.frame() %>%
  select(geoid, county_name, state_name) %>%
  st_drop_geometry()

cprg_ctu_df <- cprg_ctu %>%
  as.data.frame() %>%
  select(gnis, geoid_wis, ctu_name, ctu_class, county_name, state_name) %>%
  st_drop_geometry() %>%
  mutate(
    geoid_wis = as.numeric(geoid_wis),
    gnis = as.numeric(gnis)
  )



inpath_wetlands_raster_all <- paste0(here::here(), "/_nature/data-raw/mn_wetlands/mn_restorable_wetland_index.tif")

wetlands_raster_all <- terra::rast(inpath_wetlands_raster_all) %>%
  terra::project(., crs_use, method = "near")

wetlands_raster_cprg <- wetlands_raster_all %>%
  terra::crop(., cprg_county) %>%
  terra::mask(., cprg_county)


# reclassify any pixels with value 1, 2, or 3 to NA; 4 and 5 become 1
wetlands_raster_cprg <- terra::classify(
  wetlands_raster_cprg,
  rcl = matrix(
    c(
      1, NA,
      2, NA,
      3, NA,
      4, 1,
      5, 1
    ),
    ncol = 2,
    byrow = TRUE
  )
)


# # plot 
# terra::plot(wetlands_raster_cprg, col = c("lightblue", "blue"), legend = FALSE)

# save object for later plotting
saveRDS(wetlands_raster_cprg, "./_nature/data/mn_wetlands_msa.rds")



