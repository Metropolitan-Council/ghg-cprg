rm(list = ls())
source("R/_load_pkgs.R")
source("R/cprg_colors.R")

overwrite_RDS <- TRUE


# load the county and ctu boundaries layer
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_ctu <- readRDS("_meta/data/cprg_ctu.RDS")

# convert the counties and ctu vector to a terra object
cprg_county <- terra::vect(cprg_county)
cprg_ctu <- terra::vect(cprg_ctu)



sf_cprg_county <- st_as_sf(cprg_county)
st_write(sf_cprg_county, "~/SSURGO Data/cprg_county.shp")



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




tmp_path <- "~/SSURGO Data/"

ssurgo_sp <-  sf::st_read(paste0(tmp_path, "cprg_soilTaxOrder2.shp"))# %>% sf::st_transform(., "EPSG:26915")

ssurgo_vect <- terra::vect(ssurgo_sp)


plot(ssurgo_vect)






