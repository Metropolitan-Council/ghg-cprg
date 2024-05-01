source("R/_load_pkgs.R")
source("R/global_warming_potential.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")

library(FedData)
library(terra)
#install.packages('tidyterra')
library(tidyterra)
library(ggplot)

## load ESA World Cover rasters
wc1 <- rast('./_nature/data-raw/ESA_WorldCover_10m_2021_v200_N42W093_Map.tif')
wc2 <- rast('./_nature/data-raw/ESA_WorldCover_10m_2021_v200_N42W096_Map.tif')
wc3 <- rast('./_nature/data-raw/ESA_WorldCover_10m_2021_v200_N45W093_Map.tif')
wc4 <- rast('./_nature/data-raw/ESA_WorldCover_10m_2021_v200_N45W096_Map.tif')

### create class conversion table (couldn't get ESA to output this info which would be cleaner)
code_class_tab <- data.frame(old_lab = factor(c(10, 20, 30, 40, 50, 60, 70, 80, 90)),
                             new_lab = c('Tree','Shrubland','Grassland','Cropland','Built-up','Bare','Snow','Water','Wetland'))
#create color palette for mapping - this may be exported to qmd files later
esa_color <- c('forestgreen','brown4','yellow2','wheat3','red','plum','lightblue','navy','mediumturquoise')


## reduce raster size before merging
wc_red_list <- lapply(paste0("wc", 1:4), function(x) terra::crop(get(x), cprg_county))
#merge raster to get full area
wc_combined <- do.call(mosaic, c(wc_red_list, fun = 'min'))

# if you want to take a peek
#plot(wc_combined)

# define CRS for using with other layers
crs_use <- terra::crs(wc_combined)

#convert tigris data to SpatVector (terra) and reproject
cprg_county <- vect(cprg_county)
cprg_county <- project(cprg_county, crs_use)

# remove non-county pixels
wc_outline <- mask(wc_combined, cprg_county)
#check
#terra::plot(wc_outline)

#turn raster into factors to avoid weird warping of raster values and for plotting
wc_factor <- as.factor(wc_outline)

#nicer plot - move to qmd later
# ggplot() + geom_spatraster(data= wc_factor) +
#   scale_fill_manual(breaks = code_class_tab$old_lab,
#                     labels = code_class_tab$new_lab,
#                     values=esa_color,
#                     na.value="white",
#                     name = 'World Cover Class') + theme_void()

### load nlcd impervious surface data
