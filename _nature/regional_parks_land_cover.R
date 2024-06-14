source("R/_load_pkgs.R")
source("R/cprg_colors.R")

cprg_county <- readRDS("_meta/data/cprg_county.RDS")

## load ESA World Cover rasters
wc1 <- rast("./_nature/data-raw/ESA_WorldCover_10m_2021_v200_N42W093_Map.tif")
wc2 <- rast("./_nature/data-raw/ESA_WorldCover_10m_2021_v200_N42W096_Map.tif")
wc3 <- rast("./_nature/data-raw/ESA_WorldCover_10m_2021_v200_N45W093_Map.tif")
wc4 <- rast("./_nature/data-raw/ESA_WorldCover_10m_2021_v200_N45W096_Map.tif")

## load regional park shapefile

parks <- vect("./_nature/data-raw/DBO_RPTAdministrativeBoundary.shp")

saveRDS(parks, "./_nature/data/regional_parks_shape.rds")

### create class conversion table (couldn't get ESA to output this info which would be cleaner)
code_class_tab <- data.frame(
  old_lab = c(10, 20, 30, 40, 50, 60, 70, 80, 90),
  esa_label = c("Tree", "Shrubland", "Grassland", "Cropland", "Built-up", "Bare", "Snow", "Water", "Wetland")
)

## reduce raster size before merging
wc_red_list <- lapply(paste0("wc", 1:4), function(x) terra::crop(get(x), cprg_county))
# merge raster to get full area
wc_combined <- do.call(mosaic, c(wc_red_list, fun = "min"))

# if you want to take a peek
# plot(wc_combined)

# define CRS for using with other layers
crs_use <- terra::crs(wc_combined)

# convert tigris data to SpatVector (terra) and reproject
cprg_county <- vect(cprg_county)
cprg_county <- project(cprg_county, crs_use)
parks <- project(parks, crs_use)

# remove non-county pixels
wc_parks <- mask(wc_combined, parks)
# check
# terra::plot(wc_outline)

saveRDS(wc_parks, "./_nature/data/worldcover_parks.rds")

# calculate the area of each pixel (raster cells are skewed at higher latitudes)
area_wc <- mask(cellSize(wc_outline, unit = "km"), parks)

# rasterize cprg_county to assign pixels to counties
parks_raster <- rasterize(parks, wc_outline, field = "AgencyMana")
parks_cat_raster <- rasterize(parks, wc_outline, field = "Category")
county_raster <- rasterize(cprg_county, parks_raster, field = "NAME")



# nicer plot - move to qmd later
# ggplot() + geom_spatraster(data= wc_factor) +
#   scale_fill_manual(breaks = code_class_tab$old_lab,
#                     labels = code_class_tab$new_lab,
#                     values=esa_color,
#                     na.value="white",
#                     name = 'World Cover Class') + theme_void()

### load nlcd land cover and impervious surface data and match to WC data

nlcd_lc <- get_nlcd(
  parks,
  "county",
  year = 2021,
  dataset = "landcover"
) %>%
  project(., crs_use) %>% # reproject
  resample(., wc_outline) # resample nlcd raster to match more granular ESA World Cover raster

nlcd_is <- get_nlcd(
  parks,
  "county",
  year = 2021,
  dataset = "impervious"
) %>%
  project(., crs_use) %>%
  resample(., wc_outline)



# convert all rasters to data.frame

#### convert all rasters to data.frame. These are memory intensive steps for full county area.
nlcdlc_values <- extract(nlcd_lc, parks)
nlcdis_values <- extract(nlcd_is, parks)
# NAs to 0s
nlcdis_values[is.na(nlcdis_values$county_NLCD_Impervious_2021), ]$county_NLCD_Impervious_2021 <- 0

area_values <- extract(area_wc, parks)
wc_values <- extract(wc_outline, parks)
cty_values <- extract(county_raster, parks)
parks_values <- extract(parks_raster, parks)
parks_cat_values <- extract(parks_cat_raster, parks)

#### combine raster values into one dataframe
parks_df <- data.frame(
  county = cty_values[, 2],
  park = parks_values[, 2],
  park_type = parks_cat_values[, 2],
  nlcd_cover = nlcdlc_values[, 2],
  impervious_cover = as.numeric(as.character(nlcdis_values[, 2])),
  area = area_values[, 2],
  wc_lab = wc_values[, 2]
) %>%
  left_join(., code_class_tab, by = c("wc_lab" = "old_lab")) %>% # join in ESA WC land class labels
  mutate(
    area_corrected = if_else(esa_label == "Tree", area * (1 - (impervious_cover / 100)), area), # downscale trees by inverse of % impervious
    land_cover_type = if_else(grepl("Developed", nlcd_cover) & esa_label %in% c("Grassland", "Cropland", "Tree"),
      paste0("Urban_", esa_label),
      esa_label
    )
  ) # relabel natural areas in developed zones as urban_ as these likely have diminished C sequestration potential

# reduce data frame to sum of all pixels (i.e. area) of designated land class system within each county
wc_parks <- parks_df %>%
  group_by(park, land_cover_type) %>%
  summarize(area = sum(area_corrected))

# create metadata
wc_parks_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "park ", class(wc_parks$county), "Park Implementing Agency",
    "land_cover_type", class(wc_parks$land_cover_type), "Land cover type from World Cover. 'Urban_' indicates a natural area within NLCD designated developed land cover",
    "area", class(wc_parks$area), "Area of land cover in square kilometers. 'Urban_Tree' is scaled down by NLCD percent impervious"
  )

saveRDS(wc_parks, "./_nature/data/park_landcover_2021.rds")
saveRDS(wc_parks_meta, "./_nature/data/park_landcover_2021_meta.rds")
