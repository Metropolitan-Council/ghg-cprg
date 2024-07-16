source("R/_load_pkgs.R")
source("R/cprg_colors.R")

cprg_county <- readRDS("_meta/data/cprg_county.RDS") %>% 
  filter(!NAME %in% c("Chisago","Sherburne","St. Croix","Pierce"))

county_lc <- readRDS("_nature/data/nlcd_county_landcover_2001_2021.rds") %>% 
  filter(!county %in% c("Chisago","Sherburne","St. Croix","Pierce"),
         year == 2021)

### get Mpls, St Paul, and BLoomington shapefiles

cities <- councilR::import_from_gpkg("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_dot/bdry_mn_city_township_unorg/gpkg_bdry_mn_city_township_unorg.zip") %>%
  filter(COUNTY_NAM %in% c(cprg_county$NAME)) %>%
  mutate(
    STATEFP = "27",
    STATE = "Minnesota",
    STATE_ABB = "MN"
  ) %>%
  select(
    CTU_NAME = FEATURE_NA,
    CTU_CLASS,
    COUNTY_NAM,
    STATEFP,
    STATE,
    STATE_ABB,
    GNIS_FEATU,
    geometry = geom
  ) %>%
  filter(CTU_NAME %in% c("Minneapolis", "Saint Paul", "Bloomington"))

# convert the counties vector to a terra object
cprg_county <- terra::vect(cprg_county)

# define CRS for using with other layers
crs_use <- terra::crs(cprg_county)

cities <- terra::vect(cities) %>% 
  project(., crs_use)

## load regional park shapefile

parks <- vect("./_nature/data-raw/DBO_RPTAdministrativeBoundary.shp")

parks_use <- parks %>% filter(Implementa == "Operational", !Category %in% c("Conservation Area", "County Park", "Natural Area")) 

### start with area calculations

# first remove minneapolis, bloomington, st paul from respective counties
areas <- bind_rows(
  left_join(data.frame(geog_name = cprg_county$NAME, total_area = expanse(cprg_county, unit="km")),
            data.frame(geog_name = cities$COUNTY_NAM, 
                      city_area = expanse(cities, unit="km")) %>% 
                        group_by(geog_name) %>% 
            summarize(city_area = sum(city_area))) %>% 
    mutate(city_area = replace_na(city_area, 0),
           geog_area = total_area - city_area) %>% 
    select(-c(total_area,city_area)),
    data.frame(geog_name = cities$CTU_NAME, geog_area = expanse(cities, unit="km"))
) %>% ### bring in park areas and merge to common geography
left_join(., 
    data.frame(agency = parks_use$AgencyMana, acres = parks_use$AcresCalcu / 247.1) %>% 
    group_by(agency) %>% 
    summarize(park_area = sum(acres))%>%
      mutate(geog_name = str_extract(agency, "^[^ ]+"),
             geog_name = if_else(geog_name == "Three", "Hennepin", geog_name),
             geog_name = if_else(geog_name == "Saint", "Saint Paul", geog_name)) %>% 
      select(geog_name, park_area)
           ) %>% 
  mutate(nonpark_area = geog_area - park_area)

nlcd_lc <- get_nlcd(
  parks,
  "county",
  year = 2021,
  dataset = "landcover"
) %>%
  project(., crs_use) %>% # reproject
  resample(., wc_parks) # resample nlcd raster to match more granular ESA World Cover raster

nlcd_is <- get_nlcd(
  parks,
  "county",
  year = 2021,
  dataset = "impervious"
) %>%
  project(., crs_use) %>%
  resample(., wc_parks)

nlcd_tcc <- FedData::get_nlcd( # use FedData::get_nlcd() to load nlcd data
  cprg_county, # area to sample over
  "county", # label for current selection
  year = year, # the current year
  dataset = "canopy" # downloads tree canopy cover data only
) %>%
  terra::project(., crs_use), silent = TRUE)


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

#saveRDS(wc_parks, "./_nature/data/worldcover_parks.rds")

# calculate the area of each pixel (raster cells are skewed at higher latitudes)
area_wc <- mask(cellSize(wc_parks, unit = "km"), parks)

# rasterize cprg_county to assign pixels to counties
parks_raster <- rasterize(parks, wc_parks, field = "AgencyMana")
parks_implementa_raster <- rasterize(parks, wc_parks, field = "Implementa")
county_raster <- rasterize(cprg_county, parks_raster, field = "NAME")



# nicer plot - move to qmd later
# ggplot() + geom_spatraster(data= wc_factor) +
#   scale_fill_manual(breaks = code_class_tab$old_lab,
#                     labels = code_class_tab$new_lab,
#                     values=esa_color,
#                     na.value="white",
#                     name = 'World Cover Class') + theme_void()

### load nlcd land cover and impervious surface data and match to WC data



# convert all rasters to data.frame

#### convert all rasters to data.frame. These are memory intensive steps for full county area.
nlcdlc_values <- extract(nlcd_lc, parks)
nlcdis_values <- extract(nlcd_is, parks)
# NAs to 0s
nlcdis_values[is.na(nlcdis_values$county_NLCD_Impervious_2021), ]$county_NLCD_Impervious_2021 <- 0

area_values <- extract(area_wc, parks)
wc_values <- extract(wc_parks, parks)
cty_values <- extract(county_raster, parks)
parks_values <- extract(parks_raster, parks)
parks_implementa_values <- extract(parks_implementa_raster, parks)

#### combine raster values into one dataframe
parks_df <- data.frame(
  county = cty_values[, 2],
  park = parks_values[, 2],
  park_implementa = parks_implementa_values[, 2],
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
  group_by(park, land_cover_type,park_implementa) %>%
  summarize(area = sum(area_corrected))

# create metadata
wc_parks_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "park ", class(wc_parks$park), "Park Implementing Agency",
    "land_cover_type", class(wc_parks$land_cover_type), "Land cover type from World Cover. 'Urban_' indicates a natural area within NLCD designated developed land cover",
    "park_implementa", class(wc_parks$park_implementa), "Park land acquisition status - not all categories finalized",
    "area", class(wc_parks$area), "Area of land cover in square kilometers. 'Urban_Tree' is scaled down by NLCD percent impervious"
  )

saveRDS(wc_parks, "./_nature/data/park_landcover_draft_2021.rds")
saveRDS(wc_parks_meta, "./_nature/data/park_landcover_draft_2021_meta.rds")
