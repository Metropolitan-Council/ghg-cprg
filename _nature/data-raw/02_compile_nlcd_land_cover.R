rm(list = ls())
source("R/_load_pkgs.R")
source("R/cprg_colors.R")

overwrite_RDS <- TRUE

# import natural systems cover types
land_cover_carbon <- readRDS("_nature/data/land_cover_carbon.rds")


# Goal is to take annual land cover maps from NLCD and derive area estimates
# for different natural systems (e.g. forests, grasslands, wetlands).
# We can then aggregate area to different geographic boundaries (CTUs, counties).


# load the county and ctu boundaries layer
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_ctu <- readRDS("_meta/data/cprg_ctu.RDS")

# convert the counties and ctu vector to a terra object
cprg_county <- terra::vect(cprg_county)
cprg_ctu <- terra::vect(cprg_ctu)

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


# inpath_histosols <- "./_nature/data-raw/shapefiles/"
# histosols_sp <-  sf::st_read(paste0(inpath_histosols, "shp_geos_potentially_wet_histosols/potentially_wet_histosols.shp")) %>% 
#   sf::st_transform(., crs_use)
# 
# histosols_sp <- histosols_sp %>% sf::st_make_valid()
# 
# histosols_sp <- histosols_sp %>%
#   sf::st_crop(cprg_ctu) %>%
#   sf::st_make_valid()
# 
# # histosols_sp %>%
# #   sf::st_intersection(x) %>%
# #   sf::st_make_valid()
# 
# ggplot(cprg_county) +   geom_sf(color=NA, fill=NA, lwd=0) +
#   
#  
#   councilR::theme_council_geo() +
#   
#   geom_sf(data= cprg_county, color="gray65", fill=NA, lwd=0.4) +
#   ggnewscale::new_scale_fill() + ## geoms added after this will use a new scale definition
#   
#   geom_sf(data = histosols_sp, aes(fill=wetland_ty), color="gray80", alpha=0.8,  lwd=0.2)  +
#   
#   
#   
#   theme(
#     legend.position = c(0,1),
#     legend.direction = "vertical",
#     legend.justification = c("left","top"),
#     legend.margin = margin(0, 0, 0, 0),
#     legend.key.height = unit(0.4, "cm"),
#     legend.key.width = unit(0.4, "cm"),
#     legend.box = "vertical",
#     legend.box.just = "left",
#     legend.spacing = unit(-0.28, "cm")
#   )




# Set inpath for nlcd raw images
inpath_nlcd <- "./_nature/data-raw/nlcd_raw_imgs"
# List all files in the NLCD directory
nlcd_files_all <- list.files(inpath_nlcd, full.names = TRUE)

# Grab only the .tiff file names that match the string "Land_Cover"
nlcd_lc_files <- nlcd_files_all[grep("LndCov", nlcd_files_all)]

# Grab only the .tiff file names that match the string "tcc" (Tree Canopy Cover)
nlcd_tcc_files <- nlcd_files_all[grep("tcc", nlcd_files_all)]

# # Grab the file names for impervious surface fraction (FctImp) 
# # and impervious surface descriptor (ImpDsc)
# nlcd_isFct_files <- nlcd_files_all[grep("FctImp", nlcd_files_all)]
# nlcd_isDsc_files <- nlcd_files_all[grep("ImpDsc", nlcd_files_all)]


# grab nlcd legend
nlcd.legend <- FedData::pal_nlcd()

nlcd.legend <- nlcd.legend %>%
  mutate(Class = case_when(
    Class == "Developed, Open Space" ~ "Developed_Open",
    Class == "Developed, Low Intensity" ~ "Developed_Low",
    Class == "Developed, Medium Intensity" ~ "Developed_Med",
    Class == "Developed High Intensity" ~ "Developed_High",
    .default = Class
  ))




# Useful funs -------------------------------------------------------------
# query nlcd file names by year
get_year <- function(filename) {
  year <- as.numeric(sub(".*_(\\d{4})_.*", "\\1", filename))
  return(year)
}
# simple percentage bar function, useful for locating problems during big loops
pctBar <- function(percentage, msg = NULL) {
  if (percentage < 0 || percentage > 100) {
    stop("Percentage must be between 0 and 100")
  }
  
  total_length <- 20
  filled_length <- round(total_length * (percentage / 100))
  empty_length <- total_length - filled_length
  
  bar <- paste0("|", strrep("=", filled_length), strrep("_", empty_length), "|")
  message(paste0(sprintf("%3d%%  %s", percentage, bar), " ", msg))
}



# Loop thru years ---------------------------------------------------------
# Create empty dataframes to store the results
start_year <- 2001
end_year <- 2022


# by county
nlcd_county <- data.frame(
  geoid = as.numeric(),
  county_name = as.character(),
  state_name = as.character(),
  year = as.numeric(),
  land_cover_type = as.character(),
  area = as.numeric(),
  total_county_area = as.numeric(),
  tcc_available = as.logical()
)


# by ctu
nlcd_ctu <- data.frame(
  gnis = as.numeric(),
  geoid_wis = as.numeric(),
  ctu_name = as.character(),
  ctu_class = as.character(),
  county_name = as.character(),
  state_name = as.character(),
  year = as.numeric(),
  land_cover_type = as.character(),
  area = as.numeric(),
  total_ctu_area = as.numeric(),
  tcc_available = as.logical()
)


# Loop through all years of NLCD data and extract area estimates by land cover type and by county
lapply(start_year:end_year, function(year) {
  # browser()
  message(paste0("\nBeginning year: ", year, "\n"))
  pctBar(0, paste0("Loading NLCD land cover raster for ", year))
  
  # Test to see if the current year is in your list of lc files
  if (year %in% get_year(nlcd_lc_files)) {
    nlcd_lc <- terra::rast(nlcd_lc_files[get_year(nlcd_lc_files) %in% year])
    
    nlcd_lc <- nlcd_lc %>%
      # Reproject your data. Since the land cover dataset contains
      # discrete land cover types, use method="near" for nearest neighbor estimation
      terra::project(., crs_use, method = "near") %>%
      # Crop the raster to county boundary
      terra::crop(., cprg_county) %>%
      # Mask the raster with county boundary
      terra::mask(., cprg_county)
    # terra::plot(nlcd_lc)
    
    
    # # Load the Fractional Impervious Surface layer
    # # Continuous variable showing the fractional surface area of the map unit
    # # (pixel) that is covered with artificial substrate or structures
    # nlcd_isFct <- terra::rast(nlcd_isFct_files[get_year(nlcd_isFct_files) %in% year])
    # nlcd_isFct <- nlcd_isFct %>%
    #   terra::project(., crs_use) %>%
    #   terra::crop(., cprg_county) %>%
    #   terra::mask(., cprg_county)
    # # terra::plot(nlcd_isFct)
    # 
    # # Load the Impervious Surface Descriptor layer
    # # Contains values of 0, 1 and 2 which correspond "NA", "Roads" and "Urban, non-road"
    # # This may be helpful down the line (not sure)
    # nlcd_isDsc <- terra::rast(nlcd_isDsc_files[get_year(nlcd_isDsc_files) %in% year])
    # nlcd_isDsc <- nlcd_isDsc %>%
    #   # discrete land cover types, use method="near" for nearest neighbor estimation
    #   terra::project(., crs_use, method = "near") %>%
    #   terra::crop(., cprg_county) %>%
    #   terra::mask(., cprg_county)
    # # terra::plot(nlcd_isDsc)
    
    
  } else {
    # Move to the next year if the land cover type layer is missing
    message(paste0("\nSTOP! No land cover layers for ", year, ".\nMoving to next year..."))
    return(NULL)
  }
  pctBar(10, "Loading NLCD tree canopy raster (if available)")
  
  
  
  # Test to see if the current year is in your list of tcc files
  if (year %in% get_year(nlcd_tcc_files)) {
    # Indicate if the tree canopy cover layer was successfully retrieved
    tcc_available <- TRUE
    
    # grab the tree canopy cover file
    nlcd_tcc <- terra::rast(nlcd_tcc_files[get_year(nlcd_tcc_files) %in% year])
    
    # reproject and mask for the region
    nlcd_tcc <- nlcd_tcc %>%
      terra::project(., crs_use) %>%
      # Crop the raster to county boundary
      terra::crop(., cprg_county) %>%
      # Mask the raster with county boundary
      terra::mask(., cprg_county)
    
    
    
  } else {
    # Indicate if the tree canopy cover layer was successfully retrieved
    tcc_available <- FALSE
  }
  pctBar(20, "Determining pixel-wise area estimates")
  
  
  # Mask the cell size of nlcd_lc_mask with cprg_county (this will be used to calculate area)
  nlcd_lc_area <- terra::mask(cellSize(nlcd_lc, unit = "km"), cprg_county)
  # Rasterize cprg_ctu with nlcd_lc_mask using "county_name" field
  county_raster <- terra::rasterize(cprg_ctu, nlcd_lc, field = "county_name")
  pctBar(30, "Rasterizing vector layers")
  
  # Rasterize cprg_ctu with nlcd_lc_mask using "ctu_name" field
  ctu_raster <- terra::rasterize(cprg_ctu, nlcd_lc, field = "ctu_name")
  
  # Rasterize cprg_ctu with nlcd_lc_mask using "ctu_class" field
  ctu_class_raster <- terra::rasterize(cprg_ctu, nlcd_lc, field = "ctu_class")
  pctBar(40, "Extracting land cover values (this can take awhile)")
  
  # Next we'll build a dataframe containing rowwise information
  # at the raster pixel scale using terra::extract()
  # Extract values for land cover and area
  nlcd_lc_values <- terra::extract(nlcd_lc, cprg_county)
  area_values <- terra::extract(nlcd_lc_area, cprg_county)
  pctBar(50, "Extracting geographic boundary values (this can take awhile)")
  
  # Extract values for county and ctu information
  county_values <- terra::extract(county_raster, cprg_county)
  ctu_values <- terra::extract(ctu_raster, cprg_county)
  ctu_class_values <- terra::extract(ctu_class_raster, cprg_county)
  pctBar(60, "Finished extracting!")
  
  
  # # Extract values for impervious surface information
  # nlcd_isFct_values <- terra::extract(nlcd_isFct, cprg_county)
  # # Developed, Open Space      - Impervious surface is < 20% of cover
  # # Developed, Low Intensity   - Impervious surface is 20-49% of cover
  # # Developed, Med Intensity   - Impervious surface is 50-79% of cover
  # # Developed, High Intensity  - Impervious surface is 80-100% of cover
  # 
  # 
  # nlcd_isDsc <- resample(nlcd_isDsc, nlcd_lc, method = "near")
  # nlcd_isDsc_values <- terra::extract(nlcd_isDsc, cprg_county)
  # nlcd_isDsc_values <- nlcd_isDsc_values %>%
  #   modify_if(is.numeric, ~ replace_na(., 0))
  
  
  if (tcc_available) {
    # If tree data IS available -------------------------------------------
    pctBar(70, paste0("Tree canopy data available for ", year))
    nlcd_tcc <- resample(nlcd_tcc, nlcd_lc, method = "near")
    nlcd_tcc_values <- terra::extract(nlcd_tcc, cprg_county)
    nlcd_tcc_values <- nlcd_tcc_values %>%
      modify_if(is.numeric, ~ replace_na(., 0))
    
    lc_df <- as_tibble(data.frame(
      county_name = county_values[, 2],
      ctu_name = ctu_values[, 2],
      ctu_class = ctu_class_values[, 2],
      nlcd_cover = nlcd_lc_values[, 2],
      # isFct = nlcd_isFct_values[, 2],
      # isDsc = nlcd_isDsc_values[, 2],
      tree_canopy_cover = as.numeric(as.character(nlcd_tcc_values[, 2])),
      area = area_values[, 2]
    ))
    
    lc_df <- lc_df %>%
      left_join(cprg_county_df, by = join_by(county_name)) %>% # add county spatial info
      left_join(cprg_ctu_df, by = join_by(county_name, ctu_name, ctu_class, state_name)) %>% # add CTU spatial info
      left_join(nlcd.legend %>%
                  dplyr::select(ID, Class) %>%
                  rename(
                    nlcd_cover = ID,
                    nlcd_cover_class = Class
                  ), by = "nlcd_cover")
    
    lc_df <- lc_df %>%
      filter(!is.na(county_name))
    
    pctBar(80, "Recomputing area based on new land cover definitions")
    
    
    # browser()
    
    # Recompute area based on new land cover designations
    # Here we treat "Developed_Open" as "Urban_Grassland" if tree canopy cover is 0
    # If tree canopy cover is > 0, we treat "Developed" as "Urban_Tree"
    # If tree canopy cover is 0, we treat "Developed" as "Developed_Low/Med/High" (so long as it's not "Developed_Open")
    # The rest of the land cover types are pretty straightforward
    lc_rc <- lc_df %>%
      mutate(
        land_cover_type = case_when(
          grepl("Developed_Open", nlcd_cover_class) & tree_canopy_cover == 0 ~ "Urban_Grassland",
          grepl("Developed", nlcd_cover_class) & tree_canopy_cover > 0 ~ "Urban_Tree",
          grepl("Developed", nlcd_cover_class) & !grepl("Developed_Open", nlcd_cover_class) & tree_canopy_cover == 0 ~ nlcd_cover_class,
          grepl("Deciduous Forest", nlcd_cover_class) ~ "Tree",
          grepl("Evergreen Forest", nlcd_cover_class) ~ "Tree",
          grepl("Mixed Forest", nlcd_cover_class) ~ "Tree",
          grepl("Dwarf Scrub", nlcd_cover_class) ~ "Grassland", # Changed from Shrubland to Grassland +KS 20250331
          grepl("Shrub/Scrub", nlcd_cover_class) ~ "Grassland", # Changed from Shrubland to Grassland +KS 20250331
          grepl("Grassland/Herbaceous", nlcd_cover_class) ~ "Grassland",
          grepl("Sedge/Herbaceous", nlcd_cover_class) ~ "Grassland",
          grepl("Cultivated Crops", nlcd_cover_class) ~ "Cropland",
          grepl("Pasture/Hay", nlcd_cover_class) ~ "Cropland",
          grepl("Barren Land", nlcd_cover_class) ~ "Bare",
          grepl("Perennial Ice/Snow", nlcd_cover_class) ~ "Snow",
          grepl("Open Water", nlcd_cover_class) ~ "Water",
          grepl("Woody Wetlands", nlcd_cover_class) ~ "Tree", ## Changed from "Wetland"
          grepl("Emergent Herbaceous Wetlands", nlcd_cover_class) ~ "Wetland",
          .default = "CHECK"
        )
      ) %>%
      mutate(
        # Here we correct the area based on the tree canopy cover
        # When a pixel is classified as "Urban_Tree", we multiply the area by the tree canopy cover percentage
        area_corrected = if_else(land_cover_type == "Urban_Tree", area * (tree_canopy_cover / 100), area),
        # Since area corrected is a percentage of the land area that's occupied by tree canopy,
        # let's make a new column that accounts for the residual area (we'll call this Urban_Grass)
        area_residual = area - area_corrected
      )
    
    
    # Below we're going to take the residual area of Developed,Open Space
    # after applying tree_canopy percentage
    # Here we'll call this Urban_Grassland and then use rowbind to add this back
    # to the original dataset
    lc_rc_residual_grassland <- lc_rc %>%
      filter(area_residual > 0 & nlcd_cover_class == "Developed_Open") %>% 
      # pull(land_cover_type) %>% unique() # Check to make sure that Urban_Tree is the only class here
      mutate( # recode the land_cover_type as Urban_Grassland (the residual area will be used here)
        land_cover_type = "Urban_Grassland"
      ) %>%
      dplyr::select(-area_corrected) %>%
      rename(area_corrected = area_residual) %>%
      mutate(
        area_residual = NA,
        area = NA
      )
    
    
    # We still need to account for a little bit of area that gets missed due to filtering
    # In this case, we want the area of land after taking into account tree canopy area,
    # but in this case, the residual is Developed (not Urban_Grassland)
    lc_rc_residual_impervious <- lc_rc %>%
      filter(area_residual > 0 & nlcd_cover_class != "Developed_Open") %>%
      # pull(land_cover_type) %>% unique() # Check to make sure that Urban_Tree is the only class here
      mutate( # recode to the original cover class
        land_cover_type = nlcd_cover_class
      ) %>% # pull(land_cover_type) %>% unique() # Developed_Low, _Med, & _High are the only classes here
      dplyr::select(-area_corrected) %>%
      rename(area_corrected = area_residual) %>%
      mutate(
        area_residual = NA,
        area = NA
      )
    
    
    
    # Bind the residual grassland area to the main dataframe
    lc_rc_final <- rbind(lc_rc, lc_rc_residual_grassland, lc_rc_residual_impervious) 
    
    pctBar(90, "Tidying final data")
    
    # Summarize the area of each land cover type by county
    lc_county <- lc_rc_final %>%
      filter(!is.na(land_cover_type) & !is.na(county_name)) %>%
      group_by(county_name, state_name, land_cover_type) %>%
      summarize(area = sum(area_corrected), .groups = "keep") %>%
      ungroup() %>%
      group_by(county_name, state_name) %>%
      mutate(total_county_area = sum(area)) %>%
      mutate(year = year, .before = everything()) %>%
      left_join(cprg_county_df, by = join_by(county_name, state_name)) %>%
      mutate(tcc_available = .env$tcc_available)
    
    # Summarize the area of each land cover type by ctu
    lc_ctu <- lc_rc_final %>%
      filter(!is.na(land_cover_type) & !is.na(ctu_name)) %>%
      group_by(ctu_name, ctu_class, county_name, state_name, land_cover_type) %>%
      summarize(area = sum(area_corrected), .groups = "keep") %>%
      ungroup() %>%
      group_by(ctu_name, ctu_class, county_name, state_name) %>%
      mutate(total_ctu_area = sum(area)) %>%
      mutate(year = year, .before = everything()) %>%
      left_join(cprg_ctu_df, by = join_by(ctu_name, ctu_class, county_name, state_name)) %>%
      mutate(tcc_available = .env$tcc_available)
    
    
    # Add the results for the current year to the results dataframe
    nlcd_county <<- rbind(
      nlcd_county,
      lc_county %>%
        dplyr::select(
          geoid, county_name, state_name,
          year, land_cover_type,
          area, total_county_area, tcc_available
        )
    )
    
    nlcd_ctu <<- rbind(
      nlcd_ctu,
      lc_ctu %>%
        dplyr::select(
          gnis, geoid_wis, ctu_name, ctu_class,
          county_name, state_name,
          year, land_cover_type,
          area, total_ctu_area, tcc_available
        )
    )
    
    system(paste0("say 'Finished ",year,"'"))
    pctBar(100, "Done! Moving to next year...")
  } else {
    
    # ...if tree data NOT available -------------------------------------------
    pctBar(70, paste0("Tree canopy data NOT available for ", year))
    
    lc_df <- as_tibble(data.frame(
      county_name = county_values[, 2],
      ctu_name = ctu_values[, 2],
      ctu_class = ctu_class_values[, 2],
      nlcd_cover = nlcd_lc_values[, 2],
      # isFct = nlcd_isFct_values[, 2],
      # isDsc = nlcd_isDsc_values[, 2],
      tree_canopy_cover = as.numeric(NA),
      area = area_values[, 2]
    ))
    
    lc_df <- lc_df %>%
      left_join(cprg_county_df, by = join_by(county_name)) %>% # add county spatial info
      left_join(cprg_ctu_df, by = join_by(county_name, ctu_name, ctu_class, state_name)) %>% # add CTU spatial info
      left_join(nlcd.legend %>%
                  dplyr::select(ID, Class) %>%
                  rename(
                    nlcd_cover = ID,
                    nlcd_cover_class = Class
                  ), by = "nlcd_cover")
    
    lc_df <- lc_df %>%
      filter(!is.na(county_name))
    
    pctBar(80, "Recomputing area based on new land cover definitions")
    
    # browser()
    lc_rc <- lc_df %>%
      mutate(
        land_cover_type = case_when(
          grepl("Deciduous Forest", nlcd_cover_class) ~ "Tree",
          grepl("Evergreen Forest", nlcd_cover_class) ~ "Tree",
          grepl("Mixed Forest", nlcd_cover_class) ~ "Tree",
          grepl("Dwarf Scrub", nlcd_cover_class) ~ "Grassland", # Changed from Shrubland to Grassland +KS 20250331
          grepl("Shrub/Scrub", nlcd_cover_class) ~ "Grassland", # Changed from Shrubland to Grassland +KS 20250331
          grepl("Grassland/Herbaceous", nlcd_cover_class) ~ "Grassland",
          grepl("Sedge/Herbaceous", nlcd_cover_class) ~ "Grassland",
          grepl("Cultivated Crops", nlcd_cover_class) ~ "Cropland",
          grepl("Pasture/Hay", nlcd_cover_class) ~ "Cropland",
          grepl("Barren Land", nlcd_cover_class) ~ "Bare",
          grepl("Perennial Ice/Snow", nlcd_cover_class) ~ "Snow",
          grepl("Open Water", nlcd_cover_class) ~ "Water",
          grepl("Woody Wetlands", nlcd_cover_class) ~ "Tree", ## Changed from "Wetland"
          grepl("Emergent Herbaceous Wetlands", nlcd_cover_class) ~ "Wetland",
          # Everything else, keep the original designation
          .default = nlcd_cover_class
        )
      )
    
    
    
    pctBar(90, "Tidying final data")
    
    # Summarize the area of each land cover type by county
    lc_county <- lc_rc %>%
      filter(!is.na(land_cover_type) & !is.na(county_name)) %>%
      group_by(county_name, state_name, land_cover_type) %>%
      summarize(area = sum(area), .groups = "keep") %>%
      ungroup() %>%
      group_by(county_name, state_name) %>%
      mutate(total_county_area = sum(area)) %>%
      mutate(year = year, .before = everything()) %>%
      left_join(cprg_county_df, by = join_by(county_name, state_name)) %>%
      mutate(tcc_available = .env$tcc_available)
    
    # Summarize the area of each land cover type by ctu
    lc_ctu <- lc_rc %>%
      filter(!is.na(land_cover_type) & !is.na(ctu_name)) %>%
      group_by(ctu_name, ctu_class, county_name, state_name, land_cover_type) %>%
      summarize(area = sum(area), .groups = "keep") %>%
      ungroup() %>%
      group_by(ctu_name, ctu_class, county_name, state_name) %>%
      mutate(total_ctu_area = sum(area)) %>%
      mutate(year = year, .before = everything()) %>%
      left_join(cprg_ctu_df, by = join_by(ctu_name, ctu_class, county_name, state_name)) %>%
      mutate(tcc_available = .env$tcc_available)
    
    
    
    # Add the results for the current year to the results dataframe
    nlcd_county <<- rbind(
      nlcd_county,
      lc_county %>%
        dplyr::select(
          geoid, county_name, state_name,
          year, land_cover_type, 
          area, total_county_area, tcc_available
        )
    )
    
    nlcd_ctu <<- rbind(
      nlcd_ctu,
      lc_ctu %>%
        dplyr::select(
          gnis, geoid_wis, ctu_name, ctu_class,
          county_name, state_name,
          year, land_cover_type, 
          area, total_ctu_area, tcc_available
        )
    )
    
    system(paste0("say 'Finished ",year,"'"))
    pctBar(100, "Done! Moving to next year...")
  }
})

message("Finished!")

nlcd_county <- nlcd_county %>%
  as_tibble() %>%
  rename(
    county_id = geoid,
    total_area = total_county_area,
    inventory_year = year
  ) %>%
  dplyr::select(
    county_id, county_name, state_name,
    inventory_year, land_cover_type, area, total_area, tcc_available
  )


nlcd_ctu <- nlcd_ctu %>%
  as_tibble() %>%
  left_join(cprg_county_df, by = join_by(county_name, state_name)) %>%
  rename(
    county_id = geoid,
    total_area = total_ctu_area,
    inventory_year = year
  ) %>%
  mutate(ctu_id = case_when(
    is.na(gnis) & !is.na(geoid_wis) ~ geoid_wis,
    !is.na(gnis) & is.na(geoid_wis) ~ gnis
  )) %>%
  dplyr::select(-c(gnis, geoid_wis)) %>%
  dplyr::select(
    county_id, ctu_id, ctu_name, ctu_class, county_name, state_name,
    inventory_year, land_cover_type, area, total_area, tcc_available
  )



# User chooses whether to overwrite the rds files
if (overwrite_RDS) {
  message("Exporting RDS files...")
  
  saveRDS(nlcd_county, paste0("./_nature/data-raw/nlcd_county_landcover_allyrs_tmp.rds"))
  saveRDS(nlcd_ctu, paste0("./_nature/data-raw/nlcd_ctu_landcover_allyrs_tmp.rds"))
  
}

rm(list = ls())
