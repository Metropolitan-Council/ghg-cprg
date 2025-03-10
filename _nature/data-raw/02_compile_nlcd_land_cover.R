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




# Set inpath for nlcd raw images
inpath_nlcd <- "./_nature/data-raw/nlcd_raw_imgs"
# List all files in the NLCD directory
nlcd_files_all <- list.files(inpath_nlcd, full.names = TRUE)

# Grab only the .tiff file names that match the string "Land_Cover"
# nlcd_lc_files <- nlcd_files_all[grep("Land_Cover", nlcd_files_all)]
nlcd_lc_files <- nlcd_files_all[grep("LndCov", nlcd_files_all)]


# Grab only the .tiff file names that match the string "Land_Cover"
nlcd_tcc_files <- nlcd_files_all[grep("tcc", nlcd_files_all)]

# grab nlcd legend
nlcd.legend <- FedData::pal_nlcd()



# Useful funs -------------------------------------------------------------


get_year <- function(filename) {
  year <- as.numeric(sub(".*_(\\d{4})_.*", "\\1", filename))
  return(year)
}
pctBar <- function(percentage, msg=NULL) {
  if (percentage < 0 || percentage > 100) {
    stop("Percentage must be between 0 and 100")
  }
  
  total_length <- 20
  filled_length <- round(total_length * (percentage / 100))
  empty_length <- total_length - filled_length
  
  bar <- paste0("|", strrep("=", filled_length), strrep("_", empty_length), "|")
  message(paste0(sprintf("%3d%%  %s", percentage, bar)," ", msg))
}



# Create empty dataframes to store the results
# Make sure the number of rows corresponds to the number of unique combinations between:
# no. counties * no. years * no. land cover types
start_year <- 2001
end_year <- 2022

# List out your new land cover types
rc_cover_types <- c("Bare", "Built-up", "Cropland", "Grassland", "Shrubland", 
                    "Tree", "Water", "Wetland", "Urban_Grassland", "Urban_Tree")

# by county
nlcd_county <- cprg_county_df %>%
  crossing(year=seq(start_year,end_year),       # accounts for all unique years  
           land_cover_type=rc_cover_types) %>%  # accounts for all unique cover types 
  mutate(area = as.numeric(NA),
         total_county_area = as.numeric(NA))


# by CTU
nlcd_ctu <- cprg_ctu_df %>%
  crossing(year=seq(start_year,end_year),       # accounts for all unique years  
           land_cover_type=rc_cover_types) %>%  # accounts for all unique cover types 
  mutate(area = as.numeric(NA),
         total_ctu_area = as.numeric(NA))




# year <- 2010
# year <- 2011
# Loop through all years of NLCD data and extract area estimates by land cover type and by county
lapply(start_year:end_year, function(year) {
  # browser()
  message(paste0("\nBeginning year: ", year, "\n"))
  pctBar(0)

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
    
  } else {
    # Move to the next year if the land cover type layer is missing
    message(paste0("\nSTOP! No land cover layers for ", year, ".\nMoving to next year..."))
    return(NULL)
  }
  pctBar(10)
  
  


  # Test to see if the current year is in your list of tcc files
  if (year %in% get_year(nlcd_tcc_files)) {
    # Indicate if the tree canopy cover layer was successfully retrieved
    tcc_available <- TRUE

    nlcd_tcc <- terra::rast(nlcd_tcc_files[get_year(nlcd_tcc_files) %in% year])

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
  pctBar(20)
  

  # Mask the cell size of nlcd_lc_mask with cprg_county (this will be used to calculate area)
  nlcd_lc_area <- terra::mask(cellSize(nlcd_lc, unit = "km"), cprg_county)
  # Rasterize cprg_ctu with nlcd_lc_mask using "county_name" field
  county_raster <- terra::rasterize(cprg_ctu, nlcd_lc, field = "county_name")
  pctBar(30)
  
  # Rasterize cprg_ctu with nlcd_lc_mask using "ctu_name" field
  ctu_raster <- terra::rasterize(cprg_ctu, nlcd_lc, field = "ctu_name")
  
  # Rasterize cprg_ctu with nlcd_lc_mask using "ctu_class" field
  ctu_class_raster <- terra::rasterize(cprg_ctu, nlcd_lc, field = "ctu_class")
  pctBar(40)
  
  
  nlcd_lc_values <- terra::extract(nlcd_lc, cprg_county)
  pctBar(50)
  
  area_values <- terra::extract(nlcd_lc_area, cprg_county)
  pctBar(60)
  
  county_values <- terra::extract(county_raster, cprg_county)
  ctu_values <- terra::extract(ctu_raster, cprg_county)
  ctu_class_values <- terra::extract(ctu_class_raster, cprg_county)
  
  

  
  
  if (tcc_available) {
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
    
    pctBar(80)
    
    
    
    area_by_county <- lc_df %>%
      group_by(county_name, state_name) %>%
      summarize(county_total_area=sum(area),.groups="keep") %>%
      mutate(year=.env$year)
    
    area_by_ctu <- lc_df %>%
      group_by(ctu_name, ctu_class, county_name, state_name) %>%
      summarize(ctu_total_area=sum(area),.groups="keep") %>%
      mutate(year=.env$year)
    
    
    # Recompute area based on new land cover designations
    # Here we treat "Developed, Open Space" as "Urban_Grassland" if tree canopy cover is 0
    # If tree canopy cover is > 0, we treat "Developed" as "Urban_Tree"
    # If tree canopy cover is 0, we treat "Developed" as "Built-up" (so long as it's not "Developed, Open Space")
    # The rest of the land cover types are pretty straightforward
    lc_rc <- lc_df %>%
      mutate(
        land_cover_type = case_when(
          grepl("Developed, Open Space", nlcd_cover_class) & tree_canopy_cover == 0 ~ "Urban_Grassland",
          grepl("Developed", nlcd_cover_class) & tree_canopy_cover > 0 ~ "Urban_Tree",
          grepl("Developed", nlcd_cover_class) & !grepl("Developed, Open Space", nlcd_cover_class) & tree_canopy_cover == 0 ~ "Built-up",
          grepl("Deciduous Forest", nlcd_cover_class) ~ "Tree",
          grepl("Evergreen Forest", nlcd_cover_class) ~ "Tree",
          grepl("Mixed Forest", nlcd_cover_class) ~ "Tree",
          grepl("Dwarf Scrub", nlcd_cover_class) ~ "Shrubland",
          grepl("Shrub/Scrub", nlcd_cover_class) ~ "Shrubland",
          grepl("Grassland/Herbaceous", nlcd_cover_class) ~ "Grassland",
          grepl("Sedge/Herbaceous", nlcd_cover_class) ~ "Grassland",
          grepl("Cultivated Crops", nlcd_cover_class) ~ "Cropland",
          grepl("Pasture/Hay", nlcd_cover_class) ~ "Cropland",
          grepl("Barren Land", nlcd_cover_class) ~ "Bare",
          grepl("Perennial Ice/Snow", nlcd_cover_class) ~ "Snow",
          grepl("Open Water", nlcd_cover_class) ~ "Water",
          grepl("Woody Wetlands", nlcd_cover_class) ~ "Tree", ## Changed from "Wetland"
          grepl("Emergent Herbaceous Wetlands", nlcd_cover_class) ~ "Wetland",
          
          .default="CHECK" 
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
      filter(area_residual > 0 & nlcd_cover_class == "Developed, Open Space") %>%
      mutate(
        land_cover_type = case_when(
          land_cover_type == "Urban_Tree" ~ "Urban_Grassland",
          .default = "CHECK"
        ) 
      ) %>%
      dplyr::select(-area_corrected) %>%
      rename(area_corrected=area_residual) %>%
      mutate(area_residual = NA,
             area=NA)
    
    

    # We still need to account for a little bit of area that gets missed due to filtering
    # In this case, we want the area of land after taking into account tree canopy area, 
    # but in this case, the residual is Built-up (not Urban_Grassland)
    lc_rc_residual_impervious <- lc_rc %>%
      filter(area_residual > 0 & nlcd_cover_class != "Developed, Open Space") %>%
      mutate(
        land_cover_type = case_when(
          land_cover_type == "Urban_Tree" ~ "Built-up",
          .default = "CHECK"
        ) 
      ) %>%
      dplyr::select(-area_corrected) %>%
      rename(area_corrected=area_residual) %>%
      mutate(area_residual = NA,
             area=NA)
    
    
    
    # Bind the residual grassland area to the main dataframe
    lc_rc <- rbind(lc_rc, lc_rc_residual_grassland, lc_rc_residual_impervious)
    
    pctBar(90)
    
    
    # Summarize the area of each land cover type by county
    lc_county <- lc_rc %>%
      filter(!is.na(land_cover_type) & !is.na(county_name)) %>%
      group_by(county_name, state_name, land_cover_type) %>%
      summarize(area = sum(area_corrected), .groups = "keep") %>%
      ungroup() %>% group_by(county_name,state_name) %>%
      mutate(total_county_area = sum(area)) %>%
      mutate(year = year, .before = everything()) %>%
      left_join(cprg_county_df, by = join_by(county_name, state_name))
    
    # Summarize the area of each land cover type by ctu
    lc_ctu <- lc_rc %>%
      filter(!is.na(land_cover_type) & !is.na(ctu_name)) %>%
      group_by(ctu_name, ctu_class, county_name, state_name, land_cover_type) %>%
      summarize(area = sum(area_corrected), .groups = "keep") %>%
      ungroup() %>% group_by(ctu_name, ctu_class, county_name, state_name) %>%
      mutate(total_ctu_area = sum(area)) %>%
      mutate(year = year, .before = everything()) %>%
      left_join(cprg_ctu_df, by = join_by(ctu_name, ctu_class, county_name, state_name))
    
    
    # Add the results for the current year to the results dataframe
    nlcd_county <- nlcd_county %>%
      left_join(lc_county %>% rename(a1=area,a2=total_county_area),
                by = join_by(geoid, county_name, state_name, year, land_cover_type)) %>% 
      mutate(
        area = coalesce(area, a1),
        total_county_area = coalesce(total_county_area, a2)
      ) %>%
      dplyr::select(names(nlcd_county)) %>% View()
    
    
    # Add the results for the current year to the results dataframe
    nlcd_ctu <- nlcd_ctu %>%
      left_join(lc_ctu %>% rename(a1=area,a2=total_ctu_area),
                by = join_by(gnis, geoid_wis, ctu_name, ctu_class, 
                             county_name, state_name, year, land_cover_type)) %>% 
      mutate(
        area = coalesce(area, a1),
        total_ctu_area = coalesce(total_ctu_area, a2)
      ) %>%
      dplyr::select(names(nlcd_ctu)) 
    
    
    pctBar(100,"- Moving to next year")
    
    
  } else {
    pctBar(70, paste0("Tree canopy data NOT available for ", year))
    
    lc_df <- as_tibble(data.frame(
      county_name = county_values[, 2],
      ctu_name = ctu_values[, 2],
      ctu_class = ctu_class_values[, 2],
      nlcd_cover = nlcd_lc_values[, 2],
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
    
    pctBar(80)
    
    
    area_by_county <- lc_df %>%
      group_by(county_name, state_name) %>%
      summarize(county_total_area=sum(area),.groups="keep") %>%
      mutate(year=.env$year)
    
    area_by_ctu <- lc_df %>%
      group_by(ctu_name, ctu_class, county_name, state_name) %>%
      summarize(ctu_total_area=sum(area),.groups="keep") %>%
      mutate(year=.env$year)
    
    
    # Recompute area based on new land cover designations
    # Here we treat "Developed, Open Space" as "Urban_Grassland" if tree canopy cover is 0
    # If tree canopy cover is > 0, we treat "Developed" as "Urban_Tree"
    # If tree canopy cover is 0, we treat "Developed" as "Built-up" (so long as it's not "Developed, Open Space")
    # The rest of the land cover types are pretty straightforward
    lc_rc <- lc_df %>%
      mutate(
        land_cover_type = case_when(
          # grepl("Developed, Open Space", nlcd_cover_class) & tree_canopy_cover == 0 ~ "Urban_Grassland",
          # grepl("Developed", nlcd_cover_class) & tree_canopy_cover > 0 ~ "Urban_Tree",
          # grepl("Developed", nlcd_cover_class) & !grepl("Developed, Open Space", nlcd_cover_class) & tree_canopy_cover == 0 ~ "Built-up",
          grepl("Developed", nlcd_cover_class) ~ "Built-up",
          grepl("Deciduous Forest", nlcd_cover_class) ~ "Tree",
          grepl("Evergreen Forest", nlcd_cover_class) ~ "Tree",
          grepl("Mixed Forest", nlcd_cover_class) ~ "Tree",
          grepl("Dwarf Scrub", nlcd_cover_class) ~ "Shrubland",
          grepl("Shrub/Scrub", nlcd_cover_class) ~ "Shrubland",
          grepl("Grassland/Herbaceous", nlcd_cover_class) ~ "Grassland",
          grepl("Sedge/Herbaceous", nlcd_cover_class) ~ "Grassland",
          grepl("Cultivated Crops", nlcd_cover_class) ~ "Cropland",
          grepl("Pasture/Hay", nlcd_cover_class) ~ "Cropland",
          grepl("Barren Land", nlcd_cover_class) ~ "Bare",
          grepl("Perennial Ice/Snow", nlcd_cover_class) ~ "Snow",
          grepl("Open Water", nlcd_cover_class) ~ "Water",
          grepl("Woody Wetlands", nlcd_cover_class) ~ "Tree", ## Changed from "Wetland"
          grepl("Emergent Herbaceous Wetlands", nlcd_cover_class) ~ "Wetland"
        )
      ) 
    
    pctBar(90)
    
    
    # Summarize the area of each land cover type by county
    lc_county <- lc_rc %>%
      filter(!is.na(land_cover_type) & !is.na(county_name)) %>%
      group_by(county_name, state_name, land_cover_type) %>%
      summarize(area = sum(area), .groups = "keep") %>%
      ungroup() %>% group_by(county_name,state_name) %>%
      mutate(total_county_area = sum(area)) %>%
      mutate(year = year, .before = everything()) %>%
      left_join(cprg_county_df, by = join_by(county_name, state_name))
    
    # Summarize the area of each land cover type by ctu
    lc_ctu <- lc_rc %>%
      filter(!is.na(land_cover_type) & !is.na(ctu_name)) %>%
      group_by(ctu_name, ctu_class, county_name, state_name, land_cover_type) %>%
      summarize(area = sum(area), .groups = "keep") %>%
      ungroup() %>% group_by(ctu_name, ctu_class, county_name, state_name) %>%
      mutate(total_ctu_area = sum(area)) %>%
      mutate(year = year, .before = everything()) %>%
      left_join(cprg_ctu_df, by = join_by(ctu_name, ctu_class, county_name, state_name))
  
    # Add the results for the current year to the results dataframe
    nlcd_county <- nlcd_county %>%
      left_join(lc_county %>% rename(a1=area,a2=total_county_area),
                by = join_by(geoid, county_name, state_name, year, land_cover_type)) %>% 
      mutate(
        area = coalesce(area, a1),
        total_county_area = coalesce(total_county_area, a2)
      ) %>%
      dplyr::select(names(nlcd_county)) 
  
    
    # Add the results for the current year to the results dataframe
    nlcd_ctu <- nlcd_ctu %>%
      left_join(lc_ctu %>% rename(a1=area,a2=total_ctu_area),
                by = join_by(gnis, geoid_wis, ctu_name, ctu_class, 
                             county_name, state_name, year, land_cover_type)) %>% 
      mutate(
        area = coalesce(area, a1),
        total_ctu_area = coalesce(total_ctu_area, a2)
      ) %>%
      dplyr::select(names(nlcd_ctu)) 
    
    pctBar(100,"- Moving to next year")
    

  }
  
  
})


message("Finished!")

browser()
nlcd_county <- nlcd_county %>%
  as_tibble() %>%
  dplyr::select(-c(geoid))
nlcd_ctu <- nlcd_ctu %>%
  as_tibble() %>%
  dplyr::select(-c(gnis, geoid_wis))


# create metadata
nlcd_county_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "county_name", class(nlcd_county$county_name), "County name",
    "state_name", class(nlcd_county$state_name), "State name",
    "year", class(nlcd_county$year), "Year",
    "land_cover_type", class(nlcd_county$land_cover_type), "Land cover type from National Land Cover Database. 'Urban_' indicates a natural area within NLCD designated developed land cover",
    "area", class(nlcd_county$area), "Area of land cover in square kilometers. 'Urban_Tree' is scaled based on the percentage of tree canopy cover within 'Developed' areas",
    "total_area", class(nlcd_county$total_area), "Sum of area from all cover types in square kilometers (by county)."
  )

nlcd_ctu_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "ctu_name", class(nlcd_ctu$ctu_name), "CTU name",
    "ctu_class", class(nlcd_ctu$county_name), "CTU class",
    "county_name", class(nlcd_ctu$county_name), "County name",
    "state_name", class(nlcd_ctu$state_name), "State name",
    "year", class(nlcd_ctu$year), "Year",
    "land_cover_type", class(nlcd_ctu$land_cover_type), "Land cover type from National Land Cover Database. 'Urban_' indicates a natural area within NLCD designated developed land cover",
    "area", class(nlcd_ctu$area), "Area of land cover in square kilometers. 'Urban_Tree' is scaled based on the percentage of tree canopy cover within 'Developed' areas",
    "total_area", class(nlcd_ctu$total_area), "Sum of area from all cover types in square kilometers (by CTU)."
  )




# User chooses whether to overwrite the rds files
if (overwrite_RDS) {
  message("Exporting RDS files...")

  saveRDS(nlcd_county, paste0("./_nature/data/nlcd_county_landcover_", head(sort(unique(nlcd_county$year)), 1), "_", tail(sort(unique(nlcd_county$year)), 1), ".rds"))
  saveRDS(nlcd_county_meta, paste0("./_nature/data/nlcd_county_landcover_", head(sort(unique(nlcd_county$year)), 1), "_", tail(sort(unique(nlcd_county$year)), 1), "_meta.rds"))

  saveRDS(nlcd_ctu, paste0("./_nature/data/nlcd_ctu_landcover_", head(sort(unique(nlcd_ctu$year)), 1), "_", tail(sort(unique(nlcd_ctu$year)), 1), ".rds"))
  saveRDS(nlcd_ctu_meta, paste0("./_nature/data/nlcd_ctu_landcover_", head(sort(unique(nlcd_ctu$year)), 1), "_", tail(sort(unique(nlcd_ctu$year)), 1), "_meta.rds"))
}

