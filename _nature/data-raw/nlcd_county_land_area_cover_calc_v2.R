rm(list=ls())
source("R/_load_pkgs.R")
source("R/cprg_colors.R")

overwrite_RDS <- FALSE

nlcd.legend <- FedData::pal_nlcd()


# load the county and ctu boundaries layer
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_ctu <- readRDS("_meta/data/cprg_ctu.RDS")

# convert the counties and ctu vector to a terra object
cprg_county <- terra::vect(cprg_county)
cprg_ctu <- terra::vect(cprg_ctu)

# define CRS for using with other layers
crs_use <- terra::crs(cprg_county)
cprg_ctu <- terra::project(cprg_ctu, crs_use)







# Create an empty dataframe to store the results
nlcd_county <- data.frame(
  year = numeric(),
  county_name = character(),
  state_name = character(),
  land_cover_type = character(),
  area = numeric(),
  stringsAsFactors = FALSE
)


nlcd_ctu <- data.frame(
  year = numeric(),
  ctu_name = character(),
  ctu_class = character(),
  county_name = character(),
  state_name = character(),
  land_cover_type = character(),
  area = numeric(),
  stringsAsFactors = FALSE
)





inpath_nlcd <- "./_nature/data-raw/nlcd_raw_imgs"


# List all files in the NLCD directory
nlcd_files_all <- list.files(inpath_nlcd, full.names = TRUE)

# Grab only the .tiff file names that match the string "Land_Cover"
nlcd_lc_files <- nlcd_files_all[grep("Land_Cover", nlcd_files_all)]
# Grab only the .tiff file names that match the string "Land_Cover"
nlcd_tcc_files <- nlcd_files_all[grep("tcc", nlcd_files_all)]


get_year <- function(filename) {
  year <- as.numeric(sub('.*_(\\d{4})_.*', '\\1', filename))
  return(year)
}


start_year <- 2001
end_year <- 2021

# Loop through all years of NLCD data and extract area estimates by land cover type and by county
lapply(start_year:end_year, function(year) {
  # browser()
  message(paste0("\nBeginning year: ", year, "\n"))
  message(paste0(" 0%  |____________________|"))

  # browser()
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
    
  } else {
    # Move to the next year if the land cover type layer is missing
    message(paste0("\nSTOP! No land cover layers for ", year, ".\nMoving to next year..."))
    return(NULL)
  }
  
  
  
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
  

  

  if (tcc_available) {
    message(paste0("30%  |======______________| - Tree canopy data available for ", year))
    # Perform the necessary calculations for the current year
    nlcd_lc_mask <- terra::mask(nlcd_lc, cprg_county)
    nlcd_lc_area <- terra::mask(cellSize(nlcd_lc_mask, unit = "km"), cprg_county)
    county_raster <- terra::rasterize(cprg_ctu, nlcd_lc_mask, field = "county_name")
    ctu_raster <- terra::rasterize(cprg_ctu, nlcd_lc_mask, field = "ctu_name")
    ctu_class_raster <- terra::rasterize(cprg_ctu, nlcd_lc_mask, field = "ctu_class")
    
    message(paste0("40%  |========____________|"))
    
    nlcd_lc_values <- terra::extract(nlcd_lc, cprg_county)
    # nlcd_is_values <- terra::extract(nlcd_is, cprg_county)
    nlcd_tcc_values <- terra::extract(nlcd_tcc, cprg_county)
    
    message(paste0("50%  |==========__________|"))
    
    # nlcd_is_values <- nlcd_is_values %>%
    #   modify_if(is.numeric, ~replace_na(., 0))
    
    nlcd_tcc_values <- nlcd_tcc_values %>%
      modify_if(is.numeric, ~ replace_na(., 0))
    
    message(paste0("60%  |============________|"))
    
    area_values <- terra::extract(nlcd_lc_area, cprg_county)
    lc_values <- terra::extract(nlcd_lc_mask, cprg_county)
    county_values <- terra::extract(county_raster, cprg_county)
    ctu_values <-  terra::extract(ctu_raster, cprg_county)
    ctu_class_values <- terra::extract(ctu_class_raster, cprg_county)
    
    message(paste0("70%  |==============______|"))
    
    # browser()
    
    lc_df <- as_tibble(data.frame(
      county_name = county_values[, 2],
      ctu_name = ctu_values[, 2],
      ctu_class = ctu_class_values[, 2],
      nlcd_cover = nlcd_lc_values[, 2],
      # impervious_cover = as.numeric(as.character(nlcd_is_values[, 2])),
      tree_canopy_cover = as.numeric(as.character(nlcd_tcc_values[, 2])),
      area = area_values[, 2]
    ))
    
    lc_df <- lc_df %>%
      left_join(data.frame(cprg_ctu) %>% 
                  dplyr::select(ctu_name, ctu_class, county_name, state_name, statefp, state_abb),
                by = join_by(county_name, ctu_name, ctu_class))
    
    
    message(paste0("80%  |================____|"))
    
    
    
    lc_rc <- lc_df %>%
      left_join(nlcd.legend %>%
                  dplyr::select(ID, Class) %>%
                  rename(nlcd_cover = ID, 
                         nlcd_cover_class = Class), by = "nlcd_cover") %>%
      mutate(
        land_cover_type = case_when(
          grepl("Developed, Open Space", nlcd_cover_class) & tree_canopy_cover == 0 ~ "Urban_Grassland",
          grepl("Developed", nlcd_cover_class) & !grepl("Developed, Open Space", nlcd_cover_class) & tree_canopy_cover > 0 ~ "Urban_Tree",
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
          grepl("Emergent Herbaceous Wetlands", nlcd_cover_class) ~ "Wetland"
        )
      ) %>%
      mutate(
        area_corrected = if_else(land_cover_type == "Urban_Tree", area * (tree_canopy_cover / 100), area)
      )
    
    
    message(paste0("90%  |==================__|"))
   
    
    lc_county <- lc_rc %>%
      filter(!is.na(land_cover_type) & !is.na(county_name)) %>%
      group_by(county_name, land_cover_type, state_name, statefp, state_abb) %>%
      summarize(area = sum(area_corrected), .groups = "keep") %>%
      mutate(year = year, .before=everything())
    
    
    lc_ctu <- lc_rc %>%
      filter(!is.na(land_cover_type) & !is.na(ctu_name)) %>%
      group_by(ctu_name, ctu_class, county_name, land_cover_type, state_name, statefp, state_abb) %>%
      summarize(area = sum(area_corrected), .groups = "keep") %>%
      mutate(year = year, .before=everything())
    
    
    
    # Add the results for the current year to the results dataframe
    nlcd_county <<- rbind(nlcd_county, data.frame(
      year = lc_county$year,
      county_name = lc_county$county_name,
      state_name = lc_county$state_name,
      land_cover_type = lc_county$land_cover_type,
      area = lc_county$area,
      stringsAsFactors = FALSE
    ))
    
    
    
    # Add the results for the current year to the results dataframe
    nlcd_ctu <<- rbind(nlcd_ctu, data.frame(
      year = lc_ctu$year,
      ctu_name = lc_ctu$ctu_name,
      ctu_class = lc_ctu$ctu_class,
      county_name = lc_ctu$county_name,
      state_name = lc_ctu$state_name,
      land_cover_type = lc_ctu$land_cover_type,
      area = lc_ctu$area,
      stringsAsFactors = FALSE
    ))
    
    
    message(paste0("100% |====================|"))
    
  } else {
    
    message(paste0("30%  |======______________| - Tree canopy data NOT available for ", year))
    # Perform the necessary calculations for the current year
    nlcd_lc_mask <- terra::mask(nlcd_lc, cprg_county)
    nlcd_lc_area <- terra::mask(cellSize(nlcd_lc_mask, unit = "km"), cprg_county)
    county_raster <- terra::rasterize(cprg_ctu, nlcd_lc_mask, field = "county_name")
    ctu_raster <- terra::rasterize(cprg_ctu, nlcd_lc_mask, field = "ctu_name")
    ctu_class_raster <- terra::rasterize(cprg_ctu, nlcd_lc_mask, field = "ctu_class")
    
    message(paste0("40%  |========____________|"))
    
    nlcd_lc_values <- terra::extract(nlcd_lc, cprg_county)
    # nlcd_is_values <- terra::extract(nlcd_is, cprg_county)
    # nlcd_tcc_values <- terra::extract(nlcd_tcc, cprg_county)
    
    message(paste0("50%  |==========__________|"))
    
    # nlcd_is_values <- nlcd_is_values %>%
    #   modify_if(is.numeric, ~replace_na(., 0))
    
    # nlcd_tcc_values <- nlcd_tcc_values %>%
    #   modify_if(is.numeric, ~ replace_na(., 0))
    
    message(paste0("60%  |============________|"))
    
    area_values <- terra::extract(nlcd_lc_area, cprg_county)
    lc_values <- terra::extract(nlcd_lc_mask, cprg_county)
    county_values <- terra::extract(county_raster, cprg_county)
    ctu_values <-  terra::extract(ctu_raster, cprg_county)
    ctu_class_values <- terra::extract(ctu_class_raster, cprg_county)
    
    message(paste0("70%  |==============______|"))
    
    # browser()
    
    lc_df <- as_tibble(data.frame(
      county_name = county_values[, 2],
      ctu_name = ctu_values[, 2],
      ctu_class = ctu_class_values[, 2],
      nlcd_cover = nlcd_lc_values[, 2],
      # impervious_cover = as.numeric(as.character(nlcd_is_values[, 2])),
      tree_canopy_cover = as.numeric(NA),
      area = area_values[, 2]
    ))
    
    lc_df <- lc_df %>%
      left_join(data.frame(cprg_ctu) %>% 
                  dplyr::select(ctu_name, ctu_class, county_name, state_name, statefp, state_abb),
                by = join_by(county_name, ctu_name, ctu_class))
    
    message(paste0("80%  |================____|"))
    
    # sort(unique(lc_df$nlcd_cover))
    lc_rc <- lc_df %>%
      left_join(nlcd.legend %>%
                  dplyr::select(ID, Class) %>%
                  rename(nlcd_cover = ID, 
                         nlcd_cover_class = Class), by = "nlcd_cover") %>%
      mutate(
        land_cover_type = case_when(
          # grepl("Developed, Open Space", nlcd_cover_class) & tree_canopy_cover == 0 ~ "Urban_Grassland",
          # grepl("Developed", nlcd_cover_class) & !grepl("Developed, Open Space", nlcd_cover_class) & tree_canopy_cover > 0 ~ "Urban_Tree",
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
      ) #%>%
      # mutate(
      #   area_corrected = if_else(land_cover_type == "Urban_Tree", area * (tree_canopy_cover / 100), area)
      # )
    
    
    message(paste0("90%  |==================__|"))
   
    
    lc_county <- lc_rc %>%
      filter(!is.na(land_cover_type) & !is.na(county_name)) %>%
      group_by(county_name, land_cover_type, state_name, statefp, state_abb) %>%
      summarize(area = sum(area), .groups = "keep") %>%
      mutate(year = year, .before=everything())
    
    
    lc_ctu <- lc_rc %>%
      filter(!is.na(land_cover_type) & !is.na(ctu_name)) %>%
      group_by(ctu_name, ctu_class, county_name, land_cover_type, state_name, statefp, state_abb) %>%
      summarize(area = sum(area), .groups = "keep") %>%
      mutate(year = year, .before=everything())
    
    
    
    # Add the results for the current year to the results dataframe
    nlcd_county <<- rbind(nlcd_county, data.frame(
      year = lc_county$year,
      county_name = lc_county$county_name,
      state_name = lc_county$state_name,
      land_cover_type = lc_county$land_cover_type,
      area = lc_county$area,
      stringsAsFactors = FALSE
    ))
    
    
    
    # Add the results for the current year to the results dataframe
    nlcd_ctu <<- rbind(nlcd_ctu, data.frame(
      year = lc_ctu$year,
      ctu_name = lc_ctu$ctu_name,
      ctu_class = lc_ctu$ctu_class,
      county_name = lc_ctu$county_name,
      state_name = lc_ctu$state_name,
      land_cover_type = lc_ctu$land_cover_type,
      area = lc_ctu$area,
      stringsAsFactors = FALSE
    ))
    
    message(paste0("100% |====================|"))
  } # end else
  
  
})

browser()

# create metadata
nlcd_county_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "year", class(nlcd_county$year), "Year",
    "county_name", class(nlcd_county$county_name), "County name",
    "state_name", class(nlcd_county$state_name), "State name",
    "land_cover_type", class(nlcd_county$land_cover_type), "Land cover type from National Land Cover Database. 'Urban_' indicates a natural area within NLCD designated developed land cover",
    "area", class(nlcd_county$area), "Area of land cover in square kilometers. 'Urban_Tree' is scaled based on the percentage of tree canopy cover within 'Developed' areas"
  )

nlcd_ctu_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "year", class(nlcd_ctu$year), "Year",
    "ctu_name", class(nlcd_ctu$ctu_name), "CTU name",
    "ctu_class", class(nlcd_ctu$county_name), "CTU class",
    "county_name ", class(nlcd_ctu$county_name), "County name",
    "state_name ", class(nlcd_ctu$state_name), "State name",
    "land_cover_type", class(nlcd_ctu$land_cover_type), "Land cover type from National Land Cover Database. 'Urban_' indicates a natural area within NLCD designated developed land cover",
    "area", class(nlcd_ctu$area), "Area of land cover in square kilometers. 'Urban_Tree' is scaled based on the percentage of tree canopy cover within 'Developed' areas"
  )




# User chooses whether to overwrite the rds files
if (overwrite_RDS) {
  saveRDS(nlcd_county, paste0("./_nature/data/nlcd_county_landcover_", head(sort(unique(nlcd_county$year)), 1), "_", tail(sort(unique(nlcd_county$year)), 1), "_v2.rds"))
  saveRDS(nlcd_county_meta, paste0("./_nature/data/nlcd_county_landcover_", head(sort(unique(nlcd_county$year)), 1), "_", tail(sort(unique(nlcd_county$year)), 1), "_v2_meta.rds"))
  
  saveRDS(nlcd_ctu, paste0("./_nature/data/nlcd_ctu_landcover_", head(sort(unique(nlcd_ctu$year)), 1), "_", tail(sort(unique(nlcd_ctu$year)), 1), "_v2.rds"))
  saveRDS(nlcd_ctu_meta, paste0("./_nature/data/nlcd_ctu_landcover_", head(sort(unique(nlcd_ctu$year)), 1), "_", tail(sort(unique(nlcd_ctu$year)), 1), "_v2_meta.rds"))
}




# nlcd_county %>% filter(year==2019 & county_name == "Anoka")
# 
# 
# nlcd_county %>%
#   left_join(
#     nlcd_county_landcover_2001_2021 %>%
#       rename(area2=area,
#              county_name=county)
#   ) %>%
#   ggplot() +
#   theme_minimal() +
#   geom_point(aes(x=area,y=area2,color=land_cover_type)) +
#   facet_wrap(~county_name)

