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






# Set inpath for nlcd raw images
inpath_nlcd <- "./_nature/data-raw/nlcd_land_cover_by_year"
# List all files in the NLCD directory
nlcd_files_all <- list.files(inpath_nlcd, full.names = TRUE)


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

# function to grab year from filename
# e.g. nlcd_land_cover_by_pixel_2006.RDS
get_year <- function(filename) {
  year <- str_extract(basename(filename), "\\d{4}")
  return(as.numeric(year))
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
start_year <- 2000
end_year <- 2022


# by county
nlcd_county <- data.frame(
  geoid = as.numeric(),
  county_name = as.character(),
  state_name = as.character(),
  year = as.numeric(),
  land_cover_type = as.character(),
  area = as.numeric(),
  potential_wetland_area = as.numeric(),
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
  potential_wetland_area = as.numeric(),
  total_ctu_area = as.numeric(),
  tcc_available = as.logical()
)





# Loop through all years of NLCD data and extract area estimates by land cover type and by county
lapply(start_year:end_year, function(year) {
  # browser()
  message(paste0("\nBeginning year: ", year, "\n"))
  pctBar(0, paste0("Loading NLCD land cover dataset for ", year))

  lc_df <- readRDS(nlcd_files_all[get_year(nlcd_files_all) %in% year])
  

  # if there is no data in the tree canopy cover column...
  if (sum(!is.na(lc_df$tree_canopy_cover)) == 0) {
    tcc_available <- FALSE
    
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
        ),
        isWetland = if_else(grepl("Wetland", land_cover_type), TRUE, FALSE),
        isDeveloped = if_else(grepl("Developed", land_cover_type), TRUE, FALSE),
        isWater = if_else(grepl("Water", land_cover_type), TRUE, FALSE),
        futureWetEligible = 
          # cannot be a wetland, developed, or water currently
          !isWetland & !isDeveloped & !isWater &
          # must be classified as restorable in the land cover carbon dataset
          (!is.na(wetlands_restorable) & wetlands_restorable == 1) 
      )
    


    
    pctBar(90, "Tidying final data")
    
    # Summarize the area of each land cover type by county
    lc_county <- lc_rc %>%
      # remove any rows with NA land cover type or county name
      filter(!is.na(land_cover_type) & !is.na(county_name)) %>%
      group_by(county_name, state_name, land_cover_type) %>%
      # summarize area by land cover type and county
      summarize(area = sum(area), .groups = "keep") %>%
      ungroup() %>%
      group_by(county_name, state_name) %>%
      mutate(total_county_area = sum(area)) %>%
      mutate(year = year, .before = everything()) %>%
      left_join(cprg_county_df, by = join_by(county_name, state_name)) %>%
      mutate(tcc_available = .env$tcc_available)
    
    # add potential wetland area
    lc_county <- lc_rc %>%
      filter(!is.na(land_cover_type) & !is.na(county_name)) %>%
      # filter to only future wetland eligible areas
      filter(futureWetEligible) %>%
      group_by(county_name, state_name, land_cover_type) %>%
      summarize(potential_wetland_area = sum(area), .groups = "keep") %>%
      right_join(lc_county, by = join_by(county_name, state_name, land_cover_type)) %>%
      arrange(county_name, land_cover_type) %>%
      mutate(potential_wetland_area = if_else(is.na(potential_wetland_area), 0, potential_wetland_area)) %>%
      dplyr::relocate(year, geoid, county_name, state_name, land_cover_type, area, total_county_area, potential_wetland_area, tcc_available)
    
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
    
    # add potential wetland area
    lc_ctu <- lc_rc %>%
      filter(!is.na(land_cover_type) & !is.na(ctu_name)) %>%
      filter(futureWetEligible) %>%
      group_by(ctu_name, ctu_class, county_name, state_name, land_cover_type) %>%
      summarize(potential_wetland_area = sum(area), .groups = "keep") %>%
      right_join(lc_ctu, by = join_by(ctu_name, ctu_class, county_name, state_name, land_cover_type)) %>%
      arrange(ctu_name, land_cover_type) %>%
      mutate(potential_wetland_area = if_else(is.na(potential_wetland_area), 0, potential_wetland_area)) %>%
      dplyr::relocate(year, gnis, geoid_wis, ctu_name, ctu_class, county_name, 
                      state_name, land_cover_type, area, total_ctu_area, 
                      potential_wetland_area, tcc_available)
    
    
    
    # Add the results for the current year to the results dataframe
    nlcd_county <<- rbind(
      nlcd_county,
      lc_county %>%
        dplyr::select(
          geoid, county_name, state_name,
          year, land_cover_type, area,
          potential_wetland_area, total_county_area, tcc_available
        )
    )
    
    nlcd_ctu <<- rbind(
      nlcd_ctu,
      lc_ctu %>%
        dplyr::select(
          gnis, geoid_wis, ctu_name, ctu_class,
          county_name, state_name,
          year, land_cover_type, area, 
          potential_wetland_area, total_ctu_area, tcc_available
        )
    )
    
    system(paste0("say 'Finished ", year, "'"))
    pctBar(100, "Done! Moving to next year...")
    
  } else { # if there is data in the tree column
    tcc_available <- TRUE
    
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
        ),
        
        isWetland = if_else(grepl("Wetland", land_cover_type), TRUE, FALSE),
        # check if it matches "Developed" or "Urban"
        isDeveloped = if_else(grepl("Developed|Urban", land_cover_type), TRUE, FALSE),
        # isDeveloped = if_else(grepl("Developed", land_cover_type), TRUE, FALSE),
        isWater = if_else(grepl("Water", land_cover_type), TRUE, FALSE),
        futureWetEligible = 
          # cannot be a wetland, developed, or water currently
          !isWetland & !isDeveloped & !isWater &
          # must be classified as restorable in the land cover carbon dataset
          (!is.na(wetlands_restorable) & wetlands_restorable == 1),
        
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
    
    # add potential wetland area
    lc_county <- lc_rc_final %>%
      filter(!is.na(land_cover_type) & !is.na(county_name)) %>%
      # filter to only future wetland eligible areas
      filter(futureWetEligible) %>%
      group_by(county_name, state_name, land_cover_type) %>%
      summarize(potential_wetland_area = sum(area_corrected), .groups = "keep") %>%
      right_join(lc_county, by = join_by(county_name, state_name, land_cover_type)) %>%
      arrange(county_name, land_cover_type) %>%
      mutate(potential_wetland_area = if_else(is.na(potential_wetland_area), 0, potential_wetland_area)) %>%
      dplyr::relocate(year, geoid, county_name, state_name, land_cover_type, area, total_county_area, potential_wetland_area, tcc_available)
    
  
    
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
    
 
    # add potential wetland area
    lc_ctu <- lc_rc_final %>%
      filter(!is.na(land_cover_type) & !is.na(ctu_name)) %>%
      filter(futureWetEligible) %>%
      group_by(ctu_name, ctu_class, county_name, state_name, land_cover_type) %>%
      summarize(potential_wetland_area = sum(area_corrected), .groups = "keep") %>%
      right_join(lc_ctu, by = join_by(ctu_name, ctu_class, county_name, state_name, land_cover_type)) %>%
      arrange(ctu_name, land_cover_type) %>%
      mutate(potential_wetland_area = if_else(is.na(potential_wetland_area), 0, potential_wetland_area)) %>%
      dplyr::relocate(year, gnis, geoid_wis, ctu_name, ctu_class, county_name, 
                      state_name, land_cover_type, area, total_ctu_area, 
                      potential_wetland_area, tcc_available)
    

    
    # Add the results for the current year to the results dataframe
    nlcd_county <<- rbind(
      nlcd_county,
      lc_county %>%
        dplyr::select(
          geoid, county_name, state_name,
          year, land_cover_type, area,
          potential_wetland_area, total_county_area, tcc_available
        )
    )
    
    nlcd_ctu <<- rbind(
      nlcd_ctu,
      lc_ctu %>%
        dplyr::select(
          gnis, geoid_wis, ctu_name, ctu_class,
          county_name, state_name,
          year, land_cover_type, area, 
          potential_wetland_area, total_ctu_area, tcc_available
        )
    )
    
    system(paste0("say 'Finished ", year, "'"))
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
    inventory_year, land_cover_type, area, total_area, 
    potential_wetland_area, tcc_available
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
    inventory_year, land_cover_type, area, total_area, potential_wetland_area,
    tcc_available
  )



# User chooses whether to overwrite the rds files
if (overwrite_RDS) {
  message("Exporting RDS files...")

  saveRDS(nlcd_county, paste0("./_nature/data-raw/nlcd_county_landcover_allyrs_tmp.rds"))
  saveRDS(nlcd_ctu, paste0("./_nature/data-raw/nlcd_ctu_landcover_allyrs_tmp.rds"))
}

rm(list = ls())
