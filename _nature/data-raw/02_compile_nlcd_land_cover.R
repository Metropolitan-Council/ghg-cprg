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

    pctBar(80, "Recomputing area based on new land cover definitions")


    # Recompute area based on new land cover designations
    # Here we treat "Developed, Open Space" as "Urban_Grassland" if tree canopy cover is 0
    # If tree canopy cover is > 0, we treat "Developed" as "Urban_Tree"
    # If tree canopy cover is 0, we treat "Developed" as "Built_Up" (so long as it's not "Developed, Open Space")
    # The rest of the land cover types are pretty straightforward
    lc_rc <- lc_df %>%
      mutate(
        land_cover_type = case_when(
          grepl("Developed, Open Space", nlcd_cover_class) & tree_canopy_cover == 0 ~ "Urban_Grassland",
          grepl("Developed", nlcd_cover_class) & tree_canopy_cover > 0 ~ "Urban_Tree",
          grepl("Developed", nlcd_cover_class) & !grepl("Developed, Open Space", nlcd_cover_class) & tree_canopy_cover == 0 ~ "Built_Up",
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
      filter(area_residual > 0 & nlcd_cover_class == "Developed, Open Space") %>%
      mutate(
        land_cover_type = case_when(
          land_cover_type == "Urban_Tree" ~ "Urban_Grassland",
          .default = "CHECK"
        )
      ) %>%
      dplyr::select(-area_corrected) %>%
      rename(area_corrected = area_residual) %>%
      mutate(
        area_residual = NA,
        area = NA
      )



    # We still need to account for a little bit of area that gets missed due to filtering
    # In this case, we want the area of land after taking into account tree canopy area,
    # but in this case, the residual is Built_Up (not Urban_Grassland)
    lc_rc_residual_impervious <- lc_rc %>%
      filter(area_residual > 0 & nlcd_cover_class != "Developed, Open Space") %>%
      mutate(
        land_cover_type = case_when(
          land_cover_type == "Urban_Tree" ~ "Built_Up",
          .default = "CHECK"
        )
      ) %>%
      dplyr::select(-area_corrected) %>%
      rename(area_corrected = area_residual) %>%
      mutate(
        area_residual = NA,
        area = NA
      )



    # Bind the residual grassland area to the main dataframe
    lc_rc <- rbind(lc_rc, lc_rc_residual_grassland, lc_rc_residual_impervious)

    pctBar(90, "Tidying final data")


    # Summarize the area of each land cover type by county
    lc_county <- lc_rc %>%
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
    lc_ctu <- lc_rc %>%
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




    pctBar(100, "Done! Moving to next year...")
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

    pctBar(80, "Recomputing area based on new land cover definitions")


    # Recompute area based on new land cover designations
    # Here we treat "Developed, Open Space" as "Urban_Grassland" if tree canopy cover is 0
    # If tree canopy cover is > 0, we treat "Developed" as "Urban_Tree"
    # If tree canopy cover is 0, we treat "Developed" as "Built_Up" (so long as it's not "Developed, Open Space")
    # The rest of the land cover types are pretty straightforward
    lc_rc <- lc_df %>%
      mutate(
        land_cover_type = case_when(
          # grepl("Developed, Open Space", nlcd_cover_class) & tree_canopy_cover == 0 ~ "Urban_Grassland",
          # grepl("Developed", nlcd_cover_class) & tree_canopy_cover > 0 ~ "Urban_Tree",
          # grepl("Developed", nlcd_cover_class) & !grepl("Developed, Open Space", nlcd_cover_class) & tree_canopy_cover == 0 ~ "Built_Up",
          grepl("Developed", nlcd_cover_class) ~ "Built_Up",
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


nlcd_county <- nlcd_county %>%
  # Let's create an new column called "land_cover_main" which accounts for the fact
  # that "Urban_Tree" and "Urban_Grassland" are subcategories within "Built_Up".
  mutate(
    land_cover_main = case_when(
      land_cover_type %in% c("Urban_Tree", "Urban_Grassland") ~ "Built_Up",
      .default = land_cover_type
    ), .before = land_cover_type
  )


nlcd_ctu <- nlcd_ctu %>%
  # Let's create an new column called "land_cover_main" which accounts for the fact
  # that "Urban_Tree" and "Urban_Grassland" are subcategories within "Built_Up".
  mutate(
    land_cover_main = case_when(
      land_cover_type %in% c("Urban_Tree", "Urban_Grassland") ~ "Built_Up",
      .default = land_cover_type
    ), .before = land_cover_type
  )

#+ In years where tree canopy data was available, we first selected any "Developed"
#+ lands where tree canopy was >0% and then we called these pixels "Urban_Tree" and
#+ recomputed the area of that pixel based on its tree coverage. This left a residual area
#+ which we call "Built_Up".
#+
#+ In the cases where we have "Developed, Open Space" and some tree canopy, the residual
#+ area is renamed "Urban_Grassland". In the cases where we have "Developed, Low-High Intensity"
#+ the residual area is called "Built_Up"
#+
#+ The good news is that this method conserves the total area covered (i.e. if you
#+ compute the area sum across all cover types for a given county, the county area remains
#+ constant through time):
nlcd_county %>%
  group_by(inventory_year, county_name) %>%
  summarize(total_area = sum(area)) %>%
  arrange(county_name)

#+ The bad news is that from a time-series standpoint, the Built_Up
#+ area steadily increases until 2011 where there's a sharp decrease. This is because
#+ 2011 is the first year with tree canopy data, which implicitly divides the "Built_Up" area
#+ into three parts (Urban_Tree, Urban_Grassland, and...Built_Up)
nlcd_county %>%
  ggplot() +
  theme_minimal() +
  geom_path(aes(x = inventory_year, y = area, color = land_cover_type)) +
  geom_point(aes(x = inventory_year, y = area, color = land_cover_type), size = 0.5) +
  facet_wrap(~county_name)

nlcd_county %>%
  group_by(county_name, inventory_year) %>%
  summarize(area = sum(area)) %>%
  ggplot() +
  theme_minimal() +
  geom_path(aes(x = inventory_year, y = area)) +
  geom_point(aes(x = inventory_year, y = area), size = 0.5) +
  facet_wrap(~county_name)



#+ The best way to deal with it is to treat "Urban_Grassland" and
#+ "Urban_Tree" as subcategories within "Built_Up". So for future modeling efforts
#+ make sure that the total "Built_Up" lands within any given year is a sum of
#+ Urban_Tree, Urban_Grassland, and Built_Up.



#+ At this point, I want to do the following:
#+ - calculate the proportions of Urban_Tree, Urban_Grassland, and Built_Up in 2011 and 2021
#+ - extrapolate those proportions for years where tree canopy is not available, keeping the total
#+ sum equal to the area of "Built_Up"

builtUp_byCountyYear <- nlcd_county %>%
  filter(tcc_available & land_cover_main == "Built_Up") %>%
  pivot_wider(names_from = land_cover_type, values_from = area) %>%
  mutate(Built_Up_Total = Built_Up + Urban_Grassland + Urban_Tree) %>%
  group_by(county_name, inventory_year) %>%
  summarize(
    Urban_Tree_pct = Urban_Tree / Built_Up_Total,
    Urban_Grassland_pct = Urban_Grassland / Built_Up_Total,
    Built_Up_pct = Built_Up / Built_Up_Total
  ) %>%
  slice(c(1, n()))



# Rather than hard-code the years where tree canopy data was available,
# let's use coding logic
tcc_start_year <- builtUp_byCountyYear %>%
  ungroup() %>%
  arrange(inventory_year) %>%
  slice(1) %>%
  pull(inventory_year)
tcc_end_year <- builtUp_byCountyYear %>%
  ungroup() %>%
  arrange(-inventory_year) %>%
  slice(1) %>%
  pull(inventory_year)


# Extrapolate area covered by Urban_Tree and Urban_Grassland as a proportion of the
# Built_Up area by county (and ctu). First let's extrapolate for the years PRECEDING
# tree canopy data availability. Next we'll do the years that come after.
nlcd_county_extrapBefore <- nlcd_county %>%
  filter(!tcc_available & land_cover_main == "Built_Up" & inventory_year < tcc_start_year) %>%
  dplyr::select(-c(land_cover_type, total_area, tcc_available)) %>%
  left_join(builtUp_byCountyYear %>% filter(inventory_year == tcc_start_year) %>% dplyr::select(-inventory_year)) %>%
  mutate(
    Urban_Tree = Urban_Tree_pct * area,
    Urban_Grassland = Urban_Grassland_pct * area,
    Built_Up = Built_Up_pct * area
  ) %>%
  dplyr::select(-c(area, ends_with("_pct"))) %>%
  pivot_longer(cols = c(Urban_Tree, Urban_Grassland, Built_Up), names_to = "land_cover_type", values_to = "area") %>%
  mutate(tcc_available = FALSE)

# Now let's extrapolate for years after tree canopy is available
nlcd_county_extrapAfter <- nlcd_county %>%
  filter(!tcc_available & land_cover_main == "Built_Up" & inventory_year > tcc_end_year) %>%
  dplyr::select(-c(land_cover_type, total_area, tcc_available)) %>%
  left_join(builtUp_byCountyYear %>% filter(inventory_year == tcc_end_year) %>% dplyr::select(-inventory_year)) %>%
  mutate(
    Urban_Tree = Urban_Tree_pct * area,
    Urban_Grassland = Urban_Grassland_pct * area,
    Built_Up = Built_Up_pct * area
  ) %>%
  dplyr::select(-c(area, ends_with("_pct"))) %>%
  pivot_longer(cols = c(Urban_Tree, Urban_Grassland, Built_Up), names_to = "land_cover_type", values_to = "area") %>%
  mutate(tcc_available = FALSE)


# Row bind your extrapolated dataset with your original dataset (ensuring no duplicates!)
nlcd_county_final <- rbind(
  nlcd_county_extrapBefore,
  nlcd_county_extrapAfter,
  nlcd_county %>%
    dplyr::select(-total_area) %>%
    filter(!tcc_available & land_cover_main != "Built_Up"),
  nlcd_county %>%
    dplyr::select(-total_area) %>%
    filter(tcc_available)
) %>%
  arrange(inventory_year, county_name, land_cover_main, land_cover_type) %>%
  mutate(source = case_when(
    tcc_available ~ "nlcd",
    !tcc_available ~ "extrapolated"
  )) %>%
  ungroup() %>%
  group_by(county_name, inventory_year) %>%
  mutate(total_area = sum(area), .after = "area")

## Check figure
# nlcd_county_final %>%
#   ggplot() + theme_minimal() +
#   geom_path(aes(x=inventory_year, y=area, color=land_cover_type)) +
#   geom_point(data=. %>% filter(source=="extrapolated" & land_cover_main == "Built_Up"), shape=21, fill="white",
#              mapping=aes(x=inventory_year, y=area, color=land_cover_type), size=1.2) +
#   geom_point(data=. %>% filter(source=="nlcd" | (source == "extrapolated" & land_cover_main != "Built_Up")), shape=21,
#              mapping=aes(x=inventory_year, y=area, color=land_cover_type, fill=land_cover_type), size=1) +
#   facet_wrap(~county_name)



# Now do the same for CTU scale
builtUp_byCTUYear <- nlcd_ctu %>%
  filter(tcc_available & land_cover_main == "Built_Up") %>%
  pivot_wider(names_from = land_cover_type, values_from = area) %>%
  mutate(Built_Up_Total = Built_Up + Urban_Grassland + Urban_Tree) %>%
  group_by(ctu_name, ctu_class, county_name, inventory_year) %>%
  summarize(
    Urban_Tree_pct = Urban_Tree / Built_Up_Total,
    Urban_Grassland_pct = Urban_Grassland / Built_Up_Total,
    Built_Up_pct = Built_Up / Built_Up_Total
  ) %>%
  slice(c(1, n()))

# Let's extrapolate for years BEFORE tree canopy is available
nlcd_ctu_extrapBefore <- nlcd_ctu %>%
  filter(!tcc_available & land_cover_main == "Built_Up" & inventory_year < tcc_start_year) %>%
  dplyr::select(-c(land_cover_type, total_area, tcc_available)) %>%
  left_join(builtUp_byCTUYear %>% filter(inventory_year == tcc_start_year) %>% dplyr::select(-inventory_year)) %>%
  mutate(
    Urban_Tree = Urban_Tree_pct * area,
    Urban_Grassland = Urban_Grassland_pct * area,
    Built_Up = Built_Up_pct * area
  ) %>%
  dplyr::select(-c(area, ends_with("_pct"))) %>%
  pivot_longer(cols = c(Urban_Tree, Urban_Grassland, Built_Up), names_to = "land_cover_type", values_to = "area") %>%
  mutate(tcc_available = FALSE)

# Now let's extrapolate for years AFTER tree canopy is available
nlcd_ctu_extrapAfter <- nlcd_ctu %>%
  filter(!tcc_available & land_cover_main == "Built_Up" & inventory_year > tcc_end_year) %>%
  dplyr::select(-c(land_cover_type, total_area, tcc_available)) %>%
  left_join(builtUp_byCTUYear %>% filter(inventory_year == tcc_end_year) %>% dplyr::select(-inventory_year)) %>%
  mutate(
    Urban_Tree = Urban_Tree_pct * area,
    Urban_Grassland = Urban_Grassland_pct * area,
    Built_Up = Built_Up_pct * area
  ) %>%
  dplyr::select(-c(area, ends_with("_pct"))) %>%
  pivot_longer(cols = c(Urban_Tree, Urban_Grassland, Built_Up), names_to = "land_cover_type", values_to = "area") %>%
  mutate(tcc_available = FALSE)



nlcd_ctu_final <- rbind(
  nlcd_ctu_extrapBefore,
  nlcd_ctu_extrapAfter,
  nlcd_ctu %>%
    dplyr::select(-total_area) %>%
    filter(!tcc_available & land_cover_main != "Built_Up"),
  nlcd_ctu %>%
    dplyr::select(-total_area) %>%
    filter(tcc_available)
) %>%
  arrange(inventory_year, ctu_name, land_cover_main, land_cover_type) %>%
  mutate(source = case_when(
    tcc_available ~ "nlcd",
    !tcc_available ~ "extrapolated"
  )) %>%
  ungroup() %>%
  group_by(ctu_name, ctu_class, county_name, state_name, inventory_year) %>%
  mutate(total_area = sum(area), .after = "area")




# nlcd_ctu_final %>%
#   filter(ctu_name == "Roseville") %>%
#   ggplot() + theme_minimal() +
#   geom_path(aes(x=inventory_year, y=area, color=land_cover_type)) +
#   geom_point(data=. %>% filter(source=="extrapolated" & land_cover_main == "Built_Up"), shape=21, fill="white",
#              mapping=aes(x=inventory_year, y=area, color=land_cover_type), size=1.2) +
#   geom_point(data=. %>% filter(source=="nlcd" | (source == "extrapolated" & land_cover_main != "Built_Up")), shape=21,
#              mapping=aes(x=inventory_year, y=area, color=land_cover_type, fill=land_cover_type), size=1)






# create metadata
nlcd_county_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "county_id", class(nlcd_county_final$county_id), "County ID (5 digit)",
    "county_name", class(nlcd_county_final$county_name), "County name",
    "state_name", class(nlcd_county_final$state_name), "State name",
    "inventory_year", class(nlcd_county_final$inventory_year), "Year",
    "land_cover_main", class(nlcd_county_final$land_cover_main), "Land cover type from National Land Cover Database. This column ignores the 'Urban_' designation of land_cover_type (see below)",
    "land_cover_type", class(nlcd_county_final$land_cover_type), "Land cover type from National Land Cover Database. 'Urban_' indicates a natural area within NLCD designated developed land cover",
    "area", class(nlcd_county_final$area), "Area of land cover in square kilometers. 'Urban_Tree' is scaled based on the percentage of tree canopy cover within 'Developed' areas",
    "total_area", class(nlcd_county_final$total_area), "Sum of area from all cover types in square kilometers (by county)",
    "tcc_available", class(nlcd_county_final$tcc_available), "Indicates whether tree canopy data was available for the current year",
    "source", class(nlcd_county_final$source), "Indicates whether the area of 'Urban_Tree' or 'Urban_Grassland' is extrapolated or pulled directly from an NLCD layer"
  )

nlcd_ctu_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "county_id", class(nlcd_ctu_final$county_id), "County ID (5 digit)",
    "ctu_id", class(nlcd_ctu_final$ctu_id), "CTU ID",
    "ctu_name", class(nlcd_ctu_final$ctu_name), "CTU name",
    "ctu_class", class(nlcd_ctu_final$county_name), "CTU class",
    "county_name", class(nlcd_ctu_final$county_name), "County name",
    "state_name", class(nlcd_ctu_final$state_name), "State name",
    "inventory_year", class(nlcd_ctu_final$inventory_year), "Year",
    "land_cover_main", class(nlcd_ctu_final$land_cover_main), "Land cover type from National Land Cover Database. This column ignores the 'Urban_' designation of land_cover_type (see below)",
    "land_cover_type", class(nlcd_ctu_final$land_cover_type), "Land cover type from National Land Cover Database. 'Urban_' indicates a natural area within NLCD designated developed land cover",
    "area", class(nlcd_ctu_final$area), "Area of land cover in square kilometers. 'Urban_Tree' is scaled based on the percentage of tree canopy cover within 'Developed' areas",
    "total_area", class(nlcd_ctu_final$total_area), "Sum of area from all cover types in square kilometers (by CTU)",
    "tcc_available", class(nlcd_ctu_final$tcc_available), "Indicates whether tree canopy data was available for the current year",
    "source", class(nlcd_ctu_final$source), "Indicates whether the area of 'Urban_Tree' or 'Urban_Grassland' is extrapolated or pulled directly from an NLCD layer"
  )








# User chooses whether to overwrite the rds files
if (overwrite_RDS) {
  message("Exporting RDS files...")

  saveRDS(nlcd_county_final, paste0("./_nature/data/nlcd_county_landcover_allyrs.rds"))
  saveRDS(nlcd_county_meta, paste0("./_nature/data/nlcd_county_landcover_allyrs_meta.rds"))

  saveRDS(nlcd_ctu_final, paste0("./_nature/data/nlcd_ctu_landcover_allyrs.rds"))
  saveRDS(nlcd_ctu_meta, paste0("./_nature/data/nlcd_ctu_landcover_allyrs_meta.rds"))
}
