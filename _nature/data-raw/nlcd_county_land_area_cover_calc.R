rm(list=ls())
source("R/_load_pkgs.R")
source("R/cprg_colors.R")



renv::remove("FedData")

# Install devtools if not already installed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Load devtools
library(devtools)

dev_mode(on=T)

# Install FedData from GitHub
devtools::install_github("ropensci/FedData")

library(FedData)





overwrite_RDS <- FALSE

# load the county boundaries layer
cprg_county <- readRDS("_meta/data/cprg_county.RDS")

# convert the counties vector to a terra object
cprg_county <- terra::vect(cprg_county)

# define CRS for using with other layers
crs_use <- terra::crs(cprg_county)

# Create an empty dataframe to store the results
nlcd_county <- data.frame(
  year = numeric(),
  county = character(),
  land_cover_type = character(),
  area = numeric(),
  stringsAsFactors = FALSE
)

start_year <- 2021
end_year <- 2021


# Loop through all years of NLCD data and extract area estimates by land cover type and by county
lapply(start_year:end_year, function(year) {
  browser()
  message(paste0("\nBeginning year: ", year, "\n"))
  message(paste0(" 0%  |____________________|"))

  # Try to get the tree canopy cover layer
  nlcd_tcc <- try(FedData::get_nlcd( # use FedData::get_nlcd() to load nlcd data
    cprg_county, # area to sample over
    "county", # label for current selection
    year = year, # the current year
    dataset = "canopy" # downloads tree canopy cover data only
  ) %>%
    terra::project(., crs_use), silent = TRUE)

  # Check if the tree canopy cover layer was successfully retrieved
  tcc_available <- !inherits(nlcd_tcc, "try-error")

  message(paste0("10%  |==__________________|"))

  # # Get the impervious surfaces layer
  # nlcd_is <- try(FedData::get_nlcd(
  #   cprg_county,
  #   "county",
  #   year = year,
  #   dataset = "impervious" # downloads impervious surfaces data only
  # ) %>%
  #   terra::project(., crs_use), silent = TRUE)
  #
  # # Check if the impervious surfaces layer was successfully retrieved
  # if (inherits(nlcd_is, "try-error")) {
  #   # Move to the next year if the impervious surfaces layer is missing
  #   return(NULL)
  # }
  # message(paste0('20%  |====________________|'))

  # # Get the land cover type layer
  # nlcd_lc <- try(FedData::get_nlcd(
  #   cprg_county,
  #   "county",
  #   year = year,
  #   dataset = "landcover" # downloads land cover classification data only
  # ) %>%
  #   terra::project(., crs_use), silent = TRUE)

  
  
  # Get the land cover type layer
  nlcd_lc <- try(FedData::get_nlcd_annual(
    cprg_county,
    "county",
    year = year,
    product = "LndCov" # downloads land cover classification data only
  ), silent = TRUE)
  
  
  # %>%
  #   terra::project(., crs_use), silent = TRUE)
  # 
  
  
  NLCD_ANNUAL <-
    FedData::get_nlcd_annual(
      template = FedData::meve,
      label = "meve",
      year = 2021,
      product =
        c(
          "LndCov"
        )
    )
  
  
  
  
  
  
  # Check if the land cover type layer was successfully retrieved
  if (inherits(nlcd_lc, "try-error")) {
    # Move to the next year if the land cover type layer is missing
    message(paste0("\nSTOP! No land cover layers for ", year, ".\nMoving to next year..."))
    return(NULL)
  }



  if (tcc_available) {
    message(paste0("30%  |======______________| - Tree canopy data available for ", year))
    # Perform the necessary calculations for the current year
    nlcd_lc_mask <- terra::mask(nlcd_lc, cprg_county)
    nlcd_lc_area <- terra::mask(cellSize(nlcd_lc_mask, unit = "km"), cprg_county)
    county_raster <- terra::rasterize(cprg_county, nlcd_lc_mask, field = "NAME")

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
    cty_values <- terra::extract(county_raster, cprg_county)

    message(paste0("70%  |==============______|"))


    lc_df <- as_tibble(data.frame(
      county = cty_values[, 2],
      nlcd_cover = nlcd_lc_values[, 2],
      # impervious_cover = as.numeric(as.character(nlcd_is_values[, 2])),
      tree_canopy_cover = as.numeric(as.character(nlcd_tcc_values[, 2])),
      area = area_values[, 2]
    ))

    message(paste0("80%  |================____|"))


    lc_rc <- lc_df %>%
      mutate(
        land_cover_type = case_when(
          grepl("Developed, Open Space", nlcd_cover) & tree_canopy_cover == 0 ~ "Urban_Grassland",
          grepl("Developed", nlcd_cover) & !grepl("Developed, Open Space", nlcd_cover) & tree_canopy_cover > 0 ~ "Urban_Tree",
          grepl("Developed", nlcd_cover) & !grepl("Developed, Open Space", nlcd_cover) & tree_canopy_cover == 0 ~ "Built-up",
          grepl("Deciduous Forest", nlcd_cover) ~ "Tree",
          grepl("Evergreen Forest", nlcd_cover) ~ "Tree",
          grepl("Mixed Forest", nlcd_cover) ~ "Tree",
          grepl("Dwarf Scrub", nlcd_cover) ~ "Shrubland",
          grepl("Shrub/Scrub", nlcd_cover) ~ "Shrubland",
          grepl("Grassland/Herbaceous", nlcd_cover) ~ "Grassland",
          grepl("Sedge/Herbaceous", nlcd_cover) ~ "Grassland",
          grepl("Cultivated Crops", nlcd_cover) ~ "Cropland",
          grepl("Pasture/Hay", nlcd_cover) ~ "Cropland",
          grepl("Barren Land", nlcd_cover) ~ "Bare",
          grepl("Perennial Ice/Snow", nlcd_cover) ~ "Snow",
          grepl("Open Water", nlcd_cover) ~ "Water",
          grepl("Woody Wetlands", nlcd_cover) ~ "Tree", ## Changed from "Wetland"
          grepl("Emergent Herbaceous Wetlands", nlcd_cover) ~ "Wetland"
        )
      ) %>%
      mutate(
        area_corrected = if_else(land_cover_type == "Urban_Tree", area * (tree_canopy_cover / 100), area)
      )


    message(paste0("90%  |==================__|"))

    lc_county <- lc_rc %>%
      group_by(county, land_cover_type) %>%
      summarize(area = sum(area_corrected), .groups = "keep")


    # Add the results for the current year to the results dataframe
    nlcd_county <<- rbind(nlcd_county, data.frame(
      year = year,
      county = lc_county$county,
      land_cover_type = lc_county$land_cover_type,
      area = lc_county$area,
      stringsAsFactors = FALSE
    ))

    message(paste0("100% |====================|"))
  } else {
    message(paste0("30%  |======______________| - Tree canopy data NOT available for ", year))

    # Perform the necessary calculations for the current year
    nlcd_lc_mask <- terra::mask(nlcd_lc, cprg_county)
    nlcd_lc_area <- terra::mask(cellSize(nlcd_lc_mask, unit = "km"), cprg_county)
    county_raster <- terra::rasterize(cprg_county, nlcd_lc_mask, field = "NAME")

    message(paste0("40%  |========____________|"))

    nlcd_lc_values <- terra::extract(nlcd_lc, cprg_county)
    # nlcd_is_values <- terra::extract(nlcd_is, cprg_county)
    # nlcd_tcc_values <- terra::extract(nlcd_tcc, cprg_county)

    message(paste0("50%  |==========__________|"))

    # nlcd_is_values <- nlcd_is_values %>%
    #   modify_if(is.numeric, ~replace_na(., 0))

    # nlcd_tcc_values <- nlcd_tcc_values %>%
    #   modify_if(is.numeric, ~replace_na(., 0))

    message(paste0("60%  |============________|"))

    area_values <- terra::extract(nlcd_lc_area, cprg_county)
    lc_values <- terra::extract(nlcd_lc_mask, cprg_county)
    cty_values <- terra::extract(county_raster, cprg_county)

    message(paste0("70%  |==============______|"))

    browser()

    lc_df <- as_tibble(data.frame(
      county = cty_values[, 2],
      nlcd_cover = nlcd_lc_values[, 2],
      # impervious_cover = as.numeric(as.character(nlcd_is_values[, 2])),
      tree_canopy_cover = as.numeric(NA),
      area = area_values[, 2]
    ))

    message(paste0("80%  |================____|"))


    lc_rc <- lc_df %>%
      mutate(
        land_cover_type = case_when(
          # grepl("Developed, Open Space", nlcd_cover) & tree_canopy_cover == 0 ~ "Urban_Grassland",
          # grepl("Developed", nlcd_cover) & !grepl("Developed, Open Space", nlcd_cover) & tree_canopy_cover > 0 ~ "Urban_Tree",
          # grepl("Developed", nlcd_cover) & !grepl("Developed, Open Space", nlcd_cover) & tree_canopy_cover == 0 ~ "Built-up",

          grepl("Developed", nlcd_cover) ~ "Built-up",
          grepl("Deciduous Forest", nlcd_cover) ~ "Tree",
          grepl("Evergreen Forest", nlcd_cover) ~ "Tree",
          grepl("Mixed Forest", nlcd_cover) ~ "Tree",
          grepl("Dwarf Scrub", nlcd_cover) ~ "Shrubland",
          grepl("Shrub/Scrub", nlcd_cover) ~ "Shrubland",
          grepl("Grassland/Herbaceous", nlcd_cover) ~ "Grassland",
          grepl("Sedge/Herbaceous", nlcd_cover) ~ "Grassland",
          grepl("Cultivated Crops", nlcd_cover) ~ "Cropland",
          grepl("Pasture/Hay", nlcd_cover) ~ "Cropland",
          grepl("Barren Land", nlcd_cover) ~ "Bare",
          grepl("Perennial Ice/Snow", nlcd_cover) ~ "Snow",
          grepl("Open Water", nlcd_cover) ~ "Water",
          grepl("Woody Wetlands", nlcd_cover) ~ "Tree", ## Changed from "Wetland"
          grepl("Emergent Herbaceous Wetlands", nlcd_cover) ~ "Wetland"
        )
      ) # %>%
    #
    # mutate(
    #   area_corrected = if_else(land_cover_type == "Urban_Tree", area * (tree_canopy_cover / 100), area)
    # )


    message(paste0("90%  |==================__|"))

    lc_county <- lc_rc %>%
      group_by(county, land_cover_type) %>%
      summarize(area = sum(area), .groups = "keep")

    browser()


    # Add the results for the current year to the results dataframe
    nlcd_county <<- rbind(nlcd_county, data.frame(
      year = year,
      county = lc_county$county,
      land_cover_type = lc_county$land_cover_type,
      area = lc_county$area,
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
    "county ", class(nlcd_county$county), "County name",
    "land_cover_type", class(nlcd_county$land_cover_type), "Land cover type from National Land Cover Database. 'Urban_' indicates a natural area within NLCD designated developed land cover",
    "area", class(nlcd_county$area), "Area of land cover in square kilometers. 'Urban_Tree' is scaled based on the percentage of tree canopy cover within 'Developed' areas"
  )



# User chooses whether to overwrite the rds files
if (overwrite_RDS) {
  saveRDS(nlcd_county, paste0("./_nature/data/nlcd_county_landcover_", head(sort(unique(nlcd_county$year)), 1), "_", tail(sort(unique(nlcd_county$year)), 1), ".rds"))
  saveRDS(nlcd_county_meta, paste0("./_nature/data/nlcd_county_landcover_", head(sort(unique(nlcd_county$year)), 1), "_", tail(sort(unique(nlcd_county$year)), 1), "_meta.rds"))
}








### LOOK INTO LAKE CARBON SEQUESTRATION
