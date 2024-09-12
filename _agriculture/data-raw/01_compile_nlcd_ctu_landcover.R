source("R/_load_pkgs.R")
source("R/cprg_colors.R")

# load the county boundaries layer
cprg_county <- readRDS("_meta/data/cprg_county.RDS")

cprg_ctu_7 <- councilR::import_from_gpkg("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_dot/bdry_mn_city_township_unorg/gpkg_bdry_mn_city_township_unorg.zip") %>%
  filter(COUNTY_NAM %in% c(cprg_county$county_name)) %>%
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
  arrange(CTU_NAME)


# convert the counties vector to a terra object
cprg_ctu_7 <- terra::vect(cprg_ctu_7)

# define CRS for using with other layers
crs_use <- terra::crs(cprg_ctu_7)

# Create an empty dataframe to store the results
nlcd_ctu <- data.frame(
  year = numeric(),
  county = character(),
  land_cover_type = character(),
  area = numeric(),
  stringsAsFactors = FALSE
)

start_year <- 2001
end_year <- 2019 # 2021 is currently causing issues, loading incorrect tif files in addition to the correct one


# Loop through all years of NLCD data and extract area estimates by land cover type and by ctu
lapply(start_year:end_year, function(year) {
  # browser()
  message(paste0("\nBeginning year: ", year, "\n"))
  message(paste0(" 0%  |____________________|"))

  # Get the land cover type layer
  nlcd_lc <- try(FedData::get_nlcd(
    template =cprg_ctu_7,
    label = "city_name",
    year = 2019,
    dataset = "landcover" # downloads land cover classification data only
  ) %>%
    terra::project(., crs_use), silent = TRUE)

  # Check if the land cover type layer was successfully retrieved
  if (inherits(nlcd_lc, "try-error")) {
    # Move to the next year if the land cover type layer is missing
    message(paste0("\nSTOP! No land cover layers for ", year, ".\nMoving to next year..."))
    return(NULL)
  }

  nlcd_lc_mask <- terra::mask(nlcd_lc, cprg_ctu_7)
  nlcd_lc_area <- terra::mask(cellSize(nlcd_lc_mask, unit = "km"), cprg_ctu_7)
  ctu_raster <- terra::rasterize(cprg_ctu_7, nlcd_lc_mask, field = "CTU_NAME")
  nlcd_lc_values <- terra::extract(nlcd_lc, cprg_ctu_7)
  

    area_values <- terra::extract(nlcd_lc_area, cprg_ctu_7)
    lc_values <- terra::extract(nlcd_lc_mask, cprg_ctu_7)
    ctu_values <- terra::extract(ctu_raster, cprg_ctu_7)

    lc_df <- as_tibble(data.frame(
      ctu = ctu_values[, 2],
      nlcd_cover = nlcd_lc_values[, 2],
      # impervious_cover = as.numeric(as.character(nlcd_is_values[, 2])),
      area = area_values[, 2]
    ))

    lc_rc <- lc_df %>%
      mutate(
        land_cover_type = case_when(
          grepl("Developed", nlcd_cover) ~ "Developed",
          grepl("Developed, Open Space", nlcd_cover) ~ "Urban_Grassland",
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
      )

    lc_ctu <- lc_rc %>%
      group_by(ctu, land_cover_type) %>%
      summarize(area = sum(area), .groups = "keep")

    # Add the results for the current year to the results dataframe
    nlcd_ctu <<- rbind(nlcd_ctu, data.frame(
      year = year,
      ctu = lc_ctu$ctu,
      land_cover_type = lc_ctu$land_cover_type,
      area = lc_ctu$area,
      stringsAsFactors = FALSE
    ))

  } 
)


nlcd_ctu <- nlcd_ctu %>% 
  rename(ctu = county)



# create metadata
nlcd_ctu_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "year", class(nlcd_ctu$year), "Year",
    "ctu ", class(nlcd_ctu$ctu), "City, township, or unorganized territory",
    "land_cover_type", class(nlcd_ctu$land_cover_type), "Land cover type from National Land Cover Database",
    "area", class(nlcd_ctu$area), "Area of land cover in square kilometers"
  )

saveRDS(nlcd_ctu, "./_agriculture/data/nlcd_ctu_landcover.rds")
saveRDS(nlcd_ctu_meta, "./_agriculture/data/nlcd_ctu_landcover_meta.rds")
