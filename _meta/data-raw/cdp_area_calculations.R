### area calculations for CDP

source("R/_load_pkgs.R")

# load the county boundaries layer
cprg_county_7 <- readRDS("_meta/data/cprg_county.RDS") %>% 
  filter(!county_name %in% c("St. Croix","Pierce","Sherburne","Chisago"))

class(cprg_county_7)

regional_area <- sum(st_area(cprg_county_7)) * 1e-6 # square kilometers of total area
#7694.256

nlcd_lc <- get_nlcd(
    cprg_county_7,
    "county",
    year = 2019, ### using 2019 data as 2021 currently not working via FedData package
    dataset = "landcover" # downloads land cover classification data only
  ) %>%
  terra::project(., crs_use)

# convert the counties vector to a terra object
cprg_county <- terra::vect(cprg_county_7)

# define CRS for using with other layers
crs_use <- terra::crs(cprg_county)

nlcd_lc_mask <- terra::mask(nlcd_lc, cprg_county)
nlcd_lc_area <- terra::mask(cellSize(nlcd_lc_mask, unit = "km"), cprg_county)

area_values <- terra::extract(nlcd_lc_area, cprg_county)
lc_values <- terra::extract(nlcd_lc_mask, cprg_county)

lc_df <- as_tibble(data.frame(
  nlcd_cover = lc_values[, 2],
  area = area_values[, 2]
)) %>%
  mutate(
    land_cover_type = case_when(
      grepl("Developed, Open Space", nlcd_cover) ~ "Urban_Grassland",
      grepl("Developed", nlcd_cover) & !grepl("Developed, Open Space", nlcd_cover)~ "Built-up",
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
  ) %>% 
  group_by(land_cover_type) %>% 
  summarize(area = sum(area))


regional_area_nlcd <- lc_df %>% 
  pull(area) %>% sum()

natural_area_nlcd <- lc_df %>% 
  filter(!land_cover_type %in% c("Built-up",
                                 "Bare",
                                 "Cropland")) %>% 
  pull(area) %>% sum()

natural_area_nlcd/regional_area_nlcd
