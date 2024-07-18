source("R/_load_pkgs.R")
source("R/cprg_colors.R")

## stock and sequestration values
land_cover_c <- readRDS("./_nature/data/land_cover_carbon.rds")


### seven county shapefile
cprg_county <- readRDS("_meta/data/cprg_county.RDS") %>% 
  filter(!NAME %in% c("Chisago","Sherburne","St. Croix","Pierce"))

### seven county land cover (NLCD)
county_lc <- readRDS("_nature/data/nlcd_county_landcover_2001_2021.rds") %>% 
  filter(!county %in% c("Chisago","Sherburne","St. Croix","Pierce"),
         year == 2021)

### parks land cover (ESA)
# parks_esa <- readRDS("_nature/data/worldcover_parks.rds") #raster
parks_esa_lc <- readRDS("_nature/data/park_landcover_2021.rds")

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

parks <- vect("./_nature/data-raw/DBO_RPTAdministrativeBoundary.shp")%>% 
  project(., crs_use)

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
    data.frame(agency = parks_use$AgencyMana, sqkm = parks_use$AcresCalcu / 247.1) %>% 
    group_by(agency) %>% 
    summarize(park_area_sqkm = sum(sqkm))%>%
      mutate(geog_name = str_extract(agency, "^[^ ]+"),
             geog_name = if_else(geog_name == "Three", "Hennepin", geog_name),
             geog_name = if_else(geog_name == "Saint", "Saint Paul", geog_name)) %>% 
      select(geog_name, park_area_sqkm)
           ) %>% 
  mutate(nonpark_area_sqkm = geog_area - park_area_sqkm) %>% 
  select(-geog_area)


nlcd_lc_parks <- get_nlcd(
  parks_use,
  "county",
  year = 2021,
  dataset = "landcover"
) %>%
  terra::project(., crs_use) # reproject
 

nlcd_tcc_parks <- FedData::get_nlcd( # use FedData::get_nlcd() to load nlcd data
  parks_use, # area to sample over
  "county", # label for current selection
  year = 2021, # the current year
  dataset = "canopy" # downloads tree canopy cover data only
) %>%
  terra::project(., crs_use)

### extract needed data and repackage into data frame
nlcd_lc_mask_parks <- terra::mask(nlcd_lc_parks, parks_use)
nlcd_tcc_mask_parks <- terra::mask(nlcd_tcc_parks, parks_use)
nlcd_lc_area_parks <- terra::mask(cellSize(nlcd_lc_mask_parks, unit = "km"), parks_use)
parks_raster <- terra::rasterize(parks_use, nlcd_lc_mask_parks, field = "AgencyMana")

nlcd_lc_values_parks <- terra::extract(nlcd_lc_parks, parks_use)
nlcd_tcc_values_parks <- terra::extract(nlcd_tcc_parks, parks_use)
nlcd_tcc_values_parks <- nlcd_tcc_values_parks %>%
  modify_if(is.numeric, ~ replace_na(., 0))
area_values_parks <- terra::extract(nlcd_lc_area_parks, parks_use)
parks_values <- terra::extract(parks_raster, parks_use)


lc_df <- as_tibble(data.frame(
  park = parks_values[, 2],
  nlcd_cover = nlcd_lc_values_parks[, 2],
  # impervious_cover = as.numeric(as.character(nlcd_is_values[, 2])),
  tree_canopy_cover = as.numeric(as.character(nlcd_tcc_values_parks[, 2])),
  area = area_values_parks[, 2]
))

# reduce to categories of interest
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

# summarize and bring in sequestration and stock values
lc_park <- lc_rc %>%
  group_by(park, land_cover_type) %>%
  summarize(area = sum(area_corrected), .groups = "keep") %>%  
  mutate(geog_name = str_extract(park, "^[^ ]+"),
         geog_name = if_else(geog_name == "Three", "Hennepin", geog_name),
         geog_name = if_else(geog_name == "Saint", "Saint Paul", geog_name)) %>% 
  ungroup() %>% 
  select(-park)

park_c <- left_join(lc_park, land_cover_c) %>% 
  filter(!is.na(stock_mtco2e_sqkm)) %>% 
  mutate(mtco2e_seq_yr = seq_mtco2e_sqkm * area,
         mtco2e_stock = stock_mtco2e_sqkm * area) %>% 
  group_by(geog_name) %>% 
  summarize(mtco2e_seq_yr_park = sum(mtco2e_seq_yr),
            mtco2e_stock_park = sum(mtco2e_stock)) %>% 
  filter(!is.na(geog_name))  %>% 
  select(geog_name, mtco2e_seq_yr_park, mtco2e_stock_park)

### get cities land cover type 
  
nlcd_lc_cities <- get_nlcd(
  cities,
  "county",
  year = 2021,
  dataset = "landcover"
) %>%
  terra::project(., crs_use) # reproject


nlcd_tcc_cities <- FedData::get_nlcd( # use FedData::get_nlcd() to load nlcd data
  cities, # area to sample over
  "county", # label for current selection
  year = 2021, # the current year
  dataset = "canopy" # downloads tree canopy cover data only
) %>%
  terra::project(., crs_use)

### extract needed data and repackage into data frame
nlcd_lc_mask_cities <- terra::mask(nlcd_lc_cities, cities)
nlcd_tcc_mask_cities <- terra::mask(nlcd_tcc_cities, cities)
nlcd_lc_area_cities <- terra::mask(cellSize(nlcd_lc_mask_cities, unit = "km"), cities)
cities_raster <- terra::rasterize(cities, nlcd_lc_mask_cities, field = "CTU_NAME")

nlcd_lc_values_cities <- terra::extract(nlcd_lc_cities, cities)
nlcd_tcc_values_cities <- terra::extract(nlcd_tcc_cities, cities)
nlcd_tcc_values_cities <- nlcd_tcc_values_cities %>%
  modify_if(is.numeric, ~ replace_na(., 0))
area_values_cities <- terra::extract(nlcd_lc_area_cities, cities)
cities_values <- terra::extract(cities_raster, cities)


lc_df <- as_tibble(data.frame(
  city = cities_values[, 2],
  nlcd_cover = nlcd_lc_values_cities[, 2],
  # impervious_cover = as.numeric(as.character(nlcd_is_values[, 2])),
  tree_canopy_cover = as.numeric(as.character(nlcd_tcc_values_cities[, 2])),
  area = area_values_cities[, 2]
))

# reduce to categories of interest
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

# summarize and bring in sequestration and stock values
lc_city <- lc_rc %>%
  group_by(city, land_cover_type) %>%
  summarize(area = sum(area_corrected), .groups = "keep") %>% 
  rename(geog_name = city)

lc_city_county <- lc_rc %>% 
  mutate(county = if_else(city == "Saint Paul","Ramsey","Hennepin")) %>% 
  group_by(county, land_cover_type) %>%
  summarize(area = sum(area_corrected), .groups = "keep")
  
### need to first subtract city lc areas from counties, then park areas from relevant geogs
lc_county <- left_join(county_lc %>% filter(!is.na(land_cover_type)),
                     lc_city_county,
                     by = c("county","land_cover_type")) %>% 
  mutate(area.y = replace_na(area.y, 0),
         area = area.x - area.y) %>% 
  select(geog_name = county, land_cover_type,area)

lc_geog <- bind_rows(lc_city,lc_county) %>% 
  left_join(., lc_park, by = c("geog_name", "land_cover_type"), suffix = c("_geog","_park")) %>% 
  mutate(area_nonpark = area_geog - area_park)

geog_c <- left_join(lc_geog, land_cover_c) %>% 
  filter(!is.na(stock_mtco2e_sqkm)) %>% 
  mutate(mtco2e_seq_yr = seq_mtco2e_sqkm * area_nonpark ,
         mtco2e_stock = stock_mtco2e_sqkm * area_nonpark ) %>% 
  group_by(geog_name) %>% 
  summarize(mtco2e_seq_yr_nonpark = sum(mtco2e_seq_yr),
            mtco2e_stock_nonpark = sum(mtco2e_stock)) %>% 
  filter(!is.na(geog_name))  %>% 
  select(geog_name, mtco2e_seq_yr_nonpark, mtco2e_stock_nonpark)
  
out <- left_join(areas, geog_c) %>% 
  left_join(., park_c)

# write_csv(out,
#           "_nature/data/implementing_agency_park_c_nlcd_2021.csv")

### wetlands

park_wetland_acres <- lc_park %>% filter(land_cover_type == "Wetland") %>% mutate(area = area * 247.1) %>% pull(area) %>% sum()
county_wetland_acres <- county_lc %>% filter(land_cover_type == "Wetland") %>% mutate(area = area * 247.1) %>% pull(area) %>% sum()
park_wetland_acres / county_wetland_acres

mn <- tigris::states() %>% filter(NAME == "Minnesota")

nlcd_lc_mn <- get_nlcd(
  mn,
  "state",
  year = 2021,
  dataset = "landcover"
) %>%
  terra::project(., crs_use)

area_values_mn <- terra::extract(nlcd_lc_mn, mn)
mn_values <- terra::extract(nlcd_lc_mn, mn)
