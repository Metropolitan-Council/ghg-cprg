rm(list=ls())
source("R/_load_pkgs.R")
source("R/cprg_colors.R")

overwrite_RDS <- TRUE


# Goal is to take map of different waterways from NHD and derive area estimates
# for different waterway types (e.g. LakePond, StreamRiver, DamWeir).
# We can then aggregate area to different geographic boundaries (CTUs, counties).

# load the county and ctu boundaries layer
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_ctu <- readRDS("_meta/data/cprg_ctu.RDS")


# Use the FedData package function "get_nhd()" to grab waterways shapefiles from
# the National Hydrography Dataset. 
nhd_all <- FedData::get_nhd(
  template=cprg_county,
  label="MSA"
)

# Select only the columns that match between the two dataframes
nhd_all$Waterbody <- nhd_all$Waterbody[, colnames(nhd_all$Waterbody) %in% colnames(nhd_all$Area)]
nhd_all$Area <- nhd_all$Area[, colnames(nhd_all$Area) %in% colnames(nhd_all$Waterbody)]

# Merge the two dataframes 
NHD <- rbind(nhd_all$Waterbody, nhd_all$Area)
# Convert the coordinate system to 4269 (ideal for MN)
NHD <- NHD %>% sf::st_transform(4269)


# Now let's compute the area for each type of waterway by county and CTU
# first add an area column to the NHD dataframe, then convert to km2
NHD$area <- sf::st_area(NHD)
NHD$area_km2 <- NHD$area / 1e6



# County level estimates --------------------------------------------------
# Intersect the county level geometry with the NHD data
NHD_byCounty <- sf::st_intersection(NHD, cprg_county)
# The st_intersection call above can cause issues with plotting, 
# st_make_valid can help fix that
NHD_byCounty <- sf::st_make_valid(NHD_byCounty)

# Compute area of each waterway type by county
nhd_county <- NHD_byCounty %>% 
  sf::st_drop_geometry() %>% # drop geom features
  group_by(FTYPE, county_name) %>% # group by waterway type and county
  summarise(area = as.numeric(sum(area_km2)), .groups="keep") %>% # compute total area
  ungroup() %>% as_tibble() %>%
  mutate(FTYPE = case_when(
    FTYPE == 390 ~ "LakePond",
    FTYPE == 436 ~ "Reservoir",
    FTYPE == 466 ~ "SwampMarsh",
    FTYPE == 460 ~ "StreamRiver",
    FTYPE == 398 ~ "Lock Chamber",
    FTYPE == 343 ~ "DamWeir"
  )) %>%
  mutate(FTYPE = factor(FTYPE, levels = c("LakePond", "Reservoir", "SwampMarsh", "StreamRiver", "Lock Chamber", "DamWeir"))) %>%
  arrange(county_name, FTYPE) %>%
  left_join(cprg_county %>%
              sf::st_drop_geometry(), by="county_name") %>%
  rename(waterway_type = FTYPE) %>%
  relocate(any_of(colnames(cprg_county)), waterway_type, area) %>%
  # use crossing() to create a simulated time series of area change between 2001 and 2021
  # in reality this just duplicates each row in the dataframe for every year in the sequence
  crossing(year=seq(2001,2021, by=1)) %>% 
  dplyr::select(c("county_name", "state_name", "year", "waterway_type", "area")) %>%
  arrange(year, county_name, waterway_type)



# CTU level estimates -----------------------------------------------------
# Convert the coordinate system to 4269 (ideal for MN)
cprg_ctu <- sf::st_set_crs(cprg_ctu, 4269)
# do this prior to the intersection call, otherwise st_intersection won't work
cprg_ctu <- st_make_valid(cprg_ctu)
# Intersect the ctu level geometry with the NHD data
NHD_byCTU <- sf::st_intersection(NHD, cprg_ctu)
NHD_byCTU <- st_make_valid(NHD_byCTU)


nhd_ctu <- NHD_byCTU %>% 
  sf::st_drop_geometry() %>% # drop geom features
  group_by(FTYPE, ctu_name, ctu_class, county_name, state_name, statefp, state_abb) %>%
  summarise(area = as.numeric(sum(area_km2)), .groups="keep") %>% # compute total area
  ungroup() %>% as_tibble() %>%
  mutate(FTYPE = case_when(
    FTYPE == 390 ~ "LakePond",
    FTYPE == 436 ~ "Reservoir",
    FTYPE == 466 ~ "SwampMarsh",
    FTYPE == 460 ~ "StreamRiver",
    FTYPE == 398 ~ "Lock Chamber",
    FTYPE == 343 ~ "DamWeir"
  )) %>%
  mutate(FTYPE = factor(FTYPE, levels = c("LakePond", "Reservoir", "SwampMarsh", "StreamRiver", "Lock Chamber", "DamWeir"))) %>%
  arrange(ctu_name, FTYPE) %>%
  left_join(cprg_ctu %>%
              sf::st_drop_geometry(), 
            by = join_by(ctu_name, ctu_class, county_name, state_name, statefp, state_abb)) %>%
  rename(waterway_type = FTYPE) %>%
  relocate(any_of(colnames(cprg_ctu)), waterway_type, area) %>%
  # use crossing() to create a simulated time series of area change between 2001 and 2021
  # in reality this just duplicates each row in the dataframe for every year in the sequence
  crossing(year=seq(2001,2021, by=1)) %>% 
  dplyr::select(c("ctu_name", "ctu_class", "county_name", "state_name", "year", "waterway_type", "area"))%>%
  arrange(year, ctu_name, waterway_type)







# create metadata ---------------------------------------------------------
nhd_county_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "county_name", class(nhd_county$county_name), "County name",
    "state_name", class(nhd_county$state_name), "State name",
    "year", class(nhd_county$year), "Year",
    "waterway_type", class(nhd_county$waterway_type), "Waterway type from National Hydrogaphy Dataset",
    "area", class(nhd_county$area), "Area of land cover in square kilometers"
  )

nhd_ctu_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "ctu_name", class(nhd_ctu$ctu_name), "CTU name",
    "ctu_class", class(nhd_ctu$county_name), "CTU class",
    "county_name", class(nhd_ctu$county_name), "County name",
    "state_name", class(nhd_ctu$state_name), "State name",
    "year", class(nhd_ctu$year), "Year",
    "waterway_type", class(nhd_ctu$waterway_type), "Waterway type from National Hydrogaphy Dataset",
    "area", class(nhd_ctu$area), "Area of land cover in square kilometers"
  )




# User chooses whether to overwrite the rds files
if (overwrite_RDS) {
  message("Exporting RDS files...")
  
  saveRDS(nhd_county, paste0("./_nature/data/nhd_county_waterways_", head(sort(unique(nhd_county$year)), 1), "_", tail(sort(unique(nhd_county$year)), 1), ".rds"))
  saveRDS(nhd_county_meta, paste0("./_nature/data/nhd_county_waterways_", head(sort(unique(nhd_county$year)), 1), "_", tail(sort(unique(nhd_county$year)), 1), "_meta.rds"))
  
  saveRDS(nhd_ctu, paste0("./_nature/data/nhd_ctu_waterways_", head(sort(unique(nhd_ctu$year)), 1), "_", tail(sort(unique(nhd_ctu$year)), 1), ".rds"))
  saveRDS(nhd_ctu_meta, paste0("./_nature/data/nhd_ctu_waterways_", head(sort(unique(nhd_ctu$year)), 1), "_", tail(sort(unique(nhd_ctu$year)), 1), "_meta.rds"))
}




















