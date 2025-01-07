rm(list=ls())
source("R/_load_pkgs.R")
source("R/cprg_colors.R")

overwrite_RDS <- TRUE

# Goal is to take map of different waterways from NHD and derive area estimates
# for different waterway types (e.g. LakePond, StreamRiver, DamWeir).
# We can then aggregate area to different geographic boundaries (CTUs, counties).

message("Loading geoms")

# load the county and ctu boundaries layer
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_ctu <- readRDS("_meta/data/cprg_ctu.RDS")

# tmap::tm_shape(cprg_county) +
#   tm_borders() 


# National Hydrography Dataset --------------------------------------------
# NHD Features Overview
# 
## Line features ##
# -NHDFlowline is the fundamental flow network consisting predominantly of stream/river 
#  and artificial path vector features. It represents the spatial geometry, carries the 
#  attributes, and contains linear referencing measures for locating features or “events” 
#  on the network. Additional NHDFlowline features are canal/ditch, pipeline, connector, 
#  underground conduit, and coastline.
# 
# -NHDLine contains linear features not core to the network.
# 
# 
## Area features ##
# -NHDWaterbodies such as lake/pond features are represented in NHDWaterbody. They 
#  portray the spatial geometry and the attributes of the feature. These water polygons 
#  may have NHDFlowline artificial paths drawn through them to allow the representation of 
#  water flow direction. Other NHDWaterbody features are swamp/marsh, reservoir, playa, 
#  estuary, and ice mass. 
# 
# -NHDArea contains many additional water-polygon features. One of the more important 
#  is the stream/river feature. It represents the areal extent of the water in a wide 
#  stream/river with a basic set of attributes. These polygons typically encompass NHDFlowline 
#  artificial paths that represent the stream network. Artificial path carries the critical 
#  attributes of the stream/river, whereas NHDArea represents the geometric extent.
# 
# 
## Point features ##
# -NHDPoint contains hydrography related point features.
# 
# 
## Events ##
# -NHDPointEventFC, NHDLineEventFC, and NHDAreaEventFC represent point, line, and area 
#  data events that behave as map features and linearly referenced events. Streamgages, 
#  which are point features, can be displayed and identified in the network through linear 
#  referencing with a network address.
# 
# 
## Tables ##
# -Information about the NHD also can be obtained in a series of associated tables. 
#  This includes metadata stored in NHDFeaturetoMetadata and NHDMetadata, sources given 
#  in NHDSourceCitation, identification of model and data version given in NHDProcessingParameters, 
#  flow relations given in NHDFlow, reach code histories given in NHDReachCrossReference, the 
#  domain of feature codes given in NHDFCode, and others.

message("Loading NHD images")

# Define the input path
inpath_nhd_all <- paste0(here::here(), "/_nature/data-raw/nhd_gdb/")

# List all .gdb file paths
gdb_files <- list.dirs(inpath_nhd_all, 
                       recursive = TRUE, 
                       full.names = TRUE)[grepl("\\.gdb", 
                                                list.dirs(inpath_nhd_all, 
                                                          recursive = TRUE, 
                                                          full.names = TRUE))]



# Define a function to read specific layers from multiple .gdb files
read_gdb_layers <- function(gdb_path, layer_name) {
  st_read(gdb_path, layer = layer_name) %>%
    mutate(source = gdb_path) # Optional: Add source file info
}

# Use lapply to read the "NHDWaterbody" layer from all .gdb files
NHDWaterbody_list <- lapply(gdb_files, read_gdb_layers, layer_name = "NHDWaterbody")
# Combine all the data frames into one
NHDWaterbody_combined <- bind_rows(NHDWaterbody_list)
rm(NHDWaterbody_list)


# Use lapply to read the "NHDArea" layer from all .gdb files
NHDArea_list <- lapply(gdb_files, read_gdb_layers, layer_name = "NHDArea")
# Combine all the data frames into one
NHDArea_combined <- bind_rows(NHDArea_list)
rm(NHDArea_list)



# # Use lapply to read the "NHDFlowline" layer from all .gdb files
# NHDFlowline_list <- lapply(gdb_files, read_gdb_layers, layer_name = "NHDFlowline")
# # Combine all the data frames into one
# NHDFlowline_combined <- bind_rows(NHDFlowline_list)
# rm(NHDFlowline_list)
# # Convert the coordinate system to 4269 (ideal for MN)
# NHDFlowline_combined <- NHDFlowline_combined %>% sf::st_transform(4269)
# 
# # st_make_valid can help fix that
# NHDFlowline_combined <- sf::st_make_valid(NHDFlowline_combined)
# 
# # Now let's compute the area for each type of waterway by county and CTU
# # first add an area column to the NHD dataframe, then convert to km2
# NHD$area <- sf::st_area(NHD)
# NHD$area_km2 <- NHD$area / 1e6



message("Binding NHD data")

NHD <- rbind(NHDArea_combined %>%
               dplyr::select(NHD_ID=permanent_identifier, FTYPE = ftype, 
                             GNIS_ID = gnis_id, GNIS_NAME = gnis_name) %>%
               mutate(source="NHDArea"),
             NHDWaterbody_combined %>%
               dplyr::select(NHD_ID=permanent_identifier, FTYPE = ftype, 
                             GNIS_ID = gnis_id, GNIS_NAME = gnis_name) %>%
               mutate(source="NHDWaterbody")) %>%
  relocate(source, .before=everything())

# 
# # Select only the columns that match between the two dataframes
# NHDArea_combined <- NHDArea_combined[, colnames(NHDArea_combined) %in% colnames(NHDWaterbody_combined)]
# NHDWaterbody_combined <- NHDWaterbody_combined[, colnames(NHDWaterbody_combined) %in% colnames(NHDArea_combined)]

# # Merge the two dataframes 
# NHD <- rbind(NHDArea_combined, NHDWaterbody_combined)
rm(NHDArea_combined,NHDWaterbody_combined)
# Convert the coordinate system to 4269 (ideal for MN)
NHD <- NHD %>% sf::st_transform(4269)


# st_transform can make things kinda weird with geoms,
# st_make_valid can help fix that
NHD <- sf::st_make_valid(NHD)





# CTU level estimates -----------------------------------------------------
# Convert the coordinate system to 4269 (ideal for MN)
cprg_ctu <- sf::st_set_crs(cprg_ctu, 4269)
# do this prior to the intersection call, otherwise st_intersection won't work
cprg_ctu <- st_make_valid(cprg_ctu)

# create empty list
geom_list <- list()

cprg_ctu <- cprg_ctu %>%
  mutate(tmpID = paste0(geoid_wis, "_", gnis))

tmpNames <- unique(cprg_ctu$tmpID)

geom_list <- lapply(seq_along(tmpNames), function(i) {
  message("running ", i, " of ", length(tmpNames))
  x <- cprg_ctu %>% dplyr::filter(tmpID == tmpNames[[i]]) %>% sf::st_make_valid()
  crop <- sf::st_crop(NHD, x) %>% sf::st_make_valid()
  
  # remove invalid polygons
  notvalid <- which(s2::s2_is_valid_detail(crop) == FALSE)
  if (length(notvalid) > 0) {
    crop <- crop[-notvalid, ]
  }
  
  # intersection between x and crop
  x2 <- crop %>% sf::st_intersection(x) %>% sf::st_make_valid()
  
  # return
  x2
})

browser()

NHD_byCTU <- bind_rows(geom_list, .id = "column_label") %>%
  dplyr::select(-column_label, tmpID)

NHD_byCTU <- NHD_byCTU %>%
  distinct(SHAPE, .keep_all = TRUE) 

# The st_intersection call above can cause issues with plotting, 
# st_make_valid can help fix that
NHD_byCTU <- sf::st_make_valid(NHD_byCTU)


message("Computing waterway area by CTU")

NHD_byCTU$area <- sf::st_area(NHD_byCTU)
NHD_byCTU$area_km2 <- NHD_byCTU$area / 1e6



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



message("Wrapping up")


# create metadata ---------------------------------------------------------
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
  
  saveRDS(nhd_ctu, paste0("./_nature/data/nhd_ctu_waterways_", head(sort(unique(nhd_ctu$year)), 1), "_", tail(sort(unique(nhd_ctu$year)), 1), ".rds"))
  saveRDS(nhd_ctu_meta, paste0("./_nature/data/nhd_ctu_waterways_", head(sort(unique(nhd_ctu$year)), 1), "_", tail(sort(unique(nhd_ctu$year)), 1), "_meta.rds"))
  
}


