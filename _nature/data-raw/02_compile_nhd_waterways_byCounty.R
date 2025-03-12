rm(list = ls())
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
  full.names = TRUE
)[grepl(
  "\\.gdb",
  list.dirs(inpath_nhd_all,
    recursive = TRUE,
    full.names = TRUE
  )
)]



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



# Use lapply to read the "NHDFlowline" layer from all .gdb files
NHDFlowline_list <- lapply(gdb_files, read_gdb_layers, layer_name = "NHDFlowline")
# Combine all the data frames into one
NHDFlowline_combined <- bind_rows(NHDFlowline_list)
rm(NHDFlowline_list)
# # Convert the coordinate system to 4269 (ideal for MN)
# NHDFlowline_combined <- NHDFlowline_combined %>% sf::st_transform(4269)
#
# # st_make_valid can help fix that
# NHDFlowline_combined <- sf::st_make_valid(NHDFlowline_combined)
#
# # # Now let's compute the area for each type of waterway by county and CTU
# # # first add an area column to the NHD dataframe, then convert to km2
# # NHD$area <- sf::st_area(NHD)
# # NHD$area_km2 <- NHD$area / 1e6



message("Binding NHD data")

NHD <- rbind(
  NHDArea_combined %>%
    dplyr::select(
      NHD_ID = permanent_identifier, FTYPE = ftype,
      GNIS_ID = gnis_id, GNIS_NAME = gnis_name
    ) %>%
    mutate(source = "NHDArea"),
  NHDWaterbody_combined %>%
    dplyr::select(
      NHD_ID = permanent_identifier, FTYPE = ftype,
      GNIS_ID = gnis_id, GNIS_NAME = gnis_name
    ) %>%
    mutate(source = "NHDWaterbody"),
  NHDFlowline_combined %>%
    dplyr::select(
      NHD_ID = permanent_identifier, FTYPE = ftype,
      GNIS_ID = gnis_id, GNIS_NAME = gnis_name
    ) %>%
    mutate(source = "NHDFlowline")
) %>%
  relocate(source, .before = everything())



# # Merge the two dataframes
# NHD <- rbind(NHDArea_combined, NHDWaterbody_combined)
rm(NHDArea_combined, NHDWaterbody_combined, NHDFlowline_combined)
# Convert the coordinate system to 4269 (ideal for MN)
NHD <- NHD %>% sf::st_transform(4269)


# st_transform can make things kinda weird with geoms,
# st_make_valid can help fix that
NHD <- sf::st_make_valid(NHD)



# # Now let's compute the area for each type of waterway by county and CTU
# # first add an area column to the NHD dataframe, then convert to km2
# NHD$area <- sf::st_area(NHD)
# NHD$area_km2 <- NHD$area / 1e6

# browser()


# County level estimates --------------------------------------------------
message("Intersecting by county")

# create empty list
geom_list <- list()

cprg_county <- cprg_county %>%
  mutate(tmpID = paste0(geoid))
tmpNames <- unique(cprg_county$tmpID) # different munis
i <- 1
for (i in 1:length(tmpNames)) {
  message("running ", i, " of ", length(tmpNames))
  x <- cprg_county %>%
    dplyr::filter(tmpID == tmpNames[[i]]) %>%
    sf::st_make_valid()
  crop <- sf::st_crop(NHD, x) %>% sf::st_make_valid()

  # remove invalid polygons
  # this allows the process to continue. Sometimes topology is not perfectly
  # made and tinny pols can ruin your process
  notvalid <- which(s2::s2_is_valid_detail(crop) == FALSE)
  if (length(notvalid) > 0) {
    crop <- crop[-notvalid, ]
  }

  # intersection between x and crop
  x2 <- crop %>%
    sf::st_intersection(x) %>%
    sf::st_make_valid()

  # # "dissolve" to get
  # x3 <- x2 %>% group_by(NAME, id, name) %>%
  #   summarize()

  # return
  geom_list[[i]] <- x2
}

# browser()


NHD_byCounty <- bind_rows(geom_list, .id = "column_label") %>%
  dplyr::select(-column_label, tmpID) %>%
  mutate(FTYPE = case_when(
    # Area Classes
    FTYPE == 390 ~ "LakePond",
    FTYPE == 436 ~ "Reservoir",
    FTYPE == 466 ~ "SwampMarsh",
    FTYPE == 460 ~ "StreamRiver", # Body of flowing water.
    FTYPE == 398 ~ "Lock Chamber",
    FTYPE == 343 ~ "DamWeir",
    # Flowline Classes
    FTYPE == 334 ~ "Connector", # Known, but unspecific, connection between two nonadjacent network segments.
    FTYPE == 336 ~ "CanalDitch", # Artificial open waterway constructed to transport water, to irrigate or drain land, to connect two or more bodies of water, or to serve as a waterway for watercraft.
    FTYPE == 420 ~ "Underground Conduit", # Underground passage of surface water.
    FTYPE == 428 ~ "Pipeline", # Closed conduit with pumps, valves, and control devices, for conveying fluids, gases, or finely divided solids.
    FTYPE == 558 ~ "ArtificialPath" # Abstraction to facilitate hydrologic modeling through open water bodies and along coastal and Great Lakes shorelines and to act as a surrogate for lakes and other water bodies.
  )) %>%
  mutate(
    FTYPE = factor(FTYPE,
      levels = c(
        "LakePond", "Reservoir", "SwampMarsh", "StreamRiver",
        "Lock Chamber", "DamWeir", "Connector", "CanalDitch",
        "Underground Conduit", "Pipeline", "ArtificialPath"
      )
    )
  )


NHDFlowline_byCounty <- NHD_byCounty %>%
  filter(source == "NHDFlowline") %>%
  distinct(SHAPE, .keep_all = TRUE) %>%
  sf::st_make_valid(.)

NHDArea_byCounty <- NHD_byCounty %>%
  filter(source != "NHDFlowline") %>%
  distinct(SHAPE, .keep_all = TRUE) %>%
  sf::st_make_valid(.)

rm(NHD_byCounty)




message("Computing waterway geometry by county")
# Now let's compute the area for each type of waterway by county and CTU
# first add an area column to the NHD dataframe, then convert to km2
NHDFlowline_byCounty <- NHDFlowline_byCounty %>%
  mutate(
    distance = sf::st_length(.),
    distance_km = distance / 1e6
  )
NHDArea_byCounty <- NHDArea_byCounty %>%
  mutate(
    area = sf::st_area(.),
    area_km2 = area / 1e6
  )





# Compute area of each waterway type by county
nhd_county <- NHDArea_byCounty %>%
  sf::st_drop_geometry() %>% # drop geom features
  group_by(FTYPE, county_name) %>% # group by waterway type and county
  summarise(area = as.numeric(sum(area_km2)), .groups = "keep") %>% # compute total area
  ungroup() %>%
  as_tibble() %>%
  arrange(county_name, FTYPE) %>%
  left_join(cprg_county %>%
    sf::st_drop_geometry(), by = "county_name") %>%
  rename(waterway_type = FTYPE) %>%
  relocate(any_of(colnames(cprg_county)), waterway_type, area) %>%
  # use crossing() to create a simulated time series of area change between 2001 and 2021
  # in reality this just duplicates each row in the dataframe for every year in the sequence
  crossing(inventory_year = seq(2001, 2022, by = 1)) %>%
  dplyr::select(c("county_name", "state_name", "inventory_year", "waterway_type", "area")) %>%
  arrange(inventory_year, county_name, waterway_type)



# Export datasets for plotting (Quarto)
# Plotting
nhd_area_msa <- NHDArea_byCounty %>% dplyr::select(FTYPE, source, area_km2)


# Disable for now
# grab geoms based on linepath (nhd_all$Flowline)
nhd_flowlines_msa <- NHDFlowline_byCounty %>% dplyr::select(FTYPE, source, distance_km)


waterway_colors <- c(
  "LakePond" = "steelblue",
  "StreamRiver" = "lightskyblue",
  "Reservoir" = "maroon3",
  "SwampMarsh" = "palegreen3",
  "Lock Chamber" = "purple3",
  "DamWeir" = "darkslategray4",
  "Connector" = "red",
  "CanalDitch" = "goldenrod",
  "Underground Conduit" = "darkorchid1",
  "Pipeline" = "darkslategray",
  "ArtificialPath" = "green"
)

# browser()
#
# ggplot(cprg_county) +   geom_sf(color="gray20", fill=NA, lwd=0.5) + #geom_sf(color=NA, fill=NA, lwd=0) +
#   # ggspatial::annotation_scale(location = "br", width_hint = 0.3, pad_x = unit(0.23, "in"),unit_category="imperial") +
#   # ggspatial::annotation_north_arrow(location = "br", which_north = "true",
#   #                                   height= unit(0.33, "in"), width= unit(0.35, "in"),
#   #                                   pad_x = unit(0.1, "in"), pad_y = unit(0.35, "in"),
#   #                                   style = ggspatial::north_arrow_minimal) +
#   councilR::theme_council_geo() +
#
#
#   geom_sf(data = nhd_area_msa, aes(fill=FTYPE, color=FTYPE), lwd=0.1) +
#
#
#   scale_fill_manual("waterway type",breaks=names(waterway_colors), values = as.character(waterway_colors), guide = guide_legend(order = 1)) +
#   scale_color_manual("waterway type",breaks=names(waterway_colors), values = as.character(waterway_colors), guide = guide_legend(order = 1)) +
#
#   ggnewscale::new_scale_fill() + ## geoms added after this will use a new scale definition
#   ggnewscale::new_scale_color() + ## geoms added after this will use a new scale definition
#
#   geom_sf(data = nhd_flowlines_msa, aes(color=FTYPE), lwd=0.2) +
#   scale_color_manual("",breaks=names(waterway_colors), values = as.character(waterway_colors), guide = guide_legend(order = 2)) +
#
#   ggnewscale::new_scale_color() + ## geoms added after this will use a new scale definition
#
#
#   geom_sf(data = cprg_ctu, fill=NA, aes(color=councilR::colors$suppGray), alpha=0.8,  lwd=0.2) +
#   scale_color_manual("", values=councilR::colors$suppGray, labels = "CTU boundary") +
#
#   ggnewscale::new_scale_color() + ## geoms added after this will use a new scale definition
#
#   geom_sf(aes(color=councilR::colors$suppGray), fill=NA, lwd=0.6) + # add county lines
#   scale_color_manual("", values=councilR::colors$suppGray, labels = "County boundary") +
#   theme(
#     legend.position = "right", legend.direction = "vertical",  legend.justification = c("left","top"),
#     legend.margin = margin(2, 0, 0, 0), legend.key.height = unit(0.3, "cm"), legend.key.width = unit(0.3, "cm"),
#     legend.box = "vertical", legend.box.just = "left", legend.title = element_text(size=9), legend.text = element_text(size=8)
#   )





message("Wrapping up")




# browser()


# create metadata ---------------------------------------------------------
nhd_county_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "county_name", class(nhd_county$county_name), "County name",
    "state_name", class(nhd_county$state_name), "State name",
    "inventory_year", class(nhd_county$inventory_year), "Year",
    "waterway_type", class(nhd_county$waterway_type), "Waterway type from National Hydrogaphy Dataset",
    "area", class(nhd_county$area), "Area of land cover in square kilometers"
  )


nhd_area_msa_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "FTYPE", class(nhd_area_msa$FTYPE), "Waterway type",
    "source", class(nhd_area_msa$source), "NHD data source type",
    "area_km2", class(nhd_area_msa$area_km2), "Area of each waterway type in square kilometers",
    "SHAPE", class(nhd_area_msa$SHAPE), "Plotting geometry"
  )


nhd_flowlines_msa_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "FTYPE", class(nhd_flowlines_msa$FTYPE), "Waterway type",
    "source", class(nhd_flowlines_msa$source), "NHD data source type",
    "distance_km", class(nhd_flowlines_msa$distance_km), "Distance of each waterway type in kilometers",
    "SHAPE", class(nhd_flowlines_msa$SHAPE), "Plotting geometry"
  )


# User chooses whether to overwrite the rds files
if (overwrite_RDS) {
  message("Exporting RDS files...")

  saveRDS(nhd_county, paste0("./_nature/data/nhd_county_waterways_allyrs.rds"))
  saveRDS(nhd_county_meta, paste0("./_nature/data/nhd_county_waterways_allyrs_meta.rds"))

  saveRDS(nhd_area_msa, "./_nature/data/nhd_area_msa.rds")
  saveRDS(nhd_area_msa_meta, "./_nature/data/nhd_area_msa_meta.rds")

  saveRDS(nhd_flowlines_msa, "./_nature/data/nhd_flowlines_msa.rds")
  saveRDS(nhd_flowlines_msa_meta, "./_nature/data/nhd_flowlines_msa_meta.rds")
}
