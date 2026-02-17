rm(list = ls())
source("R/_load_pkgs.R")
source("R/cprg_colors.R")

overwrite_RDS <- TRUE


message("Loading geoms")

# load the county and ctu boundaries layer
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_ctu <- readRDS("_meta/data/cprg_ctu.RDS")


# Define the input path
inpath <- paste0(here::here(), "/_nature/data-raw/restorable_wetlands_gdb")

# List all .gdb file paths
gdb_files <- list.dirs(inpath,
                       recursive = TRUE,
                       full.names = TRUE
)[grepl(
  "\\.gdb",
  list.dirs(inpath,
            recursive = TRUE,
            full.names = TRUE
  )
)]



# Define a function to read specific layers from multiple .gdb files
read_gdb_layers <- function(gdb_path, layer_name) {
  st_read(gdb_path, layer = layer_name) 
  }


# Use lapply to read the "RestorableWetlands_CCAP" layer from all .gdb files
RestorableWetlands_list <- lapply(gdb_files, read_gdb_layers, layer_name = "RestorableWetlands_CCAP")
# Combine all the data frames into one
RestorableWetlands_combined <- bind_rows(RestorableWetlands_list)

# Convert the coordinate system to 4269 (ideal for MN)
RestorableWetlands_combined <- RestorableWetlands_combined %>% sf::st_transform(4269)


# County level estimates --------------------------------------------------
message("Intersecting by county")

# create empty list
geom_list <- list()

cprg_county <- cprg_county %>%
  mutate(tmpID = paste0(geoid))
tmpNames <- unique(cprg_county$tmpID) # different munis
i <- 1


sf::sf_use_s2(FALSE)

for (i in seq_along(tmpNames)) {
  message("running ", i, " of ", length(tmpNames))
  x <- cprg_county %>%
    filter(tmpID == tmpNames[[i]]) %>%
    st_make_valid()
  
  crop <- st_crop(RestorableWetlands_combined, x) %>% st_make_valid()
  
  notvalid <- which(!sf::st_is_valid(crop))
  if (length(notvalid) > 0) crop <- crop[-notvalid, ]
  
  x2 <- suppressWarnings(st_intersection(crop, x))
  x2 <- st_make_valid(x2)
  
  geom_list[[i]] <- x2
}

sf::sf_use_s2(TRUE)


restorableWetlands_byCounty <- bind_rows(geom_list, .id = "column_label") 

restorableWetlands_byCounty <- restorableWetlands_byCounty %>%
  mutate(
    area = sf::st_area(.),
    area_km2 = area / 1e6
  )






ggplot(cprg_county) +   geom_sf(color="gray20", fill=NA, lwd=0.5) + #geom_sf(color=NA, fill=NA, lwd=0) +
    # ggspatial::annotation_scale(location = "br", width_hint = 0.3, pad_x = unit(0.23, "in"),unit_category="imperial") +
    # ggspatial::annotation_north_arrow(location = "br", which_north = "true",
    #                                   height= unit(0.33, "in"), width= unit(0.35, "in"),
    #                                   pad_x = unit(0.1, "in"), pad_y = unit(0.35, "in"),
    #                                   style = ggspatial::north_arrow_minimal) +
    councilR::theme_council_geo() +


    geom_sf(data = restorableWetlands_byCounty, aes(fill=gridcode), lwd=0) +


    scale_fill_manual("waterway type",breaks=names(waterway_colors), values = as.character(waterway_colors), guide = guide_legend(order = 1)) +
    scale_color_manual("waterway type",breaks=names(waterway_colors), values = as.character(waterway_colors), guide = guide_legend(order = 1)) +

    ggnewscale::new_scale_fill() + ## geoms added after this will use a new scale definition
    ggnewscale::new_scale_color() + ## geoms added after this will use a new scale definition

    geom_sf(data = nhd_flowlines_msa, aes(color=FTYPE), lwd=0.2) +
    scale_color_manual("",breaks=names(waterway_colors), values = as.character(waterway_colors), guide = guide_legend(order = 2)) +

    ggnewscale::new_scale_color() + ## geoms added after this will use a new scale definition


    geom_sf(data = cprg_ctu, fill=NA, aes(color=councilR::colors$suppGray), alpha=0.8,  lwd=0.2) +
    scale_color_manual("", values=councilR::colors$suppGray, labels = "CTU boundary") +

    ggnewscale::new_scale_color() + ## geoms added after this will use a new scale definition

    geom_sf(aes(color=councilR::colors$suppGray), fill=NA, lwd=0.6) + # add county lines
    scale_color_manual("", values=councilR::colors$suppGray, labels = "County boundary") +
    theme(
      legend.position = "right", legend.direction = "vertical",  legend.justification = c("left","top"),
      legend.margin = margin(2, 0, 0, 0), legend.key.height = unit(0.3, "cm"), legend.key.width = unit(0.3, "cm"),
      legend.box = "vertical", legend.box.just = "left", legend.title = element_text(size=9), legend.text = element_text(size=8)
    )





