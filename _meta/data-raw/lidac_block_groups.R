## Some header info
# Clear old vars
rm(list = ls())


## Start here:
source(file.path(here::here(), "R/_load_pkgs.R"))
source(file.path(here::here(), "R/_quarto_helpers.R"))
source(file.path(here::here(), "R/plot_county_emissions.R"))



export.plots <- F
outpath <- NULL

# read in list of LIDACs
LIDACs <- readr::read_csv("_meta/data-raw/IRA_LIDAC_block_group.csv",
  name_repair = "universal"
)
# data.table::setDT(LIDACs)



# read in EPA EJ Screening Tool data
EJSCREEN <- readr::read_csv("_meta/data-raw/EJSCREEN_2023_BG_StatePct_with_AS_CNMI_GU_VI.csv",
  name_repair = "universal"
)
data.table::setDT(EJSCREEN)

# subset to PCAP jurisdiction
EJSCREEN <- EJSCREEN[STATE_NAME == "Minnesota" & CNTY_NAME == "Anoka County" |
  STATE_NAME == "Minnesota" & CNTY_NAME == "Carver County" |
  STATE_NAME == "Minnesota" & CNTY_NAME == "Chisago County" |
  STATE_NAME == "Minnesota" & CNTY_NAME == "Dakota County" |
  STATE_NAME == "Minnesota" & CNTY_NAME == "Hennepin County" |
  STATE_NAME == "Minnesota" & CNTY_NAME == "Ramsey County" |
  STATE_NAME == "Minnesota" & CNTY_NAME == "Scott County" |
  STATE_NAME == "Minnesota" & CNTY_NAME == "Sherburne County" |
  STATE_NAME == "Minnesota" & CNTY_NAME == "Washington County" |
  STATE_NAME == "Wisconsin" & CNTY_NAME == "Pierce County" |
  STATE_NAME == "Wisconsin" & CNTY_NAME == "St. Croix County"]


# get block group geometry (keeps failing when I ask for multiple states, so two steps then concatenate)
MN_block_groups <- tigris::block_groups(state = "MN")
WI_block_groups <- tigris::block_groups(state = "WI")
block_groups <- rbind(MN_block_groups, WI_block_groups)

# rename ID in EJ Screen data to "GEOID", convert to character for join
colnames(EJSCREEN)[2] <- "GEOID"
EJSCREEN$GEOID <- as.character(EJSCREEN$GEOID)
LIDACs$GEOID <- as.character(LIDACs$GEOID)

# join block group data to EJ Screen data
map_data <- inner_join(block_groups, EJSCREEN, by = "GEOID")


# join LIDACs to map data
map_data <- left_join(map_data, LIDACs, by = "GEOID")


# replace NAs with No, make LIDAC indicator
map_data$GEOID_tract[is.na(map_data$GEOID_tract)] <- 0
map_data$LIDAC <- ifelse(map_data$GEOID_tract == "0", "No", "Yes")


# get county data for mapping, subset to PCAP region
# county_data <- tigris::counties(c("Minnesota", "Wisconsin"))
# county_data <- subset(county_data, NAME == "Anoka" |
#   NAME == "Carver" |
#   NAME == "Chisago" |
#   NAME == "Dakota" |
#   NAME == "Hennepin" |
#   NAME == "Ramsey" |
#   NAME == "Scott" |
#   NAME == "Sherburne" |
#   NAME == "Washington" & STATEFP == "27" |
#   NAME == "Pierce" |
#   NAME == "St. Croix")
county_geog <- readRDS("_meta/data/cprg_county.RDS")

LIDAC_polygons <- map_data


# saveRDS(LIDAC_polygons, "_meta/data/lidac_block_groups.RDS")
# saveRDS(EJSCREEN, "_meta/data/ejscreen_block_groups.RDS")




# if you wanted to dissolve LIDAC geometries:
# LIDAC_polygons %>%
# dplyr::group_by(LIDAC) %>%
# summarize(do_union = TRUE)

# make_maps <- F
# 
# if (make_maps) {
#   library(tmap)
# 
# 
#   ggplot(block_groups, aes(fill = NAMELSAD)) +
#     geom_sf() +
#     theme_void()
# 
#   # make LIDAC map
#   LIDAC_map <- tmap::tmap_mode("view") +
#     tmap::tm_shape(map_data) +
#     tmap::tm_polygons(
#       col = "LIDAC",
#       border.alpha = .08,
#       palette = c("#FFFFFF", "#005DAA"),
#       perceptual = TRUE
#     ) +
#     tmap::tm_layout(
#       frame = FALSE,
#       legend.outside = TRUE,
#       legend.outside.position = "right",
#       legend.frame = "#F2F2F2",
#       legend.stack = "horizontal"
#     ) +
#     tmap::tm_shape(county_data) +
#     tmap::tm_polygons(alpha = 0)
# 
#   if (export.plots) tmap::tmap_save(tm = LIDAC_map, filename = paste0(outpath, "LIDAC_map.png"), width = 18, height = 15, units = "cm", dpi = 300)
# }



# read in CEJST communities data
CEJST <- readr::read_csv("_meta/data-raw/1.0-communities.csv",
  name_repair = "universal"
)
data.table::setDT(CEJST)


# subset to PCAP region
CEJST <- CEJST[State.Territory == "Minnesota" & County.Name == "Anoka County" |
  State.Territory == "Minnesota" & County.Name == "Carver County" |
  State.Territory == "Minnesota" & County.Name == "Chisago County" |
  State.Territory == "Minnesota" & County.Name == "Dakota County" |
  State.Territory == "Minnesota" & County.Name == "Hennepin County" |
  State.Territory == "Minnesota" & County.Name == "Ramsey County" |
  State.Territory == "Minnesota" & County.Name == "Scott County" |
  State.Territory == "Minnesota" & County.Name == "Sherburne County" |
  State.Territory == "Minnesota" & County.Name == "Washington County" |
  State.Territory == "Wisconsin" & County.Name == "Pierce County" |
  State.Territory == "Wisconsin" & County.Name == "St. Croix County"]

# get tract geometry (CEJST data use 2010 census geographies)
MN_tracts <- tigris::tracts(state = "MN", year = "2010")
WI_tracts <- tigris::tracts(state = "WI", year = "2010")
tracts <- rbind(MN_tracts, WI_tracts)

colnames(CEJST)[1] <- "GEOID"
CEJST$GEOID <- as.character(CEJST$GEOID)
colnames(tracts)[4] <- "GEOID"


CEJST_map_data <- inner_join(tracts, CEJST, by = "GEOID")




## Instead of recreating the county data, read in cprg county data

CEJST_map_data
county_data
LIDAC_polygons




# if (make_maps) {
#   # make map of PM2.5 with LIDAC overlay
#   PM2.5_map <- tmap::tmap_mode("plot") +
#     tmap::tm_shape(CEJST_map_data) +
#     tmap::tm_polygons(
#       col = "PM2.5.in.the.air..percentile.",
#       border.alpha = .3,
#       style = "jenks",
#       palette = "Blues"
#     ) +
#     tmap::tm_shape(LIDAC_polygons %>% filter(LIDAC == "Yes")) +
#     # tmap::tm_polygons(col = "black",
#     #                   border.alpha = .8) +
#     tmap::tm_fill(
#       col = "#FFFF00",
#       alpha = 0.6
#     ) +
#     tmap::tm_layout(
#       frame = FALSE,
#       legend.outside = TRUE,
#       legend.outside.position = "right",
#       legend.stack = "horizontal"
#     ) +
#     tmap::tm_shape(county_data) +
#     tmap::tm_polygons(alpha = 0)
# 
#   if (export.plots) tmap::tmap_save(tm = PM2.5_map, filename = paste0(outpath, "PM2.5_map.png"), width = 20, height = 10, units = "cm", dpi = 300)
# 
#   # make map of diesel particulate exposure with LIDAC overlay
# 
#   diesel_map <- tmap::tmap_mode("plot") +
#     tmap::tm_shape(CEJST_map_data) +
#     tmap::tm_polygons(
#       col = "Diesel.particulate.matter.exposure..percentile.",
#       border.alpha = .3,
#       style = "jenks",
#       palette = "Blues"
#     ) +
#     tmap::tm_shape(LIDAC_polygons %>% filter(LIDAC == "Yes")) +
#     # tmap::tm_polygons(col = "black",
#     #                   border.alpha = .8) +
#     tmap::tm_fill(
#       col = "#FFFF00",
#       alpha = 0.6
#     ) +
#     tmap::tm_layout(
#       frame = FALSE,
#       legend.outside = TRUE,
#       legend.outside.position = "right",
#       legend.stack = "horizontal"
#     ) +
#     tmap::tm_shape(county_data) +
#     tmap::tm_polygons(alpha = 0)
# 
#   if (export.plots) tmap::tmap_save(tm = diesel_map, filename = paste0(outpath, "diesel_map.png"), width = 20, height = 10, units = "cm", dpi = 300)
# 
#   # make map of asthma prevalence with LIDAC overlay
# 
#   asthma_map <- tmap_mode("plot") +
#     tmap::tm_shape(CEJST_map_data) +
#     tmap::tm_polygons(
#       col = "Current.asthma.among.adults.aged.greater.than.or.equal.to.18.years..percentile.",
#       border.alpha = .3,
#       style = "jenks",
#       palette = "Blues"
#     ) +
#     tmap::tm_shape(LIDAC_polygons %>% filter(LIDAC == "Yes")) +
#     # tmap::tm_polygons(col = "black",
#     #                   border.alpha = .8) +
#     tmap::tm_fill(
#       col = "#FFFF00",
#       alpha = 0.6
#     ) +
#     tmap::tm_layout(
#       frame = FALSE,
#       legend.outside = TRUE,
#       legend.outside.position = "right",
#       legend.stack = "horizontal"
#     ) +
#     tmap::tm_shape(county_data) +
#     tmap::tm_polygons(alpha = 0)
# 
#   if (export.plots) tmap::tmap_save(tm = asthma_map, filename = paste0(outpath, "asthma_map.png"), width = 20, height = 10, units = "cm", dpi = 300)
# 
# 
#   # make map of impervious surface percentile with LIDAC overlay
# 
#   impervious_map <- tmap_mode("plot") +
#     tmap::tm_shape(CEJST_map_data) +
#     tmap::tm_polygons(
#       col = "Share.of.the.tract.s.land.area.that.is.covered.by.impervious.surface.or.cropland.as.a.percent..percentile.",
#       border.alpha = .3,
#       style = "jenks",
#       palette = "Blues"
#     ) +
#     tmap::tm_shape(LIDAC_polygons %>% filter(LIDAC == "Yes")) +
#     # tmap::tm_polygons(col = "black",
#     #                   border.alpha = .8) +
#     tmap::tm_fill(
#       col = "#FFFF00",
#       alpha = 0.6
#     ) +
#     tmap::tm_layout(
#       frame = FALSE,
#       legend.outside = TRUE,
#       legend.outside.position = "right",
#       legend.stack = "horizontal"
#     ) +
#     tmap::tm_shape(county_data) +
#     tmap::tm_polygons(alpha = 0)
# 
#   if (export.plots) tmap::tmap_save(tm = impervious_map, filename = paste0(outpath, "impervious_map.png"), width = 20, height = 10, units = "cm", dpi = 300)
# 
#   # make map of housing cost burden percentile with LIDAC overlay
# 
#   housing_burden_map <- tmap_mode("plot") +
#     tmap::tm_shape(CEJST_map_data) +
#     tmap::tm_polygons(
#       col = "Housing.burden..percent...percentile.",
#       border.alpha = .3,
#       style = "jenks",
#       palette = "Blues"
#     ) +
#     tmap::tm_shape(LIDAC_polygons %>% filter(LIDAC == "Yes")) +
#     # tmap::tm_polygons(col = "black",
#     #                   border.alpha = .8) +
#     tmap::tm_fill(
#       col = "#FFFF00",
#       alpha = 0.6
#     ) +
#     tmap::tm_layout(
#       frame = FALSE,
#       legend.outside = TRUE,
#       legend.outside.position = "right",
#       legend.stack = "horizontal"
#     ) +
#     tmap::tm_shape(county_data) +
#     tmap::tm_polygons(alpha = 0)
# 
#   if (export.plots) tmap::tmap_save(tm = housing_burden_map, filename = paste0(outpath, "housing_burden_map.png"), width = 20, height = 10, units = "cm", dpi = 300)
# 
#   # make map of energy cost burden percentile with LIDAC overlay
# 
#   energy_burden_map <- tmap_mode("plot") +
#     tmap::tm_shape(CEJST_map_data) +
#     tmap::tm_polygons(
#       col = "Energy.burden..percentile.",
#       border.alpha = .3,
#       style = "jenks",
#       palette = "Blues"
#     ) +
#     tmap::tm_shape(LIDAC_polygons %>% filter(LIDAC == "Yes")) +
#     # tmap::tm_polygons(col = "black",
#     #                   border.alpha = .8) +
#     tmap::tm_fill(
#       col = "#FFFF00",
#       alpha = 0.6
#     ) +
#     tmap::tm_layout(
#       frame = FALSE,
#       legend.outside = TRUE,
#       legend.outside.position = "right",
#       legend.stack = "horizontal"
#     ) +
#     tmap::tm_shape(county_data) +
#     tmap::tm_polygons(alpha = 0)
# 
#   if (export.plots) tmap::tmap_save(tm = energy_burden_map, filename = paste0(outpath, "energy_burden_map.png"), width = 20, height = 10, units = "cm", dpi = 300)
# }

browser()

## Some more changes...
