source("R/_load_pkgs.R")

wi_counties <- read_rds("_meta/data/cprg_county.RDS") %>%
  filter(statefp == 55)


# downloaded from https://psc.wi.gov/Pages/ForConsumers/Maps.aspx. CRS is NAD83(HARN) / Wisconsin Transverse Mercator.
# input data from WI had issues with dimensionality/geometry on some features; I filtered this data to just the 2 municipal
# utilities -- New Richmond and River Falls -- that fall within our study area in ArcGIS PRO to address this issue)
wi_muni <- st_read(here(
  "_energy",
  "data-raw",
  "EL_PSCW_ServTerr (1)",
  "EL_PSCW_ServTerr_Muni_repaired.shp"
)) %>%
  select(LABEL, PSC_ID, Util_Type, geometry) %>%
  rename(utility_name = LABEL, utility_type = Util_Type)

wi_iou <- st_read(here(
  "_energy",
  "data-raw",
  "EL_PSCW_ServTerr (1)",
  "EL_PSCW_ServTerr_IOU.shp"
)) %>%
  select(LABEL, PSC_ID, UTIL_TYPE, geometry) %>%
  rename(utility_name = LABEL, utility_type = UTIL_TYPE)


wi_coop <- st_read(here(
  "_energy",
  "data-raw",
  "EL_PSCW_ServTerr (1)",
  "EL_PSCW_ServTerr_Coop.shp"
)) %>%
  select(LABEL, PSC_ID, Util_Type, geometry) %>%
  rename(utility_name = LABEL, utility_type = Util_Type)

# ONLY FOR DOCUMENTATION/METADATA USE
wi_muni_all <- st_read(here(
  "_energy",
  "data-raw",
  "EL_PSCW_ServTerr (1)",
  "EL_PSCW_ServTerr_Muni.shp"
)) %>%
  st_make_valid() %>%
  select(LABEL, PSC_ID, Util_Type, geometry) %>%
  rename(utility_name = LABEL, utility_type = Util_Type)

# harmonize county data CRS to WI state-provided data
wi_counties <- st_transform(wi_counties, st_crs(wi_iou))


# function to process the three different shapefiles (for the three different utility types) into one combined WI electric utility service area shapefile
identify_WIutilities_in_scope <- function(wi_counties, utility_files) {
  utilities_in_scope <- list()

  for (utility_file in utility_files) {
    utility_type <- basename(utility_file) # Get the filename
    utility_type <- sub("^EL_PSCW_ServTerr_", "", utility_type) # Remove prefix
    utility_type <- sub("\\.shp$", "", utility_type) # Remove file extension

    utility_data <- st_read(utility_file) # Read the shapefile
    clipped_utility <- st_intersection(utility_data, wi_counties) # Clip utility to counties

    # Store in the list with utility type
    utilities_in_scope[[utility_type]] <- clipped_utility
  }

  return(utilities_in_scope)
}

# vector that points to location of three WI utility shapefiles
wi_utility_files <- c(
  here(
    "_energy",
    "data-raw",
    "EL_PSCW_ServTerr (1)",
    "EL_PSCW_ServTerr_Muni_repaired.shp"
  ),
  here(
    "_energy",
    "data-raw",
    "EL_PSCW_ServTerr (1)",
    "EL_PSCW_ServTerr_IOU.shp"
  ),
  here(
    "_energy",
    "data-raw",
    "EL_PSCW_ServTerr (1)",
    "EL_PSCW_ServTerr_Coop.shp"
  )
)

# run function to create lists of in-scope utilities (utilities that operate within our study area)
results <- identify_WIutilities_in_scope(wi_counties, wi_utility_files)

# write outputs of function to separate environmental variables for inspection,
#  trim to essential columns, and rename for consistency
wi_muni_utilitiesInScope <- results$Muni_repaired %>%
  select(
    LABEL, Util_Type, PSC_ID, ADDRESS_1, ADDRESS_2,
    CITY, STATE, ZIP, geoid, county_name
  ) %>%
  rename(
    utility_name = LABEL, utility_type = Util_Type
  )

wi_iou_utilitiesInScope <- results$IOU %>%
  select(
    LABEL, UTIL_TYPE, PSC_ID, ADDRESS_1, ADDRESS_2,
    CITY, STATE, ZIP, geoid, county_name
  ) %>%
  rename(
    utility_name = LABEL, utility_type = UTIL_TYPE
  )


wi_coop_utilitiesInScope <- results$Coop %>%
  select(
    LABEL, Util_Type, PSC_ID, ADDRESS_1, ADDRESS_2,
    CITY, STATE, ZIP, geoid, county_name
  ) %>%
  rename(
    utility_name = LABEL, utility_type = Util_Type
  )


WIutilities_in_scope <- rbind(
  wi_muni_utilitiesInScope,
  wi_iou_utilitiesInScope,
  wi_coop_utilitiesInScope
)


st_write(WIutilities_in_scope,
  here("_energy", "data-raw", "EL_PSCW_ServTerr (1)", "WIutilities_in_scope.shp"),
  append = FALSE
)

# read in shapefile with the full service area of all in-scope utilities to facilitate allocation of statewide activity data (manually processed in ArcGIS Pro, using the same shapefiles as wi_iou, wi_coop, and wi_muni). This is simply a filtered version of WIutilities_in_scope
fullServiceArea_inScopeUtilities_WI <- st_read(here(
  "_energy",
  "data-raw",
  "EL_PSCW_ServTerr (1)",
  "fullServiceArea_inScopeUtilities_WI.shp"
))

distinct_util_type_WI <- WIutilities_in_scope %>%
  distinct(utility_name, utility_type)

wi_allElecUtilTypes <- rbind(wi_muni_all, wi_iou, wi_coop)

inScope_WI_elecUtils_fullServTerr <- wi_allElecUtilTypes %>%
  filter(utility_name %in% distinct_util_type_WI$utility_name)

write_rds(wi_allElecUtilTypes, here(
  "_energy",
  "data",
  "WI_elecUtils_allTypes.RDS"
))

write_rds(inScope_WI_elecUtils_fullServTerr, here(
  "_energy",
  "data",
  "WI_inScope_elecUtils_fullServTerr.RDS"
))

write_rds(WIutilities_in_scope, here(
  "_energy",
  "data",
  "WI_electricity_inScope_utilityCountyPairs.RDS"
))

write_rds(distinct_util_type_WI, here(
  "_energy",
  "data",
  "distinct_electricity_util_type_WI.RDS"
))
