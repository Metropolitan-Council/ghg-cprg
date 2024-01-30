# Counties -----
source("R/_load_pkgs.R")

# our study area includes the 7-county metro
# plus Sherburne and Chisago in MN
# and St. Croix and Pierce in WI

# fetch MN counties
mn_counties <- tigris::counties(state = "MN") %>%
  filter(NAME %in% c(
    "Anoka",
    "Carver",
    "Dakota",
    "Hennepin",
    "Ramsey",
    "Scott",
    "Sherburne",
    "Chisago",
    "Washington"
  ))

# fetch WI counties
wi_counties <- tigris::counties(state = "WI") %>%
  filter(NAME %in% c(
    "St. Croix",
    "Pierce"
  ))


# Combine to get cprg_counties
# Get state names from FIPS codes
cprg_county <- bind_rows(mn_counties, wi_counties) %>%
  left_join(
    tigris::fips_codes %>%
      select(state_code, state_name) %>%
      unique(),
    by = c("STATEFP" = "state_code")
  ) %>%
  select(STATE = state_name, STATEFP, COUNTYFP, GEOID, NAME, NAMELSAD)


cprg_county_meta <- tribble(
  ~Column, ~Class, ~Description,
  "STATE", class(cprg_county$STATE), "Full state name",
  "STATEFP", class(cprg_county$STATEFP), "State FIPS code",
  "COUNTYFP", class(cprg_county$COUNTYFP), "County FIPS code",
  "GEOID", class(cprg_county$GEOID), "County GEOID",
  "NAME", class(cprg_county$NAME), "County name",
  "NAMELSAD", class(cprg_county$NAMELSAD), "Full county name",
  "geometry", class(cprg_county$geometry)[1], "Simple feature geometry"
)

# Cities ------

# fetch cities from MN Geospatial Commons
mn_ctu <- councilR::import_from_gpkg("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_dot/bdry_mn_city_township_unorg/gpkg_bdry_mn_city_township_unorg.zip") %>%
  filter(COUNTY_NAM %in% c(cprg_county$NAME)) %>%
  mutate(
    STATEFP = "27",
    STATE = "Minnesota"
  ) %>%
  select(
    CTU_NAME = FEATURE_NA,
    CTU_CLASS,
    COUNTY_NAM,
    STATEFP,
    STATE,
    GNIS_FEATU,
    geometry = geom
  ) %>%
  arrange(CTU_NAME)

# downloaded from https://gis-ltsb.hub.arcgis.com/pages/download-data
wi_ctu <- sf::read_sf("R/WI_Cities%2C_Towns_and_Villages_(July_2023)/CTV_July_2023.shp") %>%
  filter(CNTY_NAME %in% c("Pierce", "St. Croix")) %>%
  select(CNTY_NAME, CNTY_FIPS, GEOID, MCD_NAME, MCD_FIPS, CTV) %>%
  mutate(
    CTU_CLASS = case_when(
      CTV == "T" ~ "TOWN",
      CTV == "C" ~ "CITY",
      CTV == "V" ~ "VILLAGE"
    ),
    COUNTY_NAM = CNTY_NAME,
    CTU_NAME = MCD_NAME,
    STATE = "Wisconsin",
    STATEFP = "55"
  ) %>%
  select(
    CTU_NAME,
    CTU_CLASS,
    COUNTY_NAM,
    STATEFP,
    STATE,
    GEOID
  )

cprg_ctu <- bind_rows(mn_ctu, wi_ctu)

cprg_ctu_meta <- tribble(
  ~Column, ~Class, ~Description,
  "CTU_NAME", class(cprg_ctu$CTU_NAME), "City, township, unorganized territory, or village name",
  "CTU_CLASS", class(cprg_ctu$CTU_CLASS), "City class (City, township, unorganized territory, or village)",
  "COUNTY_NAM", class(cprg_ctu$COUNTY_NAM), "County name",
  "STATEFP", class(cprg_ctu$STATEFP), "State FIPS code",
  "STATE", class(cprg_ctu$STATE), "Full state name",
  "GNIS_FEATU", class(cprg_ctu$GNIS_FEATU), "Minnesota geographic identifier",
  "geometry", class(cprg_ctu$geometry)[1], "Simple feature geometry",
  "GEOID", class(cprg_ctu$GEOID), "Wisconsin geographic identifier"
)

# create coherent geogs list
geogs_list_ctu <- cprg_ctu %>% 
  mutate(GEOG_LEVEL_ID = 'CTU',
         GEOG_UNIT_ID = if_else(is.na(GNIS_FEATU), GEOID, as.character(GNIS_FEATU)),  # pad with zeros on left til 8 chars
         GEOG_UNIT_NAME = CTU_NAME, GEOG_UNIT_DESC = CTU_NAME) %>% 
  sf::st_drop_geometry()
  
geogs_list_co <- cprg_county %>% mutate(GEOG_LEVEL_ID = 'CO', 
                                        GEOG_UNIT_ID = as.character(COUNTYFP), GEOG_UNIT_NAME = NAME,
                                        GEOG_UNIT_DESC = NAMELSAD) %>%
  sf::st_drop_geometry()

ctu_co_crosswalk <- left_join(geogs_list_ctu, geogs_list_co, 
                              by = c('COUNTY_NAM' = 'NAME', 'STATEFP' = 'STATEFP'),
                              suffix = c('.CHILD', '.PARENT')) %>%
  select(GEOG_LEVEL_ID.PARENT, GEOG_UNIT_ID.PARENT,
         GEOG_LEVEL_ID.CHILD, GEOG_UNIT_ID.CHILD,
         STATEFP) 

geogs_list <- bind_rows(select(geogs_list_ctu, GEOG_UNIT_ID, GEOG_LEVEL_ID, GEOG_UNIT_NAME, GEOG_UNIT_DESC, STATEFP),
                        select(geogs_list_co, GEOG_UNIT_ID, GEOG_LEVEL_ID, GEOG_UNIT_NAME, GEOG_UNIT_DESC, STATEFP)) 
# different CRS for geometries between CTU/CO; removed geometries above

# compile RDS
saveRDS(cprg_county, "_meta/data/cprg_county.RDS")
saveRDS(cprg_county_meta, "_meta/data/cprg_county_meta.RDS")

saveRDS(cprg_ctu, "_meta/data/cprg_ctu.RDS")
saveRDS(cprg_ctu_meta, "_meta/data/cprg_ctu_meta.RDS")
