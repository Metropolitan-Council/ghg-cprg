# Counties -----
source("R/_load_pkgs.R")

ctu_population <- readRDS("_meta/data/ctu_population.RDS")
ctu_population_meta <- read_rds("_meta/data/ctu_population_meta.RDS")

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
  )) %>%
  mutate(STATE_ABB = "MN") %>%
  clean_names()

# fetch WI counties
wi_counties <- tigris::counties(state = "WI") %>%
  filter(NAME %in% c(
    "St. Croix",
    "Pierce"
  )) %>%
  mutate(STATE_ABB = "WI") %>%
  clean_names()


# Combine to get cprg_counties
# Get state names from FIPS codes
cprg_county <- bind_rows(mn_counties, wi_counties) %>%
  left_join(
    tigris::fips_codes %>%
      select(state_code, state_name) %>%
      unique(),
    by = c("statefp" = "state_code")
  ) %>%
  mutate(cprg_area = TRUE) %>%
  select(
    geoid,
    county_name = name,
    county_name_full = namelsad,
    state_name, statefp,
    state_abb = STATE_ABB,
    cprg_area,
    geometry
  )



cprg_county_meta <- 
  ctu_population_meta %>% 
  filter(Column %in% names(cprg_county)) %>% 
  bind_rows(
    tribble(
      ~Column, ~Class, ~Description,
      "county_name", class(cprg_county$county_name), "County name",
      "county_name_full", class(cprg_county$county_name_full), "Full county name",
      "state_name", class(cprg_county$state_name), "Full state name",
      "statefp", class(cprg_county$statefp), "State FIPS code",
      "state_abb", class(cprg_county$state_abb), "Abbreviated state name",
      "cprg_area", class(cprg_county$cprg_area), "Whether county is included in the CPRG area",
      "geometry", class(cprg_county$geometry)[1], "Simple feature geometry"
    ))

# Cities ------

# fetch cities from MN Geospatial Commons
mn_ctu <- councilR::import_from_gpkg("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_dot/bdry_mn_city_township_unorg/gpkg_bdry_mn_city_township_unorg.zip") %>%
  filter(COUNTY_NAME %in% c(cprg_county$county_name)) %>%
  mutate(
    STATEFP = "27",
    STATE = "Minnesota",
    STATE_ABB = "MN",
    GNIS_FEATURE_ID = stringr::str_pad(as.character(GNIS_FEATURE_ID), 
                                       width = 8, side = "left",
                                       pad = "0")
  ) %>%
  select(
    CTU_NAME = FEATURE_NAME,
    CTU_CLASS,
    COUNTY_NAME,
    STATEFP,
    STATE,
    STATE_ABB,
    GNIS_FEATURE_ID,
    geometry = SHAPE
  ) %>%
  arrange(CTU_NAME) %>%
  clean_names() %>% 
  left_join(cprg_county %>% sf::st_drop_geometry(),
            by = join_by(county_name, statefp, state_abb)) %>% 
  mutate(coctu_id_gnis = paste0(
    stringr::str_sub(geoid, -3, -1), 
    gnis_feature_id
  )) %>% 
  left_join(ctu_population %>% 
              select(geoid, coctu_id_fips, coctu_id_gnis, ctuid) %>% 
              unique(),
            join_by(geoid, coctu_id_gnis))


if (file.exists("_meta/data-raw/WI_Cities%2C_Towns_and_Villages_(July_2023)/CTV_July_2023.shp") == FALSE) {
  cli::cli_abort(c(
    "Required datasets unavailable",
    "*" = "Download CTU data from the Wisconsin Legislative Technology site",
    "*" = "{.url https://gis-ltsb.hub.arcgis.com/pages/download-data}"
  ))
}

# downloaded from https://gis-ltsb.hub.arcgis.com/pages/download-data
wi_ctu <- sf::read_sf("_meta/data-raw/WI_Cities%2C_Towns_and_Villages_(July_2023)/CTV_July_2023.shp") %>%
  filter(CNTY_NAME %in% c("Pierce", "St. Croix")) %>%
  select(CNTY_NAME, COUSUBFP, CNTY_FIPS, GEOID, MCD_NAME, MCD_FIPS, CTV) %>%
  mutate(
    CTU_CLASS = case_when(
      CTV == "T" ~ "TOWN",
      CTV == "C" ~ "CITY",
      CTV == "V" ~ "VILLAGE"
    ),
    COUNTY_NAME = CNTY_NAME,
    COUNTY_NAM = CNTY_NAME,
    CTU_NAME = MCD_NAME,
    STATE = "Wisconsin",
    STATEFP = "55",
    STATE_ABB = "WI",
    geoid = stringr::str_sub(GEOID, 1, 5)
  ) %>%
  select(
    CTU_NAME,
    CTU_CLASS,
    COUNTY_NAM,
    COUNTY_NAME,
    STATEFP,
    STATE,
    STATE_ABB,
    geoid,
    ctu_id_fips = COUSUBFP
  ) %>%
  clean_names()

### fetch thrive community designations

# thrive <- councilR::import_from_gpkg("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/society_thrive_msp2040_com_des/gpkg_society_thrive_msp2040_com_des.zip") %>%
#   st_drop_geometry() %>%
#   clean_names() %>%
#   distinct(ctu_id, comdesname) %>%
#   rename(thrive_designation = comdesname)

thrive <- councilR::import_from_gpkg("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/society_thrive_msp2040_com_des/gpkg_society_thrive_msp2040_com_des.zip") %>%
  st_drop_geometry() %>% 
  separate(COCTU_DESC, sep = " [(]", into = c("ctu", "cty"), fill = "right") %>%
  mutate(
    GNIS_FEATURE_ID = stringr::str_pad(as.character(CTU_ID), 
                                       width = 8, side = "left",
                                       pad = "0"),
    COMDESNAME = factor(COMDESNAME,
                        levels = c(
                          "Urban Center",
                          "Urban",
                          "Suburban",
                          "Suburban Edge",
                          "Emerging Suburban Edge",
                          "Rural Center",
                          "Diversified Rural",
                          "Rural Residential",
                          "Agricultural",
                          "Non-Council Area",
                          "Non-Council Community"
                        ),
                        ordered = T
    ),
    URB_RURAL = stringr::str_sub(URB_RURAL, 1, 5),
    URB_SUB_RURAL = case_when(
      COMDESNAME %in% c(
        "Suburban",
        "Suburban Edge",
        "Emerging Suburban Edge"
      ) ~ "Suburban",
      TRUE ~ URB_RURAL
    ) %>%
      factor(levels = c(
        "Urban",
        "Suburban",
        "Rural"
      ), ordered = T)
  ) %>%
  group_by(CTU_ID, COMDESNAME, GNIS_FEATURE_ID) %>%
  count() %>%
  group_by(CTU_ID, GNIS_FEATURE_ID) %>%
  filter(as.integer(COMDESNAME) == max(as.integer(COMDESNAME))) %>%
  ungroup() %>%
  select(
    ctu_id = CTU_ID,
    GNIS_FEATURE_ID,
    thrive_designation = COMDESNAME
  )


imagine <- import_from_gpkg("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/plan_imagine2050_comdes/gpkg_plan_imagine2050_comdes.zip") %>% 
  st_drop_geometry() %>% 
  mutate(
    COCTU_ID_GNIS = COCTU_ID,
    GNIS_FEATURE_ID = stringr::str_sub(COCTU_ID_GNIS, -8, -1),
    COMDESNAME = factor(COMDESNAME,
                        levels =  c("Urban", 
                                    "Urban Edge", 
                                    "Suburban", 
                                    "Suburban Edge", 
                                    "Rural Center",
                                    "Diversified Rural",
                                    "Rural Residential",
                                    "Agricultural", 
                                    "Non-Council Community"),
                        ordered = T
    ),
    URB_SUB_RURAL = case_when(
      COMDESNAME %in% c("Urban Center",
                        "Urban Edge",
                        "Urban") ~ "Urban",
      COMDESNAME %in% c(
        "Suburban",
        "Suburban Edge",
        "Emerging Suburban Edge"
      ) ~ "Suburban",
      COMDESNAME %in% c( "Diversified Rural",
                         "Rural Residential",
                         "Rural Center",
                         "Agricultural"
      ) ~ "Rural"
    ) %>%
      factor(levels = c(
        "Urban",
        "Suburban",
        "Rural"
      ), ordered = T)
  ) %>%
  group_by(COMDESNAME, GNIS_FEATURE_ID) %>%
  count() %>% 
  group_by(GNIS_FEATURE_ID) %>%
  filter(as.integer(COMDESNAME) == max(as.integer(COMDESNAME))) %>%
  ungroup() %>%
  select(
    GNIS_FEATURE_ID,
    imagine_designation = COMDESNAME
  )

cprg_ctu <- bind_rows(mn_ctu, wi_ctu) %>%
  mutate(cprg_area = TRUE) %>%
  select(ctu_name, ctu_class,
         county_name,
         state_name = state, statefp, 
         state_abb,
         geoid_wis = ctu_id_fips,
         gnis = gnis_feature_id,
         cprg_area,
         geometry
  ) %>%
  left_join(thrive,
            by = c("gnis" = "GNIS_FEATURE_ID")
  ) %>%
  left_join(imagine,
            by = c("gnis" = "GNIS_FEATURE_ID")
  ) %>% 
  mutate(thrive_designation = if_else(
    is.na(thrive_designation),
    "Non-Council Area",
    thrive_designation
  ),
  imagine_designation = if_else(
    is.na(imagine_designation),
    "Non-Council Area",
    imagine_designation
  ))


cprg_ctu_meta <- 
  ctu_population_meta %>% 
  filter(Column %in% names(cprg_ctu)) %>% 
  bind_rows(
    tribble(
      ~Column, ~Class, ~Description,
      "geoid_wis", class(cprg_ctu$geoid_wis), "Wisconsin geographic identifier",
      "geometry", class(cprg_ctu$geometry)[1], "Simple feature geometry",
      "thrive_designation", class(cprg_ctu$thrive_designation), "Community designation in Thrive 2040",
      "imagine_designation", class(cprg_ctu$thrive_designation), "Community designation in Imagine 2050",
    ) )%>%
  bind_rows(cprg_county_meta) %>%
  filter(Column %in% names(cprg_ctu)) %>% 
  unique()

# create coherent geogs list
geogs_list_ctu <- cprg_ctu %>%
  mutate(
    geog_level_id = "CTU",
    geog_unit_id = if_else(is.na(gnis), geoid_wis, as.character(gnis)), # pad with zeros on left til 8 chars
    geog_unit_name = ctu_name,
    geog_unit_desc = ctu_name
  ) %>%
  sf::st_drop_geometry()

geogs_list_co <- cprg_county %>%
  mutate(
    geog_level_id = "CO",
    geog_unit_id = as.character(geoid),
    geog_unit_name = county_name,
    geog_unit_desc = county_name_full
  ) %>%
  sf::st_drop_geometry()

ctu_co_crosswalk <- left_join(geogs_list_ctu, geogs_list_co,
                              by = c(
                                "county_name",
                                "statefp"
                              ),
                              suffix = c(".child", ".parent")
) %>%
  select(
    geog_level_id.parent,
    geog_unit_id.parent,
    geog_level_id.child,
    geog_unit_id.child,
    statefp
  )

geogs_list <- bind_rows(
  select(
    geogs_list_ctu, geog_unit_id,
    geog_level_id,
    geog_unit_name,
    geog_unit_desc,
    statefp
  ),
  select(
    geogs_list_co, geog_unit_id,
    geog_level_id,
    geog_unit_name,
    geog_unit_desc,
    statefp
  )
)
# there are different CRS for geometries between CTU/CO; removed geometries above, geogs_list is a simple table


# waldo::compare(cprg_ctu, readRDS("_meta/data/cprg_ctu.RDS"))


# compile RDS
saveRDS(cprg_county, "_meta/data/cprg_county.RDS")
saveRDS(cprg_county_meta, "_meta/data/cprg_county_meta.RDS")

saveRDS(cprg_ctu, "_meta/data/cprg_ctu.RDS")
saveRDS(cprg_ctu_meta, "_meta/data/cprg_ctu_meta.RDS")

saveRDS(ctu_co_crosswalk, "_meta/data/geog_crosswalk.RDS")
saveRDS(geogs_list, "_meta/data/geogs_list.RDS")

