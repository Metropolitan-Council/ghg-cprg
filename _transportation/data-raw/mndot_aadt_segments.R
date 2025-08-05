# download and unzip
cprg_county <- readRDS("_meta/data/cprg_county.RDS")

ctu_population <- readRDS("_meta/data/ctu_population.RDS") %>%
  left_join(cprg_county %>% sf::st_drop_geometry()) %>%
  mutate(ctu_name_full = paste0(ctu_name, ", ", ctu_class),
         ctu_name_full_county = paste0(ctu_name_full, ", ", county_name ))


cprg_ctu <- readRDS("_meta/data/cprg_ctu.RDS") %>%
  left_join(cprg_county %>% sf::st_drop_geometry()) %>% 
  mutate(ctu_name_full = paste0(ctu_name, ", ", ctu_class)) %>% 
  left_join(ctu_population %>% 
              select(geoid, gnis, coctu_id_fips, coctu_id_gnis) %>% 
              unique(),
            by = c("gnis", "geoid"))

download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_dot/trans_aadt_traffic_segments/gpkg_trans_aadt_traffic_segments.zip",
              destfile = "_transportation/data-raw/mndot/gpkg_trans_aadt_traffic_segments.zip")

unzip("_transportation/data-raw/mndot/gpkg_trans_aadt_traffic_segments.zip",
      exdir = "_transportation/data-raw/mndot/")

aadt_all <- sf::read_sf("_transportation/data-raw/mndot/trans_annual average daily traffic segments.gpkg") %>% 
  st_zm() %>%
  st_transform(4326) %>%
  # st_intersection(metro_counties_buffer) %>%
  # dplyr::select(-names(metro_counties)[1:17]) %>%
  mutate(
    SEQUENCE_N = as.character(SEQUENCE_NUMBER),
    urban_rural_segment = str_extract(AXLE_FACTOR_GROUP, "\\b(Urban|Rural)\\b"),
    SEGMENT_LENGTH = st_length(SHAPE) %>% 
      # units::as_units("meter") %>% 
      units::set_units("mile") %>% 
      as.numeric()) %>% 
  # perform basic join with counties to remove outstate segments
  sf::st_intersection(cprg_county %>% 
                        st_transform(4326) %>%
                        st_union())



ctu_segments <- aadt_all %>% 
  st_intersection(cprg_ctu %>% 
                    filter(state_abb == "MN",
                           !is.na(ctu_id))) %>% 
  mutate(SEGMENT_LENGTH_CTU = st_length(SHAPE) %>% 
           # units::as_units("meter") %>% 
           units::set_units("mile") %>% 
           as.numeric()) %>% 
  mutate(from_year = year(FROM_DATE)) %>% 
  select(gnis, ctu_name, county_name, coctu_id_fips, coctu_id_gnis, from_year,
         SEQUENCE_N, SEQUENCE_NUMBER, ROUTE_LABEL, AXLE_FACTOR_GROUP, CURRENT_YEAR,
         CURRENT_VOLUME, ctu_name, ctu_id, SEGMENT_LENGTH, SEGMENT_LENGTH_CTU,
         ctu_name_full)


# note that we SHOULD NOT use volume/AADT from these segments,
# particularly for geographic aggregations
# here we are using it ot get a sense of scale
ctu_segment_summary <- ctu_segments %>% 
  sf::st_drop_geometry() %>% 
  group_by(gnis, ctu_name, county_name, ctu_id, coctu_id_gnis, coctu_id_fips) %>% 
  summarize(
    total_segment_length = sum(SEGMENT_LENGTH_CTU, na.rm = T),
    axle_factors = paste0(unique(AXLE_FACTOR_GROUP), collapse = ", "),
    sequence_numbers = paste0(unique(SEQUENCE_N), collapse = ", "),
    current_years = paste0(sort(unique(CURRENT_YEAR)), collapse = ", "),
    total_volume = sum(CURRENT_VOLUME, na.rm = T),
    .groups = "keep"
  ) %>% 
  ungroup() %>% 
  mutate(
    region_total_segment_length = sum(total_segment_length),
    ctu_prop_length = total_segment_length/region_total_segment_length
  )

saveRDS(ctu_segments, "_transportation/data-raw/mndot/aadt_ctu_segments.RDS")


ctu_segment_summary %>% 
  filter(gnis %in% ctu_unreliable$gnis)

# Historic AADT -----

download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_dot/trans_historic_aadt/gpkg_trans_historic_aadt.zip",
              destfile = "_transportation/data-raw/mndot/gpkg_trans_historic_aadt.zip")

unzip("_transportation/data-raw/mndot/gpkg_trans_historic_aadt.zip",
      exdir = "_transportation/data-raw/mndot/")

aadt_historic <- sf::read_sf("_transportation/data-raw/mndot/trans_Historic Annual Average Daily Traffic.gpkg") %>%
  filter(SEQUENCE_NUMBER %in% ctu_segments$SEQUENCE_NUMBER) %>%
  mutate(
    SEQUENCE_N = as.character(SEQUENCE_NUMBER))

