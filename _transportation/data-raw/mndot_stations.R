source("R/_load_pkgs.R")

## read in traffic ratios
traffic_ratios <- readRDS(paste0("_transportation/data-raw/mndot/most_recent_yearly_volume_percentage_by_class.RDS"))

## point locations
## https://gisdata.mn.gov/dataset/trans-aadt-traffic-count-locs
tmp_dir <- tempdir()
tmp_file <- tempfile()
download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_dot/trans_aadt_traffic_count_locs/gpkg_trans_aadt_traffic_count_locs.zip",
  destfile = tmp_file
)
unzip(tmp_file, exdir = tmp_dir)
locations <- sf::read_sf(paste0(tmp_dir, "/trans_aadt_traffic_count_locs.gpkg"))

## convert to table
locations_table <- as.data.table(locations) %>%
  clean_names() %>%
  select(
    sequence_n,
    route_labe,
    location_d,
    current_ye,
    current_vo,
    geom
  )

## station list table
station_list <- readxl::read_xlsx("_transportation/data-raw/mndot/Current_CC_StationList.xlsx") %>%
  clean_names() %>%
  filter(
    collection_type %in% c(
      "ATR Volume, Speed, Class",
      "WIM"
    ),
    county_name %in% c(
      "Anoka",
      "Carver",
      "Dakota",
      "Hennepin",
      "Scott",
      "Ramsey",
      "Washington",
      "Chisago",
      "Sherburne"
    ),
    continuous_number %in% traffic_ratios$station_id
  )

## join station list with traffic ratios, locations
stations_ratios <- inner_join(
  # note that station_id  = continuous number
  station_list, traffic_ratios,
  by = c("continuous_number" = "station_id")
) %>%
  mutate(sequence_n = as.double(sequence_number)) %>%
  left_join(locations_table, by = "sequence_n") %>%
  st_as_sf()


# read in AADT
download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_dot/trans_aadt_traffic_segments/gpkg_trans_aadt_traffic_segments.zip",
  destfile = tmp_file
)
unzip(tmp_file, exdir = tmp_dir)
aadt <- sf::read_sf(paste0(tmp_dir, "/trans_aadt_traffic_segments.gpkg")) %>%
  mutate(aadt_vol = CURRENT_VO)
rm(tmp_dir, tmp_file)

## filter AADT (line) and select cols
aadt_filtered <- aadt %>%
  filter(SEQUENCE_N %in% stations_ratios$sequence_number) %>%
  select(SEQUENCE_N, ROUTE_LABE, LOCATION_D, CURRENT_YE, CURRENT_VO) %>%
  clean_names() %>%
  st_cast(to = "LINESTRING", warn = FALSE) %>%
  st_zm()


## join AADT filtered and station ratios. Output is lines
stations_ratios_aadt <- left_join(
  aadt_filtered,
  stations_ratios %>%
    select(
      sequence_n, continuous_number, year,
      passenger, medium_duty, heavy_duty
    ) %>%
    sf::st_drop_geometry(),
  by = "sequence_n"
) %>%
  select(sequence_n, continuous_number, year,
    passenger, medium_duty, heavy_duty,
    current_volume = current_vo
  ) %>%
  st_transform(4326)


## export
saveRDS(stations_ratios_aadt, paste0("_transportation/data-raw/mndot/2021_plus_stations_ratios.RDS"))

## note, if you are getting "GDAL Error 1: PROJ: proj_create_from_database: Cannot find proj.db"
## and are on a Mac,
## try setting  PROJ_LIB=/usr/local/share/proj in your .Renviron
