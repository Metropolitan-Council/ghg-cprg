source("R/_load_pkgs.R")

## read in traffic ratios
traffic_ratios <- readRDS(paste0("_transportation/data-raw/mndot/most_recent_yearly_volume_percentage_by_class.RDS"))

## point locations
## https://gisdata.mn.gov/dataset/trans-aadt-traffic-count-locs
tmp_dir <- tempdir()
tmp_file <- tempfile()
download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_dot/trans_aadt_traffic_count_locs/gpkg_trans_aadt_traffic_count_locs.zip",
              destfile = tmp_file)
unzip(tmp_file, exdir = tmp_dir)
list.files(tmp_dir)
locations <- sf::read_sf(paste0(tmp_dir, "/trans_aadt_traffic_count_locs.gpkg")) %>% 
  sf::st_set_crs(26915)
rm(tmp_dir, tmp_file)

## convert to table
locations_table <- as.data.table(locations) %>% 
  clean_names() %>% 
  select(sequence_n,
         route_labe,
         location_d,
         current_ye,
         current_vo,
         geom)

## station list table
station_list <- readxl::read_xlsx("_transportation/data-raw/mndot/Current_CC_StationList.xlsx") %>% 
  clean_names()  %>% 
  filter(collection_type %in% c("ATR Volume, Speed, Class",
                                "WIM"),
         county_name %in% c("Anoka",
                            "Carver",
                            "Dakota",
                            "Hennepin",
                            "Scott",
                            "Ramsey",
                            "Washington",
                            "Chisago",
                            "Sherburne"),
         continuous_number %in% traffic_ratios$station_id) 

## join station list with traffic ratios, locations
stations_ratios <- inner_join(station_list, traffic_ratios,
                              by = c("continuous_number" = "station_id")) %>% 
  mutate(sequence_n = as.double(sequence_number)) %>% 
  left_join(locations_table, by = "sequence_n") %>% 
  st_as_sf()
  # st_transform(crs = 4326) # project to web mercator


# read in AADT
tmp_dir <- tempdir()
tmp_file <- tempfile()
download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_dot/trans_aadt_traffic_segments/gpkg_trans_aadt_traffic_segments.zip",
              destfile = tmp_file)
unzip(tmp_file, exdir = tmp_dir)
list.files(tmp_dir)
aadt <- sf::read_sf(paste0(tmp_dir, "/trans_aadt_traffic_segments.gpkg")) %>% 
  sf::st_set_crs(26915) %>% 
  mutate(aadt_vol = CURRENT_VO)
rm(tmp_dir, tmp_file)

## filter AADT (line) and select cols
aadt_filtered <- aadt %>% 
  filter(SEQUENCE_N %in% traffic_ratios$sequence_number) %>% 
  select(SEQUENCE_N, ROUTE_LABE, LOCATION_D, CURRENT_YE, CURRENT_VO) %>% 
  as.data.table() %>% 
  clean_names()


## join AADT filtered and station ratios. Output is lines
stations_ratios_aadt <- inner_join(aadt_filtered, stations_ratios,
                                   by = "sequence_n") %>% 
  st_as_sf() %>% 
  select(sequence_n, continuous_number, passenger, medium_duty, heavy_duty, 
         current_volume = current_vo.y,
         geom = geom.x) %>% 
  st_cast(to = "LINESTRING") %>% 
  st_zm() 


## export
saveRDS(stations_ratios_aadt, paste0("data/", date, "_2019_plus_stations_ratios.RDS"))



