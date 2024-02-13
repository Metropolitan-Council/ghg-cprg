# aadt---
# created and downloaded from StL platform

county_aadt <- read.csv("_transportation/data-raw/stl_raw_downloads/1689255_CPRG_County_AADT_motor_trunk_primary/1689255_CPRG_County_AADT_motor_trunk_primary_estimated_aadt_all.csv") %>%
  clean_names() %>%
  mutate(analysis_name = "CPRG_County_AADT_motor_trunk_primary") %>%
  bind_rows(read.csv("_transportation/data-raw/stl_raw_downloads/1689256_CPRG_County_AADT_secondary_tertiary/1689256_CPRG_County_AADT_secondary_tertiary_estimated_aadt_all.csv") %>%
    clean_names() %>%
    mutate(analysis_name = "CPRG_County_AADT_secondary_tertiary")) %>%
  mutate(metric_group = "estimated_aadt_all")


primary_length <- sf::read_sf("_transportation/data-raw/stl_raw_downloads/1689255_CPRG_County_AADT_motor_trunk_primary/Shapefile/1689255_CPRG_County_AADT_motor_trunk_primary_segment/1689255_CPRG_County_AADT_motor_trunk_primary_segment.shp") %>%
  mutate(length = sf::st_length(geometry) %>%
    as.numeric() %>%
    units::as_units("m") %>%
    units::set_units("mile") %>%
    as.numeric())

secondary_length <- sf::read_sf("_transportation/data-raw/stl_raw_downloads/1689256_CPRG_County_AADT_secondary_tertiary/Shapefile/1689256_CPRG_County_AADT_secondary_tertiary_segment/1689256_CPRG_County_AADT_secondary_tertiary_segment.shp") %>%
  mutate(length = sf::st_length(geometry) %>%
    as.numeric() %>%
    units::as_units("m") %>%
    units::set_units("mile") %>%
    as.numeric())

stl_roads <- bind_rows(primary_length, secondary_length) %>%
  left_join(county_aadt %>%
    select(segment_id, county) %>%
    unique())

stl_roads %>%
  sf::st_drop_geometry() %>%
  group_by(county) %>%
  summarize(sum(length))

# saveRDS(county_aadt, "_transportation/data-raw/analysis_runs/county_aadt.RDS")
