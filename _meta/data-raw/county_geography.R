source("R/_load_pkgs.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
# fetch county geographies for all counties in
# Minnesota and Wisconsin
# This will give us GEOID, NAME, STATE, and other identifying columns
county_geography <- bind_rows(
  tigris::counties(state = "MN") %>%
    mutate(
      state_name = "Minnesota",
      state_abb = "MN"
    ),
  tigris::counties(state = "WI") %>%
    mutate(
      state_name = "Wisconsin",
      state_abb = "WI"
    )
) %>%
  clean_names() %>% 
  mutate(cprg_area = ifelse(geoid %in% cprg_county$geoid, TRUE, FALSE),
         county_name = name) %>% 
  sf::st_drop_geometry()

counties_light <- county_geography %>% 
  select(geoid, state_name, county_name, cprg_area)
