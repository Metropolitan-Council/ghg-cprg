source("R/_load_pkgs.R")

# load the county boundaries layer
cprg_county <- readRDS("_meta/data/cprg_county.RDS")

### load emissions and downscaling var (proportion of county ag land)
agricultural_emissions <- read_rds( "./_agriculture/data/_agricultural_emissions.rds")
ctu_ag_proportion <- read_rds("./_agriculture/data/ctu_ag_proportion.rds")

agricultural_emissions_2019 <- left_join(ctu_ag_proportion,
                                         cprg_county %>% 
                                         select(geoid, county_name) %>% 
                                         st_drop_geometry(),
                                         by = 'county_name') %>% 
  left_join(., agricultural_emissions %>% 
              group_by(inventory_year, geoid) %>% 
              summarize(mt_co2e = sum(mt_co2e)),
            by = c("year" = "inventory_year", "geoid")) %>% 
  filter(year == 2019) %>% 
  mutate(ctu_emissions = proportion_ag_land * mt_co2e)

### map CTU scale agricultural emissions

cprg_ctu_9 <- councilR::import_from_gpkg("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_dot/bdry_mn_city_township_unorg/gpkg_bdry_mn_city_township_unorg.zip") %>%
  filter(COUNTY_NAM %in% c(cprg_county$county_name)) %>%
  mutate(
    STATEFP = "27",
    STATE = "Minnesota",
    STATE_ABB = "MN"
  ) %>%
  select(
    CTU_NAME = FEATURE_NA,
    CTU_CLASS,
    COUNTY_NAM,
    STATEFP,
    STATE,
    STATE_ABB,
    GNIS_FEATU,
    geometry = geom
  ) %>%
  arrange(CTU_NAME)


ctu_ag_map <- left_join(cprg_ctu_9 %>% 
                          rename(ctu = CTU_NAME), 
                        agricultural_emissions_2019,
                        by = "ctu") %>% 
  select(ctu, area,  ctu_emissions)

# Plot the agricultural area on the map
ggplot(data = ctu_ag_map) +
  geom_sf(aes(fill = area)) +
  scale_fill_viridis_c(option = "inferno") +  # You can change the color scale here
  theme_minimal() +
  labs(fill = "Agricultural area")

# Plot the emissions on the map
ggplot(data = ctu_ag_map) +
  geom_sf(aes(fill = ctu_emissions)) +
  scale_fill_viridis_c(option = "plasma") +  # You can change the color scale here
  theme_minimal() +
  labs(fill = "CTU Emissions", title = "CTU Emissions Map")
