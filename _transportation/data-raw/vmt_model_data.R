# create a compiled table of COCTU level data to use in our modeling script -----

source("R/_load_pkgs.R")

## demographic data ----
ctu_urbansim <- read_rds("_meta/data/urbansim_data.RDS") %>% 
  # remove mystery COCTU
  filter(coctu_id_gnis != "13900649741")

cprg_county <- readRDS("_meta/data/cprg_county.RDS")

ctu_population <- readRDS("_meta/data/ctu_population.RDS") %>%
  left_join(cprg_county %>% sf::st_drop_geometry(),
            join_by(geoid)) 

cprg_ctu <- readRDS("_meta/data/cprg_ctu.RDS") %>%
  left_join(cprg_county %>% sf::st_drop_geometry(),
            join_by(county_name, state_name, statefp, state_abb, cprg_area)) %>% 
  mutate(ctu_name_full = paste0(ctu_name, ", ", ctu_class)) %>% 
  left_join(ctu_population %>% 
              select(geoid, gnis, coctu_id_fips, coctu_id_gnis) %>% 
              unique(),
            by = c("gnis", "geoid")) %>% 
  filter(state_abb == "MN")

# create index with all contextual information for each coctu_id_gnis
# independent of year
ctu_coctu_index <- ctu_population %>%
  mutate(ctu_name_full = paste0(ctu_name, ", ", ctu_class),
         ctu_name_full_county = paste0(ctu_name_full, ", ", county_name )) %>% 
  # remove credit river township!
  filter(coctu_id_gnis != "13900663886",
         coctu_id_gnis != "03700664099") %>% 
  select(geoid, gnis, coctu_id_gnis, ctu_name, ctu_name_full, ctu_name_full_county, county_name) %>% 
  unique() %>% 
  left_join(cprg_ctu %>% 
              select(ctu_name, gnis, imagine_designation) %>% sf::st_drop_geometry() %>% unique(),
            by = join_by(gnis, ctu_name)) %>% 
  mutate(imagine_designation = case_when(coctu_id_gnis == "03700664099" ~ "Agricultural", # Empire Township
                                         coctu_id_gnis == "13900663886" ~ "Rural Residential", # Credit River township
                                         TRUE ~ imagine_designation))


# create year and county series, all years
ctu_year_series <- ctu_coctu_index %>% 
  select(coctu_id_gnis) %>% 
  unique() %>% 
  expand_grid(inventory_year = 2010:2050)

testthat::expect_equal(
  length(unique(ctu_coctu_index$coctu_id_gnis)),unique(ctu_year_series$coctu_id_gnis) %>% length())

co_year_series <- ctu_coctu_index %>% 
  select(geoid) %>% 
  unique() %>% 
  expand_grid(inventory_year = 2010:2050)


# create full time series for UrbanSim population, households, jobs -----
ctu_pop_full <- ctu_urbansim %>% 
  filter(variable == "total_pop") %>% 
  mutate(total_pop = value) %>% 
  select(coctu_id_gnis, inventory_year, total_pop) %>%
  full_join(ctu_year_series, by = join_by(coctu_id_gnis, inventory_year)) %>% 
  mutate(total_pop = case_when(inventory_year == 2050 & is.na(total_pop) ~ 0,
                               TRUE ~ total_pop)) %>% 
  group_by(coctu_id_gnis) %>% 
  arrange(coctu_id_gnis, inventory_year) %>% 
  mutate(total_pop = round(zoo::na.approx(total_pop)))


ctu_households_full <- ctu_urbansim %>% 
  filter(variable == "total_households") %>% 
  mutate(total_households = value) %>% 
  select(coctu_id_gnis, inventory_year, total_households) %>%
  full_join(ctu_year_series, by = join_by(coctu_id_gnis, inventory_year)) %>% 
  mutate(total_households = case_when(inventory_year == 2050 & is.na(total_households) ~ 0,
                                      TRUE ~ total_households)) %>% 
  group_by(coctu_id_gnis) %>% 
  arrange(coctu_id_gnis, inventory_year) %>% 
  mutate(total_households = round(zoo::na.approx(total_households)))


ctu_jobs_full <- ctu_urbansim %>% 
  filter(variable == "total_jobs") %>% 
  mutate(total_jobs = value) %>% 
  select(coctu_id_gnis, inventory_year, total_jobs) %>%
  full_join(ctu_year_series, by = join_by(coctu_id_gnis, inventory_year)) %>% 
  mutate(total_jobs = case_when(inventory_year == 2050 & is.na(total_jobs) ~ 0,
                                TRUE ~ total_jobs)) %>% 
  group_by(coctu_id_gnis) %>% 
  arrange(coctu_id_gnis, inventory_year) %>% 
  mutate(total_jobs = round(zoo::na.approx(total_jobs)))

# VMT data -----

## CTU -----
mndot_vmt_ctu <- readRDS("_transportation/data/mndot_vmt_ctu.RDS") %>% 
  # left_join(cprg_county %>% sf::st_drop_geometry(), by = "geoid") %>% 
  mutate(vmt_year = as.numeric(vmt_year)) %>% 
  unique() %>% 
  select(vmt_year, geoid, coctu_id_gnis, daily_vmt, centerline_miles) %>% 
  filter(vmt_year <= 2022,
         vmt_year >= 2010)

ctu_vmt_forecast <- readRDS("_transportation/data-raw/metc_travel_model/ctu_vmt_forecast.RDS") %>% 
  # change 2025 to 2023, which better represents what the model is using
  mutate(vmt_year = ifelse(vmt_year == 2025, 2023, as.numeric(vmt_year))) %>% 
  select(vmt_year, coctu_id_gnis, network_vmt,network_passenger_vmt, network_truck_vmt) %>% 
  full_join(ctu_year_series %>% filter(inventory_year >= 2023),
            by = c("vmt_year" ="inventory_year",
                   "coctu_id_gnis" = "coctu_id_gnis")) %>%
  mutate(network_passenger_vmt = case_when(vmt_year == 2050 & is.na(network_passenger_vmt) ~ 0,
                                           TRUE ~ network_passenger_vmt),
         network_truck_vmt = case_when(vmt_year == 2050 & is.na(network_truck_vmt) ~ 0,
                                       TRUE ~ network_passenger_vmt),
         network_vmt = case_when(vmt_year == 2050 & is.na(network_vmt) ~ 0,
                                 TRUE ~ network_vmt)
  ) %>% 
  group_by(coctu_id_gnis) %>% 
  arrange(coctu_id_gnis, vmt_year) %>% 
  mutate(
    # network_passenger_vmt = round(zoo::na.approx(network_passenger_vmt)),
    # network_truck_vmt = round(zoo::na.approx(network_truck_vmt)),
    network_vmt = round(zoo::na.approx(network_vmt)),
    daily_vmt = network_vmt) %>% 
  left_join(ctu_coctu_index %>% 
              select(coctu_id_gnis, geoid) %>% unique(),
            by = join_by(coctu_id_gnis)) %>% 
  select(coctu_id_gnis, geoid, vmt_year, daily_vmt)



ctu_mndot_forecast_vmt <- mndot_vmt_ctu %>% 
  mutate(vmt_source = "MnDOT") %>% 
  bind_rows(ctu_vmt_forecast %>% 
              mutate(vmt_source = "Regional Travel Demand Model")) %>% 
  arrange(coctu_id_gnis, vmt_year) %>% 
  select(coctu_id_gnis, geoid, vmt_source, vmt_year, daily_vmt, centerline_miles) %>% 
  filter(!is.na(geoid)) %>% 
  arrange(coctu_id_gnis, vmt_year)


## County -----
mndot_vmt_county <- readRDS("_transportation/data-raw/mndot/mndot_vmt_county.RDS") %>% 
  mutate(vmt_year = as.numeric(year),
         county_daily_vmt = daily_vmt,
         county_name = county) %>% 
  left_join(ctu_coctu_index %>% 
              select(geoid, county_name) %>% 
              unique(),
            by = c("county_name")) %>% 
  filter(vmt_year <= 2022,
         vmt_year >= 2010) %>% 
  # remove wright and sherburne
  filter(!is.na(geoid))

county_vmt_forecast <- readRDS("_transportation/data-raw/metc_travel_model/county_vmt_forecast.RDS") %>% 
  mutate(vmt_year = ifelse(vmt_year == 2025, 2023, as.numeric(vmt_year))) %>% 
  select(vmt_year, geoid, network_vmt,network_passenger_vmt, network_truck_vmt) %>% 
  full_join(co_year_series %>% filter(inventory_year >= 2023),
            by  =c("vmt_year" ="inventory_year",
                   "geoid" = "geoid")) %>%
  group_by(geoid) %>% 
  arrange(geoid, vmt_year) %>% 
  mutate(
    # network_passenger_vmt = round(zoo::na.approx(network_passenger_vmt)),
    # network_truck_vmt = round(zoo::na.approx(network_truck_vmt)),
    network_vmt = round(zoo::na.approx(network_vmt)),
    county_daily_vmt = network_vmt) %>% 
  select(geoid, vmt_year, county_daily_vmt) %>% 
  ungroup()



county_mndot_vmt_forecast <- county_vmt_forecast %>% 
  mutate(vmt_source_county = "Regional Travel Demand Model") %>% 
  bind_rows(mndot_vmt_county %>% 
              select(vmt_year, geoid, county_daily_vmt, county_centerline_miles = centerline_miles) %>% 
              mutate(vmt_source_county = "MnDOT")) %>% 
  left_join(ctu_coctu_index %>% 
              select(geoid, county_name) %>% 
              unique(), by = c("geoid")) %>% 
  filter(!is.na(geoid)) %>% 
  arrange(geoid, vmt_year)

# combine all tables -----
ctu_pop_jobs_vmt <- ctu_jobs_full %>% 
  left_join(ctu_pop_full, join_by(coctu_id_gnis, inventory_year)) %>% 
  left_join(ctu_households_full, join_by(coctu_id_gnis, inventory_year)) %>% 
  mutate(geoid = paste0( "27", stringr::str_sub(coctu_id_gnis, 1,3))) %>% 
  left_join(ctu_coctu_index %>% 
              select(geoid, county_name) %>% 
              unique(),
            by = c("geoid")) %>% 
  filter(!is.na(geoid), geoid != "27NA") %>% 
  group_by(geoid, inventory_year) %>% 
  mutate(county_total_jobs = sum(total_jobs),
         county_total_pop = sum(total_pop),
         county_total_hh = sum(total_households)) %>% 
  ungroup() %>% 
  left_join(
    county_mndot_vmt_forecast %>% 
      mutate(inventory_year = as.numeric(vmt_year)) %>% 
      select(inventory_year,
             county_daily_vmt, geoid, vmt_source_county),
    join_by(inventory_year, geoid)
  ) %>% 
  full_join(ctu_mndot_forecast_vmt %>% 
              mutate(inventory_year = vmt_year),
            by = c("coctu_id_gnis", "inventory_year", "geoid")) %>% 
  left_join(ctu_coctu_index,
            join_by(coctu_id_gnis, geoid, county_name)) %>%
  # filter(!is.na(gnis)) %>% 
  unique() %>% 
  select(-vmt_year)


testthat::expect_equal(
  ctu_pop_jobs_vmt %>% filter(is.na(geoid) | is.na(coctu_id_gnis) | is.na(ctu_name)) %>% select(coctu_id_gnis, geoid, county_name) %>% unique() %>% nrow(),
  0)

testthat::expect_equal(
  unique(ctu_pop_jobs_vmt$coctu_id_gnis) %>% length(),
  193)

# remove intermediary tables
rm(ctu_jobs_full, ctu_households_full, ctu_pop_full)
