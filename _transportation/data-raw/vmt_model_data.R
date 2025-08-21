# create a compiled table of COCTU level data to use in our modeling script -----

source("R/_load_pkgs.R")

## demographic data ----
ctu_urbansim <- read_rds("_meta/data/urbansim_data.RDS") %>%
  # remove mystery COCTU
  filter(coctu_id_gnis != "13900649741")
source("_meta/data-raw/ctu_coctu_index.R")

# create year and county series, all years
ctu_year_series <- ctu_coctu_index %>%
  select(coctu_id_gnis) %>%
  unique() %>%
  expand_grid(inventory_year = 2010:2050)

testthat::expect_equal(
  length(unique(ctu_coctu_index$coctu_id_gnis)), unique(ctu_year_series$coctu_id_gnis) %>% length()
)

co_year_series <- ctu_coctu_index %>%
  select(geoid) %>%
  unique() %>%
  expand_grid(inventory_year = 2010:2050)


# create full time series for UrbanSim population, households, jobs -----
process_ctu_var <- function(df, var_name, new_col) {
  df %>%
    # get variable name
    filter(variable == var_name) %>%
    # rename with new column name
    mutate(!!new_col := value) %>%
    select(coctu_id_gnis, inventory_year, !!sym(new_col)) %>%
    # join wth ctu year series to get all interstitial years
    full_join(ctu_year_series, by = join_by(coctu_id_gnis, inventory_year)) %>%
    mutate(
      !!new_col := case_when(
        inventory_year == 2050 & is.na(!!sym(new_col)) ~ 0,
        TRUE ~ !!sym(new_col)
      )
    ) %>%
    # group by coctu_id_gnis and arrange
    group_by(coctu_id_gnis) %>%
    arrange(coctu_id_gnis, inventory_year) %>%
    # interpolate  interstitial years using na.approx
    mutate(!!new_col := round(zoo::na.approx(!!sym(new_col)))) %>%
    ungroup()
}

ctu_urbansim_combined <- list(
  process_ctu_var(ctu_urbansim, "total_pop", "total_pop"),
  process_ctu_var(ctu_urbansim, "total_households", "total_households"),
  process_ctu_var(ctu_urbansim, "total_jobs", "total_jobs")
) %>%
  reduce(full_join, by = c("coctu_id_gnis", "inventory_year"))

# VMT data -----

## CTU -----
mndot_vmt_ctu <- readRDS("_transportation/data/mndot_vmt_ctu.RDS") %>%
  # left_join(cprg_county %>% sf::st_drop_geometry(), by = "geoid") %>%
  mutate(vmt_year = as.numeric(vmt_year)) %>%
  unique() %>%
  select(vmt_year, geoid, coctu_id_gnis, daily_vmt, centerline_miles) %>%
  filter(
    vmt_year <= 2022,
    vmt_year >= 2010
  )

ctu_vmt_forecast <- readRDS("_transportation/data/rtdm_forecast_ctu.RDS") %>%
  # change 2025 to 2023, which better represents what the model is using
  select(vmt_year, coctu_id_gnis, network_vmt, network_passenger_vmt, network_truck_vmt) %>%
  full_join(ctu_year_series %>% filter(inventory_year >= 2023),
    by = c(
      "vmt_year" = "inventory_year",
      "coctu_id_gnis" = "coctu_id_gnis"
    )
  ) %>%
  mutate(
    network_passenger_vmt = case_when(
      vmt_year == 2050 & is.na(network_passenger_vmt) ~ 0,
      TRUE ~ network_passenger_vmt
    ),
    network_truck_vmt = case_when(
      vmt_year == 2050 & is.na(network_truck_vmt) ~ 0,
      TRUE ~ network_passenger_vmt
    ),
    network_vmt = case_when(
      vmt_year == 2050 & is.na(network_vmt) ~ 0,
      TRUE ~ network_vmt
    )
  ) %>%
  group_by(coctu_id_gnis) %>%
  arrange(coctu_id_gnis, vmt_year) %>%
  mutate(
    # network_passenger_vmt = round(zoo::na.approx(network_passenger_vmt)),
    # network_truck_vmt = round(zoo::na.approx(network_truck_vmt)),
    network_vmt = round(zoo::na.approx(network_vmt)),
    daily_vmt = network_vmt
  ) %>%
  left_join(
    ctu_coctu_index %>%
      select(coctu_id_gnis, geoid) %>% unique(),
    by = join_by(coctu_id_gnis)
  ) %>%
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
  mutate(
    vmt_year = as.numeric(year),
    county_daily_vmt = daily_vmt,
    county_name = county
  ) %>%
  left_join(
    ctu_coctu_index %>%
      select(geoid, county_name) %>%
      unique(),
    by = c("county_name")
  ) %>%
  filter(
    vmt_year <= 2022,
    vmt_year >= 2010
  ) %>%
  # remove wright and sherburne
  filter(!is.na(geoid))

county_vmt_forecast <- readRDS("_transportation/data/rtdm_forecast_county.RDS") %>%
  select(vmt_year, geoid, network_vmt, network_passenger_vmt, network_truck_vmt) %>%
  full_join(co_year_series %>% filter(inventory_year >= 2023),
    by = c(
      "vmt_year" = "inventory_year",
      "geoid" = "geoid"
    )
  ) %>%
  group_by(geoid) %>%
  arrange(geoid, vmt_year) %>%
  mutate(
    # network_passenger_vmt = round(zoo::na.approx(network_passenger_vmt)),
    # network_truck_vmt = round(zoo::na.approx(network_truck_vmt)),
    network_vmt = round(zoo::na.approx(network_vmt)),
    county_daily_vmt = network_vmt
  ) %>%
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
ctu_pop_jobs_vmt <- ctu_urbansim_combined %>%
  mutate(geoid = paste0("27", stringr::str_sub(coctu_id_gnis, 1, 3))) %>%
  left_join(
    ctu_coctu_index %>%
      select(geoid, county_name) %>%
      unique(),
    by = c("geoid")
  ) %>%
  filter(!is.na(geoid), geoid != "27NA") %>%
  group_by(geoid, inventory_year) %>%
  mutate(
    county_total_jobs = sum(total_jobs),
    county_total_pop = sum(total_pop),
    county_total_households = sum(total_households)
  ) %>%
  ungroup() %>%
  left_join(
    county_mndot_vmt_forecast %>%
      mutate(inventory_year = as.numeric(vmt_year)) %>%
      select(
        inventory_year,
        county_daily_vmt, geoid, vmt_source_county
      ),
    join_by(inventory_year, geoid)
  ) %>%
  full_join(
    ctu_mndot_forecast_vmt %>%
      mutate(inventory_year = vmt_year),
    by = c("coctu_id_gnis", "inventory_year", "geoid")
  ) %>%
  left_join(
    ctu_coctu_index,
    join_by(coctu_id_gnis, geoid, county_name)
  ) %>%
  # filter(!is.na(gnis)) %>%
  unique() %>%
  select(-vmt_year)


testthat::expect_equal(
  ctu_pop_jobs_vmt %>% filter(is.na(geoid) | is.na(coctu_id_gnis) | is.na(ctu_name)) %>%
    select(coctu_id_gnis, geoid, county_name) %>% unique() %>% nrow(),
  0
)

testthat::expect_equal(
  unique(ctu_pop_jobs_vmt$coctu_id_gnis) %>% length(),
  193
)

saveRDS(ctu_pop_jobs_vmt, "_transportation/data/vmt_model_data.RDS")

# create metadata


ctu_population_meta <- read_rds("_meta/data/ctu_population_meta.RDS")
dot_vmt_meta <- readRDS("_transportation/data/dot_vmt_meta.RDS")
cprg_ctu_meta <- readRDS("_meta/data/cprg_ctu_meta.RDS")
rtdm_forecast_ctu_meta <- readRDS("_transportation/data/rtdm_forecast_ctu_meta.RDS")

urbansim_meta <- ctu_urbansim %>%
  filter(variable %in% names(ctu_pop_jobs_vmt)) %>%
  select(variable, definition) %>%
  unique() %>%
  mutate(
    Column = variable,
    Class = c(class(ctu_pop_jobs_vmt$total_households), class(ctu_pop_jobs_vmt$total_jobs), class(ctu_pop_jobs_vmt$total_pop)),
    Description = paste0(stringr::str_to_sentence(definition), ". UrbanSim output")
  ) %>%
  select(names(ctu_population_meta))


ctu_pop_jobs_vmt_meta <- ctu_population_meta %>%
  filter(Column %in% names(ctu_pop_jobs_vmt)) %>%
  bind_rows(urbansim_meta) %>%
  bind_rows(cprg_ctu_meta %>% filter(Column %in% names(ctu_pop_jobs_vmt))) %>%
  bind_rows(rtdm_forecast_ctu_meta %>% filter(Column %in% names(ctu_pop_jobs_vmt))) %>%
  bind_rows(
    tibble::tribble(
      ~Column, ~Class, ~Description,
      "daily_vmt", class(ctu_pop_jobs_vmt$daily_vmt), "CTU vehicle miles traveled (VMT) on an average day",
      "county_daily_vmt", class(ctu_pop_jobs_vmt$county_daily_vmt), "CTU vehicle miles traveled (VMT) on an average day",
      "centerline_miles", class(ctu_pop_jobs_vmt$centerline_miles), "CTU centerline miles",
      "vmt_source", class(ctu_pop_jobs_vmt$vmt_source), "CTU VMT data source",
      "vmt_source_county", class(ctu_pop_jobs_vmt$vmt_source), "County VMT data source",
      "county_total_households", class(ctu_pop_jobs_vmt$county_total_households), "Total county number of households. UrbanSim output",
      "county_total_jobs", class(ctu_pop_jobs_vmt$county_total_jobs), "Total county payroll employment. UrbanSim output",
      "county_total_pop", class(ctu_pop_jobs_vmt$county_total_pop), "Total county population. UrbanSim output",
      "ctu_name_full_county", class(ctu_pop_jobs_vmt$ctu_name_full_county), "CTU name, class, and county name"
    )
  ) %>%
  unique() %>%
  arrange(match(Column, names(ctu_pop_jobs_vmt)))

saveRDS(ctu_pop_jobs_vmt_meta, "_transportation/data/vmt_model_data_meta.RDS")
