### Develop model for predicting CTU business natural gas usage ###
# This script should be rerun after all updates to ctu_utility_mcf.RDS
# from script _energy/data-raw/compile_ctu_natGas_records.R
source("R/_load_pkgs.R")
source("_energy/data-raw/_energy_emissions_factors.R")

## load in supporting data
cprg_ctu <- read_rds("_meta/data/cprg_ctu.RDS") %>%
  filter(!county_name %in% c("Chisago", "Sherburne", "St. Croix", "Pierce"))
cprg_county <- read_rds("_meta/data/cprg_county.RDS") %>%
  filter(!county_name %in% c("Chisago", "Sherburne", "St. Croix", "Pierce"))
ctu_population <- read_rds("_meta/data/ctu_population.RDS") %>%
  left_join(cprg_county %>% st_drop_geometry() %>% select(geoid, county_name)) %>%
  filter(!county_name %in% c("Chisago", "Sherburne", "St. Croix", "Pierce"))

## load in utility data, keeping only complete city-years and collapse to city-year
## i.e. if city-year is missing ANY utility data, delete

# bring in weather data
# weather data
noaa <- readRDS("_meta/data/noaa_weather_monthly.rds")

noaa_year <- noaa %>%
  group_by(inventory_year) %>%
  summarize(
    heating_degree_days = sum(heating_degree_days),
    cooling_degree_days = sum(cooling_degree_days),
    temperature = mean(dry_bulb_temp)
  )

ctu_utility_year <- read_rds("_energy/data/ctu_utility_mcf.RDS") %>%
  group_by(ctu_name, ctu_class, inventory_year) %>%
  filter(!any(is.na(total_mcf))) %>%
  summarize(
    residential_mcf = sum(residential_mcf, na.rm = TRUE),
    business_mcf = sum(business_mcf, na.rm = TRUE),
    total_mcf = sum(total_mcf)
  ) %>%
  ungroup()

urbansim <- readRDS("_meta/data/urbansim_data.RDS")
mn_parcel <- readRDS("_meta/data/ctu_parcel_data_2021.RDS") %>%
  mutate(ctu_id = stringr::str_pad(ctu_id, width = 8, pad = "0", side = "left"))

#### BUSINESS ####


business <- c(
  "total_job_spaces",
  "js_type_1011",
  "js_type_13",
  "js_type_14",
  "zones_total_jobs_20_minutes_tt",
  "zones_total_jobs_45_minutes_tt",
  "max_industrial",
  "js_type_12",
  "jobs_sector_4",
  "jobs_sector_5",
  "jobs_sector_6",
  "jobs_sector_7",
  "jobs_sector_8",
  "jobs_sector_9",
  "jobs_sector_10",
  "js_type_12",
  "jobs_sector_1",
  "jobs_sector_2",
  "jobs_sector_3"
)

### create 2010-2025 urbansim residential dataset
urbansim_busi <- urbansim %>%
  filter(variable %in% business) %>%
  group_by(coctu_id_gnis, ctu_id, variable) %>%
  complete(inventory_year = full_seq(c(2005, 2025), 1)) %>% # add interstitial years and expand to 2025
  arrange(coctu_id_gnis, ctu_id, variable, inventory_year) %>%
  mutate(value = approx(inventory_year, value, inventory_year, method = "linear", rule = 2)$y) %>% # allow extrapolation
  ungroup() %>%
  pivot_wider(
    id_cols = c(coctu_id_gnis, ctu_id, inventory_year),
    names_from = variable,
    values_from = value
  ) %>%
  filter(!is.na(coctu_id_gnis)) %>%
  mutate(
    county_id = stringr::str_sub(coctu_id_gnis, start = 1, end = 3),
  ) %>%
  left_join(
    cprg_ctu %>% st_drop_geometry() %>%
      distinct(ctu_name, ctu_class, thrive_designation, gnis),
    by = c("ctu_id" = "gnis")
  ) %>%
  left_join(
    cprg_county %>% st_drop_geometry() %>%
      mutate(geoid = str_sub(geoid, -3, -1)) %>%
      select(county_name, geoid),
    by = c("county_id" = "geoid")
  )

# create jobs proportions for coctu
coctu_jobs <- urbansim_busi %>%
  distinct(ctu_name, ctu_class, ctu_id, inventory_year, county_name, total_job_spaces) %>% # Ensure unique rows per city-county-year
  group_by(ctu_name, ctu_class, ctu_id, inventory_year) %>%
  mutate(
    total_ctu_jobs = sum(total_job_spaces, na.rm = TRUE), # Sum populations across counties for each city-year
    coctu_jobs_prop = total_job_spaces / total_ctu_jobs,
    multi_county = n_distinct(county_name) > 1
  ) %>%
  ungroup()

## split out business mcf based on job proportions for cities in two counties
coctu_busi_year <- ctu_utility_year %>%
  # Join city_total_population back to main dataset
  full_join(coctu_jobs,
    by = c("ctu_name", "ctu_class", "inventory_year"),
    relationship = "many-to-many"
  ) %>%
  # Calculate proportions and disaggregated values
  group_by(ctu_name, ctu_class, inventory_year, county_name) %>%
  mutate(
    business_mcf = ifelse(
      multi_county,
      business_mcf * coctu_jobs_prop,
      business_mcf
    )
  ) %>%
  ungroup() %>%
  filter(!is.na(business_mcf)) %>%
  select(ctu_name, ctu_class, inventory_year, business_mcf, county_name)

# grab mn parcel data for business
mn_parcel_busi <- mn_parcel %>%
  filter(mc_classification %in% c(
    "commercial",
    "industrial",
    "public_building"
  )) %>%
  group_by(ctu_name, ctu_id, mc_classification) %>%
  summarize(total_emv = sum(total_emv), mean_year = mean(mean_year)) %>%
  pivot_wider(
    id_cols = c(ctu_name, ctu_id),
    names_from = mc_classification,
    values_from = c(total_emv, mean_year)
  ) %>%
  na_replace() %>%
  ungroup()


### compare to MPCA nat_gas
# merge into utility data
nat_gas_busi <- left_join(coctu_busi_year,
  urbansim_busi,
  by = c("ctu_name", "ctu_class", "county_name", "inventory_year")
) %>%
  left_join(mn_parcel_busi %>% select(-ctu_name),
    by = c("ctu_id" = "ctu_id")
  ) %>%
  # weather data
  left_join(noaa_year, by = "inventory_year") %>%
  filter(
    !is.na(coctu_id_gnis),
    business_mcf != 0
  )

rf_nonres_model <- randomForest(
  business_mcf ~
    thrive_designation +
    total_job_spaces +
    js_type_1011 +
    js_type_13 +
    js_type_14 +
    total_job_spaces +
    max_industrial +
    js_type_12 +
    jobs_sector_4 +
    jobs_sector_5 +
    jobs_sector_6 +
    jobs_sector_7 +
    jobs_sector_8 +
    jobs_sector_9 +
    jobs_sector_10 +
    jobs_sector_1 +
    jobs_sector_2 +
    jobs_sector_3 +
    cooling_degree_days,
  importance = T, data = nat_gas_busi
)

rf_nonres_model
p_full <- predict(rf_nonres_model, nat_gas_busi)
plot(p_full, nat_gas_busi$business_mcf)
abline(0, 1)

### save rf_res_model output
# saveRDS(rf_nonres_model, "_energy/data/ctu_business_elec_random_forest.RDS")


### predict 2020, 2021, 2022 data for unknown coctu


coctu_busi_predict_rf <- cprg_ctu %>%
  left_join(urbansim_busi, by = c(
    "gnis" = "ctu_id",
    "ctu_name",
    "ctu_class",
    "county_name",
    "thrive_designation"
  )) %>%
  filter(
    !coctu_id_gnis %in% nat_gas_busi$coctu_id_gnis,
    inventory_year %in% c(2020:2022)
  ) %>%
  left_join(mn_parcel_busi %>% select(-ctu_name),
    by = c("gnis" = "ctu_id")
  ) %>%
  # weather data
  left_join(noaa_year, by = "inventory_year") %>%
  mutate(
    mcf_predicted = predict(rf_nonres_model, .),
    data_source = "Model prediction"
  ) %>%
  filter(!is.na(mcf_predicted)) %>%
  st_drop_geometry() %>%
  select(
    coctu_id_gnis,
    ctu_name,
    ctu_class,
    county_name,
    inventory_year,
    mcf_predicted,
    data_source
  )


coctu_busi_out <- bind_rows(
  coctu_busi_year %>%
    left_join(nat_gas_busi %>%
      distinct(
        ctu_name,
        ctu_class,
        county_name,
        coctu_id_gnis
      )) %>%
    filter(!is.na(coctu_id_gnis)) %>%
    select(
      coctu_id_gnis,
      ctu_name,
      ctu_class,
      county_name,
      inventory_year,
      business_mcf
    ) %>%
    mutate(data_source = "Utility report"),
  coctu_busi_predict_rf %>%
    rename(business_mcf = mcf_predicted)
) %>%
  filter(business_mcf != 0)



# save intermediate rds
saveRDS(coctu_busi_out, "_energy/data-raw/predicted_coctu_business_mcf.rds")
