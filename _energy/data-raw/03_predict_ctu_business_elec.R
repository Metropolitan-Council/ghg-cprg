### Predict CTU business electricity usage with mean RF scale correction ###
### Trains on all complete city-years, predicts all missing years 2010-2023
### Per-city mean ratio correction matching natural gas business methodology

source("R/_load_pkgs.R")
source("_energy/data-raw/_energy_emissions_factors.R")

# ── Supporting data ───────────────────────────────────────────────────────────

cprg_ctu <- read_rds("_meta/data/cprg_ctu.RDS") %>%
  filter(
    !county_name %in% c("Chisago", "Sherburne", "St. Croix", "Pierce"),
    !thrive_designation == "Non-Council Area"
  ) %>%
  mutate(thrive_designation = as.factor(if_else(
    thrive_designation == "Rural Center", # insufficient data in natgas; check elec below
    "Emerging Suburban Edge",
    thrive_designation
  )))

cprg_county <- read_rds("_meta/data/cprg_county.RDS") %>%
  filter(!county_name %in% c("Chisago", "Sherburne", "St. Croix", "Pierce"))

ctu_population <- read_rds("_meta/data/ctu_population.RDS") %>%
  left_join(cprg_county %>% st_drop_geometry() %>% select(geoid, county_name)) %>%
  filter(!county_name %in% c("Chisago", "Sherburne", "St. Croix", "Pierce"))

noaa_year <- readRDS("_meta/data/noaa_weather_monthly.rds") %>%
  group_by(inventory_year) %>%
  summarize(
    heating_degree_days = sum(heating_degree_days),
    cooling_degree_days = sum(cooling_degree_days),
    temperature         = mean(dry_bulb_temp)
  )

mn_parcel <- readRDS("_meta/data/ctu_parcel_data_2021.RDS") %>%
  mutate(ctu_id = str_pad(ctu_id, width = 8, pad = "0", side = "left"))

urbansim <- readRDS("_meta/data/urbansim_data.RDS")

# ── Utility data ──────────────────────────────────────────────────────────────

ctu_utility_mwh <- read_rds("_energy/data/ctu_utility_mwh.RDS") %>% 
  filter(ctu_name != "Champlin") #something wrong with all years, unresolvable without better utility data

# Row-level filtering: drop all-NA rows before grouping so a single phantom
# utility doesn't poison a city-year (the Saint Paul / CenterPoint pattern).
# If electricity has nonresponder utilities analogous to MER/Greater MN Gas,
# add them to the keep-list below.
ctu_utility_year <- ctu_utility_mwh %>%
  filter(!is.na(total_mwh) | !is.na(residential_mwh) | !is.na(business_mwh)) %>%
  group_by(ctu_name, ctu_class, inventory_year) %>%
  filter(
    !any(is.na(total_mwh)),
    total_mwh > 0
  ) %>%
  summarize(
    residential_mwh = sum(residential_mwh, na.rm = TRUE),
    business_mwh    = sum(business_mwh, na.rm = TRUE),
    total_mwh       = sum(total_mwh),
    .groups         = "drop"
  )

# ── Job splits for multi-county CTUs ─────────────────────────────────────────

business_vars <- c(
  "total_job_spaces",
  "js_type_1011", "js_type_12", "js_type_13", "js_type_14",
  "max_industrial",
  "jobs_sector_1", "jobs_sector_2", "jobs_sector_3",
  "jobs_sector_4", "jobs_sector_5", "jobs_sector_6",
  "jobs_sector_7", "jobs_sector_8", "jobs_sector_9", "jobs_sector_10",
  "zones_total_jobs_20_minutes_tt", "zones_total_jobs_45_minutes_tt"
)

urbansim_busi <- urbansim %>%
  filter(variable %in% business_vars) %>%
  group_by(coctu_id_gnis, ctu_id, variable) %>%
  complete(inventory_year = full_seq(c(2005, 2025), 1)) %>%
  arrange(coctu_id_gnis, ctu_id, variable, inventory_year) %>%
  mutate(value = approx(inventory_year, value, inventory_year,
                        method = "linear", rule = 2
  )$y) %>%
  ungroup() %>%
  pivot_wider(
    id_cols     = c(coctu_id_gnis, ctu_id, inventory_year),
    names_from  = variable,
    values_from = value
  ) %>%
  filter(!is.na(coctu_id_gnis)) %>%
  mutate(county_id = str_sub(coctu_id_gnis, 1, 3)) %>%
  left_join(
    cprg_ctu %>% st_drop_geometry() %>%
      distinct(ctu_name, ctu_class, gnis, thrive_designation),
    by = c("ctu_id" = "gnis")
  ) %>%
  left_join(
    cprg_county %>% st_drop_geometry() %>%
      mutate(geoid = str_sub(geoid, -3, -1)) %>%
      select(county_name, geoid),
    by = c("county_id" = "geoid")
  )

coctu_jobs <- urbansim_busi %>%
  distinct(ctu_name, ctu_class, ctu_id, inventory_year, county_name, total_job_spaces) %>%
  group_by(ctu_name, ctu_class, ctu_id, inventory_year) %>%
  mutate(
    total_ctu_jobs  = sum(total_job_spaces, na.rm = TRUE),
    coctu_jobs_prop = total_job_spaces / total_ctu_jobs,
    multi_county    = n_distinct(county_name) > 1
  ) %>%
  ungroup()

coctu_busi_known <- ctu_utility_year %>%
  full_join(coctu_jobs,
            by = c("ctu_name", "ctu_class", "inventory_year"),
            relationship = "many-to-many"
  ) %>%
  mutate(
    business_mwh = if_else(multi_county,
                           business_mwh * coctu_jobs_prop,
                           business_mwh
    )
  ) %>%
  filter(!is.na(business_mwh), business_mwh > 0) %>%
  select(ctu_name, ctu_class, inventory_year, business_mwh, county_name)

# ── Predictor data ────────────────────────────────────────────────────────────

mn_parcel_busi <- mn_parcel %>%
  filter(mc_classification %in% c("commercial", "industrial", "public_building")) %>%
  group_by(ctu_name, ctu_id, mc_classification) %>%
  summarize(total_emv = sum(total_emv), mean_year = mean(mean_year)) %>%
  pivot_wider(
    id_cols     = c(ctu_name, ctu_id),
    names_from  = mc_classification,
    values_from = c(total_emv, mean_year)
  ) %>%
  na_replace() %>%
  ungroup()

# ── Training dataset ─────────────────────────────────────────────────────────

elec_busi_train <- coctu_busi_known %>%
  left_join(urbansim_busi, by = c("ctu_name", "ctu_class", "county_name", "inventory_year")) %>%
  left_join(mn_parcel_busi %>% select(-ctu_name), by = "ctu_id") %>%
  left_join(noaa_year, by = "inventory_year") %>%
  filter(!is.na(coctu_id_gnis))

# ── Check thrive_designation factor levels ────────────────────────────────────

elec_busi_train %>%
  count(thrive_designation) %>%
  print()

# ── Train RF on all complete city-years ───────────────────────────────────────

set.seed(1029)

rf_busi_model <- randomForest(
  business_mwh ~
    thrive_designation +
    total_job_spaces +
    js_type_1011 + js_type_12 + js_type_13 + js_type_14 +
    max_industrial +
    jobs_sector_1 + jobs_sector_2 + jobs_sector_3 +
    jobs_sector_4 + jobs_sector_5 + jobs_sector_6 +
    jobs_sector_7 + jobs_sector_8 + jobs_sector_9 + jobs_sector_10 +
    cooling_degree_days,
  data = elec_busi_train,
  importance = TRUE,
  na.action = na.omit
)

print(rf_busi_model)
varImpPlot(rf_busi_model, sort = TRUE)

# ── Predict ALL city-years 2010-2023 ─────────────────────────────────────────

full_pred_grid <- cprg_ctu %>%
  st_drop_geometry() %>%
  left_join(
    urbansim_busi,
    by = c(
      "gnis" = "ctu_id", "ctu_name", "ctu_class",
      "county_name", "thrive_designation"
    )
  ) %>%
  filter(inventory_year %in% 2010:2023) %>%
  left_join(mn_parcel_busi %>% select(-ctu_name), by = c("gnis" = "ctu_id")) %>%
  left_join(noaa_year, by = "inventory_year") %>%
  filter(!is.na(coctu_id_gnis)) %>%
  mutate(rf_predicted = predict(rf_busi_model, .))

# ── Mean RF scale correction ─────────────────────────────────────────────────
# For cities with utility-reported anchor years, compute the mean ratio of
# actual to RF-predicted. Apply this scalar to all model-predicted years so
# the full series reflects the city's systematic deviation from the RF baseline.
# Cities with no anchor years get raw RF predictions.

known_with_pred <- coctu_busi_known %>%
  left_join(
    full_pred_grid %>%
      select(
        coctu_id_gnis, ctu_name, ctu_class, county_name,
        inventory_year, rf_predicted
      ),
    by = c("ctu_name", "ctu_class", "county_name", "inventory_year")
  ) %>%
  filter(!is.na(rf_predicted))

city_rf_scale <- known_with_pred %>%
  mutate(scale = business_mwh / rf_predicted) %>%
  group_by(ctu_name, ctu_class, county_name) %>%
  summarize(mean_scale = mean(scale, na.rm = TRUE), .groups = "drop")

missing_years_out <- full_pred_grid %>%
  anti_join(coctu_busi_known,
            by = c("ctu_name", "ctu_class", "county_name", "inventory_year")
  ) %>%
  select(
    coctu_id_gnis, ctu_name, ctu_class, county_name,
    inventory_year, rf_predicted
  ) %>%
  left_join(city_rf_scale, by = c("ctu_name", "ctu_class", "county_name")) %>%
  mutate(
    business_mwh = if_else(!is.na(mean_scale),
                           rf_predicted * mean_scale,
                           rf_predicted
    ),
    data_source = if_else(!is.na(mean_scale),
                          "Model prediction (RF scaled)",
                          "Model prediction (RF only)"
    )
  ) %>%
  select(
    coctu_id_gnis, ctu_name, ctu_class, county_name,
    inventory_year, business_mwh, data_source
  )

# ── Pre-2010 utility data ────────────────────────────────────────────────────

known_pre2010 <- coctu_busi_known %>%
  filter(inventory_year < 2010) %>%
  left_join(
    urbansim_busi %>% distinct(ctu_name, ctu_class, county_name, coctu_id_gnis),
    by = c("ctu_name", "ctu_class", "county_name")
  ) %>%
  mutate(data_source = "Utility report") %>%
  select(
    coctu_id_gnis, ctu_name, ctu_class, county_name,
    inventory_year, business_mwh, data_source
  )

# ── Combine ───────────────────────────────────────────────────────────────────

coctu_busi_out <- bind_rows(
  known_pre2010,
  known_with_pred %>%
    select(
      coctu_id_gnis, ctu_name, ctu_class, county_name,
      inventory_year, business_mwh
    ) %>%
    mutate(data_source = "Utility report"),
  missing_years_out
) %>%
  filter(business_mwh > 0) %>%
  arrange(ctu_name, ctu_class, county_name, inventory_year)

stopifnot(
  coctu_busi_out %>%
    count(ctu_name, ctu_class, county_name, inventory_year) %>%
    filter(n > 1) %>%
    nrow() == 0
)

# ── Partial utility guardrails ────────────────────────────────────────────────
# Cities excluded from RF training due to missing utility responses may still
# have partial observations that bound reasonable predictions.
# No power plant deduction needed for electricity (unlike natgas Shakopee).

util_guardrails <- ctu_utility_mwh %>%
  group_by(ctu_name, ctu_class, inventory_year) %>%
  summarize(
    busi_floor = sum(business_mwh, na.rm = TRUE),
    n_na_total = sum(is.na(total_mwh)),
    n_na_busi  = sum(is.na(business_mwh)),
    .groups    = "drop"
  ) %>%
  filter(n_na_total > 0 | n_na_busi > 0) %>%
  mutate(apply_floor = busi_floor > 0) %>%
  select(-n_na_total, -n_na_busi)

# For cities split between counties, distribute floor by county share
city_pred_totals <- coctu_busi_out %>%
  group_by(ctu_name, ctu_class, inventory_year) %>%
  summarize(city_pred_total = sum(business_mwh, na.rm = TRUE), .groups = "drop")

coctu_busi_adj <- coctu_busi_out %>%
  left_join(city_pred_totals,
            by = c("ctu_name", "ctu_class", "inventory_year")
  ) %>%
  left_join(
    util_guardrails %>%
      select(ctu_name, ctu_class, inventory_year, busi_floor, apply_floor),
    by = c("ctu_name", "ctu_class", "inventory_year")
  ) %>%
  mutate(
    is_modeled   = !data_source %in% c("Utility report", "RII utility data"),
    county_share = if_else(city_pred_total > 0,
                           business_mwh / city_pred_total,
                           1
    ),
    busi_floor = busi_floor * county_share,
    floor_hit  = is_modeled &
      !is.na(apply_floor) &
      apply_floor &
      business_mwh < busi_floor,
    business_mwh = if_else(floor_hit, busi_floor, business_mwh),
    data_source  = if_else(floor_hit,
                           paste0(data_source, " [partial utility floor]"),
                           data_source
    )
  ) %>%
  select(
    -city_pred_total, -county_share, -busi_floor, -apply_floor,
    -is_modeled, -floor_hit
  )

stopifnot(
  coctu_busi_adj %>%
    count(ctu_name, ctu_class, county_name, inventory_year) %>%
    filter(n > 1) %>%
    nrow() == 0
)

# ── Diagnostic plots ─────────────────────────────────────────────────────────

coctu_busi_adj %>%
  filter(ctu_name == "Rosemount") %>%
  ggplot(aes(inventory_year, business_mwh, color = data_source)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(title = "Rosemount business MWh -- scale correction check")

coctu_busi_adj %>%
  filter(ctu_name == "Hollywood") %>%
  ggplot(aes(inventory_year, business_mwh, color = data_source)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(title = "Hollywood business MWh -- scale correction check")

coctu_busi_adj %>%
  filter(ctu_name == "Minneapolis") %>%
  ggplot(aes(inventory_year, business_mwh, color = data_source)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(title = "Minneapolis business MWh -- scale correction check")

saveRDS(coctu_busi_adj, "_energy/data-raw/predicted_coctu_business_mwh.rds")
