### Predict CTU residential electricity usage with mean RF scale correction ###
### Trains on all complete city-years, predicts all missing years 2010-2023
### Per-city mean ratio correction matching natural gas residential methodology

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

ctu_utility_mwh <- read_rds("_energy/data/ctu_utility_mwh.RDS")

# Row-level filtering: drop all-NA rows before grouping so a single phantom
# utility doesn't poison a city-year (the Saint Paul / CenterPoint pattern).
ctu_utility_year <- ctu_utility_mwh %>%
  group_by(ctu_name, ctu_class, inventory_year) %>%
  filter(!any(is.na(total_mwh)), total_mwh > 0) %>%
  summarize(
    residential_mwh = sum(residential_mwh, na.rm = TRUE),
    business_mwh    = sum(business_mwh, na.rm = TRUE),
    total_mwh       = sum(total_mwh),
    .groups         = "drop"
  )

# ── Population splits for multi-county CTUs ───────────────────────────────────

coctu_population <- ctu_population %>%
  distinct(ctu_name, ctu_class, inventory_year, county_name, ctu_population) %>%
  group_by(ctu_name, ctu_class, inventory_year) %>%
  mutate(
    total_ctu_population  = sum(ctu_population, na.rm = TRUE),
    coctu_population_prop = ctu_population / total_ctu_population,
    multi_county          = n_distinct(county_name) > 1
  ) %>%
  ungroup()

coctu_res_known <- ctu_utility_year %>%
  full_join(coctu_population,
    by = c("ctu_name", "ctu_class", "inventory_year"),
    relationship = "many-to-many"
  ) %>%
  mutate(
    residential_mwh = if_else(multi_county,
      residential_mwh * coctu_population_prop,
      residential_mwh
    )
  ) %>%
  filter(!is.na(residential_mwh), residential_mwh > 0) %>%
  select(
    ctu_name, ctu_class, inventory_year, residential_mwh,
    county_name, ctu_population
  )

# ── Predictor data ────────────────────────────────────────────────────────────

mn_parcel_res <- mn_parcel %>%
  filter(mc_classification %in% c("single_family_home", "multifamily_home", "apartment")) %>%
  group_by(ctu_name, ctu_id, mc_classification) %>%
  summarize(total_emv = sum(total_emv), mean_year = mean(mean_year)) %>%
  pivot_wider(
    id_cols     = c(ctu_name, ctu_id),
    names_from  = mc_classification,
    values_from = c(total_emv, mean_year)
  ) %>%
  na_replace() %>%
  ungroup()


residential_vars <- c(
  "total_pop", "total_households", "total_residential_units",
  "manufactured_homes", "single_fam_det_sl_own", "single_fam_det_ll_own",
  "single_fam_det_rent", "single_fam_attached_own", "single_fam_attached_rent",
  "multi_fam_own", "multi_fam_rent"
)

urbansim_res <- urbansim %>%
  filter(variable %in% residential_vars) %>%
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

# Recode manufactured homes for Landfall so it doesn't fall out of model

urbansim_res <- urbansim_res %>%
  mutate(
    single_fam_det_sl_own = if_else(
      ctu_name == "Landfall",
      single_fam_det_sl_own + manufactured_homes,
      single_fam_det_sl_own
    )
  )


# ── Training dataset ─────────────────────────────────────────────────────────

elec_res_train <- coctu_res_known %>%
  left_join(urbansim_res, by = c("ctu_name", "ctu_class", "county_name", "inventory_year")) %>%
  left_join(mn_parcel_res %>% select(-ctu_name), by = c("ctu_id")) %>%
  left_join(noaa_year, by = "inventory_year") %>%
  filter(!is.na(coctu_id_gnis), residential_mwh != 0)

# ── Impute regional means for cities missing parcel data ──────────────────────
parcel_cols <- names(mn_parcel_res) %>%
  str_subset("^(mean_year_|total_emv_)")

elec_res_train <- elec_res_train %>%
  mutate(across(
    all_of(parcel_cols),
    ~ if_else(is.na(.), mean(., na.rm = TRUE), .)
  ))

# ── Check thrive_designation factor levels ────────────────────────────────────

elec_res_train %>%
  count(thrive_designation) %>%
  print()

# ── Train RF on all complete city-years ───────────────────────────────────────

set.seed(1029)

rf_res_model <- randomForest(
  residential_mwh ~
    thrive_designation +
    total_pop + total_households + total_residential_units +
    mean_year_apartment + mean_year_multifamily_home + mean_year_single_family_home +
    total_emv_apartment + total_emv_single_family_home + total_emv_multifamily_home +
    single_fam_det_sl_own + single_fam_det_ll_own +
    single_fam_det_rent + single_fam_attached_own +
    single_fam_attached_rent + multi_fam_own + multi_fam_rent +
    cooling_degree_days,
  data = elec_res_train,
  importance = TRUE,
  na.action = na.omit
)

print(rf_res_model)
varImpPlot(rf_res_model, sort = TRUE)

# ── Predict ALL city-years 2010-2023 ─────────────────────────────────────────

full_pred_grid <- cprg_ctu %>%
  st_drop_geometry() %>%
  left_join(
    urbansim_res,
    by = c(
      "gnis" = "ctu_id", "ctu_name", "ctu_class",
      "county_name", "thrive_designation"
    )
  ) %>%
  filter(inventory_year %in% 2010:2023) %>%
  left_join(mn_parcel_res %>% select(-ctu_name), by = c("gnis" = "ctu_id")) %>%
  mutate(across(
    all_of(parcel_cols),
    ~ if_else(is.na(.), mean(., na.rm = TRUE), .)
  )) %>%
  left_join(noaa_year, by = "inventory_year") %>%
  filter(!is.na(coctu_id_gnis)) %>%
  mutate(rf_predicted = predict(rf_res_model, .))

# ── Mean RF scale correction ─────────────────────────────────────────────────

known_with_pred <- coctu_res_known %>%
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
  mutate(scale = residential_mwh / rf_predicted) %>%
  group_by(ctu_name, ctu_class, county_name) %>%
  summarize(mean_scale = mean(scale, na.rm = TRUE), .groups = "drop")

missing_years_out <- full_pred_grid %>%
  anti_join(coctu_res_known,
    by = c("ctu_name", "ctu_class", "county_name", "inventory_year")
  ) %>%
  select(
    coctu_id_gnis, ctu_name, ctu_class, county_name,
    inventory_year, rf_predicted
  ) %>%
  left_join(city_rf_scale, by = c("ctu_name", "ctu_class", "county_name")) %>%
  mutate(
    residential_mwh = if_else(!is.na(mean_scale),
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
    inventory_year, residential_mwh, data_source
  )

# ── Pre-2010 utility data ────────────────────────────────────────────────────

known_pre2010 <- coctu_res_known %>%
  filter(inventory_year < 2010) %>%
  left_join(
    urbansim_res %>% distinct(ctu_name, ctu_class, county_name, coctu_id_gnis),
    by = c("ctu_name", "ctu_class", "county_name")
  ) %>%
  mutate(data_source = "Utility report") %>%
  select(
    coctu_id_gnis, ctu_name, ctu_class, county_name,
    inventory_year, residential_mwh, data_source
  )

# ── Combine ───────────────────────────────────────────────────────────────────

coctu_res_out <- bind_rows(
  known_pre2010,
  known_with_pred %>%
    select(
      coctu_id_gnis, ctu_name, ctu_class, county_name,
      inventory_year, residential_mwh
    ) %>%
    mutate(data_source = "Utility report"),
  missing_years_out
) %>%
  filter(residential_mwh > 0) %>%
  arrange(ctu_name, ctu_class, county_name, inventory_year)

stopifnot(
  coctu_res_out %>%
    count(ctu_name, ctu_class, county_name, inventory_year) %>%
    filter(n > 1) %>%
    nrow() == 0
)

# ── Partial utility guardrails ────────────────────────────────────────────────
# Cities excluded from RF training due to missing utility responses may still
# have partial observations that bound reasonable predictions.

res_guardrails <- ctu_utility_mwh %>%
  group_by(ctu_name, ctu_class, inventory_year) %>%
  summarize(
    res_floor_mwh = sum(residential_mwh, na.rm = TRUE),
    n_na_total    = sum(is.na(total_mwh)),
    n_na_res      = sum(is.na(residential_mwh)),
    .groups       = "drop"
  ) %>%
  filter(n_na_total > 0 | n_na_res > 0) %>%
  mutate(apply_floor = res_floor_mwh > 0) %>%
  select(-n_na_total, -n_na_res)

# ── Apply residential floor ──────────────────────────────────────────────────

coctu_res_adj <- coctu_res_out %>%
  left_join(
    coctu_population %>%
      distinct(
        ctu_name, ctu_class, county_name, inventory_year,
        coctu_population_prop, multi_county
      ),
    by = c("ctu_name", "ctu_class", "county_name", "inventory_year")
  ) %>%
  left_join(
    res_guardrails %>%
      select(ctu_name, ctu_class, inventory_year, res_floor_mwh, apply_floor),
    by = c("ctu_name", "ctu_class", "inventory_year")
  ) %>%
  mutate(
    res_floor_prop = res_floor_mwh * if_else(
      !is.na(multi_county) & multi_county,
      coctu_population_prop,
      1
    ),
    is_modeled = !data_source %in% c("Utility report", "RII utility data"),
    floor_hit = is_modeled &
      !is.na(apply_floor) &
      apply_floor &
      residential_mwh < res_floor_prop,
    residential_mwh = if_else(floor_hit, res_floor_prop, residential_mwh),
    data_source = if_else(
      floor_hit,
      paste0(data_source, " [partial utility floor]"),
      data_source
    )
  ) %>%
  select(
    -coctu_population_prop, -multi_county, -res_floor_mwh, -apply_floor,
    -res_floor_prop, -is_modeled, -floor_hit
  )

# ── Propagate floor corrections to remaining RF-only years ────────────────────
# For cities where some years were floor-adjusted, compute the mean ratio of
# floor-adjusted total to RF-predicted total across those anchor years.
# Apply that scaling factor uniformly to all remaining RF-only years so the
# series is consistent rather than jumping between corrected and uncorrected.

floor_scale <- coctu_res_adj %>%
  filter(grepl("partial utility floor", data_source)) %>%
  left_join(
    full_pred_grid %>%
      select(
        coctu_id_gnis, ctu_name, ctu_class, county_name,
        inventory_year, rf_predicted
      ),
    by = c("coctu_id_gnis", "ctu_name", "ctu_class", "county_name", "inventory_year")
  ) %>%
  mutate(scale = residential_mwh / rf_predicted) %>%
  group_by(ctu_name, ctu_class, county_name) %>%
  summarize(mean_scale = mean(scale, na.rm = TRUE), .groups = "drop")

coctu_res_adj_out <- coctu_res_adj %>%
  left_join(floor_scale, by = c("ctu_name", "ctu_class", "county_name")) %>%
  mutate(
    apply_scale = data_source == "Model prediction (RF only)" & !is.na(mean_scale),
    residential_mwh = if_else(apply_scale, residential_mwh * mean_scale, residential_mwh),
    data_source = if_else(apply_scale,
      "Model prediction (RF only) [floor correction propagated]",
      data_source
    )
  ) %>%
  select(-mean_scale, -apply_scale)

# ── Final integrity check and save ────────────────────────────────────────────

stopifnot(
  coctu_res_adj_out %>%
    count(ctu_name, ctu_class, county_name, inventory_year) %>%
    filter(n > 1) %>%
    nrow() == 0
)

# ── Diagnostic plots ─────────────────────────────────────────────────────────

coctu_res_adj_out %>%
  filter(ctu_name == "Rosemount" & ctu_class == "CITY") %>%
  ggplot(aes(inventory_year, residential_mwh, color = data_source)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(title = "Rosemount residential MWh -- scale correction check")

coctu_res_adj_out %>%
  filter(ctu_name == "Landfall") %>%
  ggplot(aes(inventory_year, residential_mwh, color = data_source)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(title = "Dayton residential MWh -- scale correction check")

coctu_res_adj_out %>%
  filter(ctu_name == "Columbus") %>%
  ggplot(aes(inventory_year, residential_mwh, color = data_source)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(title = "Dayton residential MWh -- scale correction check")

coctu_res_adj_out %>%
  filter(ctu_name == "Saint Paul") %>%
  ggplot(aes(inventory_year, residential_mwh, color = data_source)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(title = "Saint Paul residential MWh -- scale correction check")

# ── Per-capita diagnostic ─────────────────────────────────────────────────────

res_per_capita <- coctu_res_adj_out %>%
  filter(inventory_year == 2022) %>%
  left_join(
    ctu_population %>%
      filter(inventory_year == 2022) %>%
      distinct(ctu_name, ctu_class, county_name, ctu_population),
    by = c("ctu_name", "ctu_class", "county_name")
  ) %>%
  filter(!is.na(ctu_population), ctu_population > 0) %>%
  mutate(mwh_per_capita = residential_mwh / ctu_population) %>%
  select(
    ctu_name, ctu_class, county_name, ctu_population,
    residential_mwh, mwh_per_capita, data_source
  ) %>%
  arrange(desc(mwh_per_capita))

cat("=== Residential MWh per capita, 2022 ===\n")
cat(sprintf(
  "Median: %.1f  Mean: %.1f\n",
  median(res_per_capita$mwh_per_capita),
  mean(res_per_capita$mwh_per_capita)
))

cat("\n--- Top 20 (possible over-prediction) ---\n")
res_per_capita %>%
  head(20) %>%
  print(n = 20, width = Inf)

cat("\n--- Bottom 20 (possible under-reporting) ---\n")
res_per_capita %>%
  tail(20) %>%
  print(n = 20, width = Inf)

saveRDS(coctu_res_adj_out, "_energy/data-raw/predicted_coctu_residential_mwh.rds")
