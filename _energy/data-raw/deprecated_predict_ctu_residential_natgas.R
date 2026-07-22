### Predict CTU residential natural gas usage with mean RF scale correction ###
### Trains on all complete city-years, predicts all missing years 2010-2023
### Per-city mean ratio correction matching business methodology

source("R/_load_pkgs.R")
source("_energy/data-raw/_energy_emissions_factors.R")

# ── Supporting data ───────────────────────────────────────────────────────────

cprg_ctu <- read_rds("_meta/data/cprg_ctu.RDS") %>%
  filter(
    !county_name %in% c("Chisago", "Sherburne", "St. Croix", "Pierce"),
    !thrive_designation == "Non-Council Area"
  ) %>%
  mutate(thrive_designation = as.factor(if_else(
    thrive_designation == "Rural Center",
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

# ── Utility data: complete city-years only ────────────────────────────────────

ctu_utility_year <- read_rds("_energy/data/ctu_utility_mcf.RDS") %>%
  group_by(ctu_name, ctu_class, inventory_year) %>%
  filter(!any(is.na(total_mcf))) %>%
  summarize(
    residential_mcf = sum(residential_mcf, na.rm = TRUE),
    business_mcf    = sum(business_mcf, na.rm = TRUE),
    total_mcf       = sum(total_mcf)
  ) %>%
  ungroup()

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
    residential_mcf = if_else(multi_county,
      residential_mcf * coctu_population_prop,
      residential_mcf
    )
  ) %>%
  filter(!is.na(residential_mcf), residential_mcf > 0) %>%
  select(
    ctu_name, ctu_class, inventory_year, residential_mcf,
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

# ── Training dataset ─────────────────────────────────────────────────────────

ng_res_train <- coctu_res_known %>%
  left_join(urbansim_res, by = c("ctu_name", "ctu_class", "county_name", "inventory_year")) %>%
  left_join(mn_parcel_res %>% select(-ctu_name), by = c("ctu_id")) %>%
  left_join(noaa_year, by = "inventory_year") %>%
  filter(!is.na(coctu_id_gnis), residential_mcf != 0)

# ── Train RF on all complete city-years ───────────────────────────────────────

set.seed(1029)

rf_res_model <- randomForest(
  residential_mcf ~
    thrive_designation +
    total_pop + total_households + total_residential_units +
    single_fam_det_sl_own + single_fam_det_ll_own +
    single_fam_det_rent + single_fam_attached_own +
    single_fam_attached_rent + multi_fam_own + multi_fam_rent +
    heating_degree_days,
  data = ng_res_train,
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
  left_join(noaa_year, by = "inventory_year") %>%
  filter(!is.na(coctu_id_gnis)) %>%
  mutate(rf_predicted = predict(rf_res_model, .))

# ── Mean RF scale correction ─────────────────────────────────────────────────
# For cities with utility-reported anchor years, compute the mean ratio of
# actual to RF-predicted. Apply this scalar to all model-predicted years so
# the full series reflects the city's systematic deviation from the RF baseline.
# Cities with no anchor years get raw RF predictions.

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
  mutate(scale = residential_mcf / rf_predicted) %>%
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
    residential_mcf = if_else(!is.na(mean_scale),
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
    inventory_year, residential_mcf, data_source
  )

# ── Pre-2010 RII data ────────────────────────────────────────────────────────

known_pre2010 <- coctu_res_known %>%
  filter(inventory_year < 2010) %>%
  left_join(
    urbansim_res %>% distinct(ctu_name, ctu_class, county_name, coctu_id_gnis),
    by = c("ctu_name", "ctu_class", "county_name")
  ) %>%
  mutate(data_source = "RII utility data") %>%
  select(
    coctu_id_gnis, ctu_name, ctu_class, county_name,
    inventory_year, residential_mcf, data_source
  )

# ── Combine ───────────────────────────────────────────────────────────────────

coctu_res_out <- bind_rows(
  known_pre2010,
  known_with_pred %>%
    select(
      coctu_id_gnis, ctu_name, ctu_class, county_name,
      inventory_year, residential_mcf
    ) %>%
    mutate(data_source = "Utility report"),
  missing_years_out
) %>%
  filter(residential_mcf > 0) %>%
  arrange(ctu_name, ctu_class, county_name, inventory_year)

stopifnot(
  coctu_res_out %>%
    count(ctu_name, ctu_class, county_name, inventory_year) %>%
    filter(n > 1) %>%
    nrow() == 0
)

# ── Diagnostic plots ─────────────────────────────────────────────────────────

coctu_res_out %>%
  filter(ctu_name == "Saint Paul") %>%
  ggplot(aes(inventory_year, residential_mcf, color = data_source)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(title = "Residential MCF -- scale correction check")

coctu_res_out %>%
  filter(ctu_name == "South Saint Paul") %>%
  ggplot(aes(inventory_year, residential_mcf, color = data_source)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(title = "Lake Elmo residential MCF -- scale correction check")

saveRDS(coctu_res_out, "_energy/data-raw/predicted_coctu_residential_mcf.rds")
