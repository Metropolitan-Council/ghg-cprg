### Predict CTU residential natural gas usage with bias-decay blending ###
### Trains on all complete city-years, predicts all missing years 2010-2023
### Uses bidirectional bias-decay correction anchored at known data boundaries

source("R/_load_pkgs.R")
source("_energy/data-raw/_energy_emissions_factors.R")

# load in supporting data

cprg_ctu <- read_rds("_meta/data/cprg_ctu.RDS") %>%
  filter(
    !county_name %in% c("Chisago", "Sherburne", "St. Croix", "Pierce"),
    !thrive_designation == "Non-Council Area"
  ) %>%
  mutate(thrive_designation = as.factor(if_else(
    thrive_designation == "Rural Center",
    "Rural Residential",
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

# get complete city-years utility data

ctu_utility_year <- read_rds("_energy/data/ctu_utility_mcf.RDS") %>%
  group_by(ctu_name, ctu_class, inventory_year) %>%
  filter(!any(is.na(total_mcf))) %>%
  summarize(
    residential_mcf = sum(residential_mcf, na.rm = TRUE),
    business_mcf    = sum(business_mcf, na.rm = TRUE),
    total_mcf       = sum(total_mcf)
  ) %>%
  ungroup()

# population splits for multi-county CTUs

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

# predictor data

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

# training dataset

ng_res_train <- coctu_res_known %>%
  left_join(urbansim_res, by = c("ctu_name", "ctu_class", "county_name", "inventory_year")) %>%
  left_join(mn_parcel_res %>% select(-ctu_name), by = c("ctu_id")) %>%
  left_join(noaa_year, by = "inventory_year") %>%
  filter(!is.na(coctu_id_gnis), residential_mcf != 0)

# train RF on all complete city-years

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
# Including known years so we can compute anchor residuals

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

# ── Bias-decay blending ───────────────────────────────────────────────────────

# Known years with their RF predictions and actual values
known_with_pred <- coctu_res_known %>%
  left_join(
    full_pred_grid %>%
      select(
        coctu_id_gnis, ctu_name, ctu_class, county_name,
        inventory_year, rf_predicted
      ),
    by = c("ctu_name", "ctu_class", "county_name", "inventory_year")
  ) %>%
  mutate(
    residual = residential_mcf - rf_predicted,
    percent_deviation = residual / residential_mcf
  ) %>%
  filter(!is.na(rf_predicted))

# For each missing city-year, find the nearest known year before AND after
missing_years <- full_pred_grid %>%
  anti_join(coctu_res_known,
    by = c("ctu_name", "ctu_class", "county_name", "inventory_year")
  ) %>%
  select(
    coctu_id_gnis, ctu_name, ctu_class, county_name,
    inventory_year, rf_predicted
  )

half_life <- 5 # residual halves every 5 years -- tune per diagnostics below

blended_predictions <- missing_years %>%
  left_join(
    # last known year before each missing year
    known_with_pred %>%
      select(ctu_name, ctu_class, county_name,
        anchor_before_year = inventory_year,
        residual_before    = residual
      ),
    by = c("ctu_name", "ctu_class", "county_name"),
    relationship = "many-to-many"
  ) %>%
  filter(anchor_before_year < inventory_year) %>%
  group_by(coctu_id_gnis, ctu_name, ctu_class, county_name, inventory_year) %>%
  slice_max(anchor_before_year, n = 1) %>%
  ungroup() %>%
  left_join(
    # first known year after each missing year
    known_with_pred %>%
      select(ctu_name, ctu_class, county_name,
        anchor_after_year = inventory_year,
        residual_after    = residual
      ),
    by = c("ctu_name", "ctu_class", "county_name"),
    relationship = "many-to-many"
  ) %>%
  filter(anchor_after_year > inventory_year) %>%
  group_by(coctu_id_gnis, ctu_name, ctu_class, county_name, inventory_year) %>%
  slice_min(anchor_after_year, n = 1) %>%
  ungroup() %>%
  mutate(
    years_from_before = inventory_year - anchor_before_year,
    years_from_after = anchor_after_year - inventory_year,
    w_before = 0.5^(years_from_before / half_life),
    w_after = 0.5^(years_from_after / half_life),
    # normalize so weights sum to 1 -- handles both gap interior and edge cases
    w_before_norm = w_before / (w_before + w_after),
    w_after_norm = w_after / (w_before + w_after),
    correction = residual_before * w_before_norm + residual_after * w_after_norm,
    residential_mcf = rf_predicted + correction,
    data_source = "Model prediction (bias-decay)"
  ) %>%
  select(
    coctu_id_gnis, ctu_name, ctu_class, county_name,
    inventory_year, residential_mcf, data_source
  )

# Edge case: missing years with ONLY a before anchor (trailing gap, no data after)
trailing_gap <- missing_years %>%
  anti_join(blended_predictions,
    by = c("ctu_name", "ctu_class", "county_name", "inventory_year")
  ) %>%
  left_join(
    known_with_pred %>%
      select(ctu_name, ctu_class, county_name,
        anchor_before_year = inventory_year,
        residual_before    = residual
      ),
    by = c("ctu_name", "ctu_class", "county_name"),
    relationship = "many-to-many"
  ) %>%
  filter(anchor_before_year < inventory_year) %>%
  group_by(coctu_id_gnis, ctu_name, ctu_class, county_name, inventory_year) %>%
  slice_max(anchor_before_year, n = 1) %>%
  ungroup() %>%
  mutate(
    years_from_before = inventory_year - anchor_before_year,
    decay = 0.5^(years_from_before / half_life),
    residential_mcf = rf_predicted + residual_before * decay,
    data_source = "Model prediction (bias-decay trailing)"
  ) %>%
  select(
    coctu_id_gnis, ctu_name, ctu_class, county_name,
    inventory_year, residential_mcf, data_source
  )

# Edge case: missing years with ONLY an after anchor (leading gap, no data before)
leading_gap <- missing_years %>%
  anti_join(blended_predictions,
    by = c("ctu_name", "ctu_class", "county_name", "inventory_year")
  ) %>%
  anti_join(trailing_gap,
    by = c("ctu_name", "ctu_class", "county_name", "inventory_year")
  ) %>%
  left_join(
    known_with_pred %>%
      select(ctu_name, ctu_class, county_name,
        anchor_after_year = inventory_year,
        residual_after    = residual
      ),
    by = c("ctu_name", "ctu_class", "county_name"),
    relationship = "many-to-many"
  ) %>%
  filter(anchor_after_year > inventory_year) %>%
  group_by(coctu_id_gnis, ctu_name, ctu_class, county_name, inventory_year) %>%
  slice_min(anchor_after_year, n = 1) %>%
  ungroup() %>%
  mutate(
    years_from_after = anchor_after_year - inventory_year,
    decay = 0.5^(years_from_after / half_life),
    residential_mcf = rf_predicted + residual_after * decay,
    data_source = "Model prediction (bias-decay leading)"
  ) %>%
  select(
    coctu_id_gnis, ctu_name, ctu_class, county_name,
    inventory_year, residential_mcf, data_source
  )

# ── Diagnostics: verify smooth transitions ────────────────────────────────────
# Inspect half_life choice -- plot a few cities with gap years
blending_check <- bind_rows(
  known_with_pred %>%
    select(ctu_name, ctu_class, county_name, inventory_year, residential_mcf) %>%
    mutate(data_source = "Utility report"),
  blended_predictions,
  trailing_gap,
  leading_gap
) %>%
  arrange(ctu_name, ctu_class, county_name, inventory_year)

# plot a city of interest
blending_check %>%
  filter(ctu_name == "Richfield") %>%
  ggplot(aes(inventory_year, residential_mcf, color = ctu_name)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(title = "Rosemount residential MCF -- blending check")

# cities with NO known years at all -- pure RF, no anchor available
no_data_cities <- full_pred_grid %>%
  anti_join(coctu_res_known,
    by = c("ctu_name", "ctu_class", "county_name") # note: no inventory_year
  ) %>% # excludes entire city, not just year
  select(
    coctu_id_gnis, ctu_name, ctu_class, county_name,
    inventory_year, rf_predicted
  ) %>%
  mutate(
    residential_mcf = rf_predicted,
    data_source     = "Model prediction (RF only)"
  ) %>%
  select(-rf_predicted)

## bring back pre-2010 RII data
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

# add into the final bind_rows alongside the three gap types
coctu_res_out <- bind_rows(
  known_pre2010,
  known_with_pred %>%
    select(
      coctu_id_gnis, ctu_name, ctu_class, county_name,
      inventory_year, residential_mcf
    ) %>%
    mutate(data_source = "Utility report"),
  blended_predictions,
  trailing_gap,
  leading_gap,
  no_data_cities
) %>%
  filter(residential_mcf > 0) %>%
  arrange(ctu_name, ctu_class, county_name, inventory_year)

# sanity check
stopifnot(
  coctu_res_out %>%
    count(ctu_name, ctu_class, county_name, inventory_year) %>%
    filter(n > 1) %>%
    nrow() == 0
)

# check - plot a city of interest
coctu_res_out %>%
  filter(ctu_name == "Lake Elmo") %>%
  ggplot(aes(inventory_year, residential_mcf, color = data_source)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(title = "Residential MCF -- blending check")

saveRDS(coctu_res_out, "_energy/data-raw/predicted_coctu_residential_mcf.rds")
