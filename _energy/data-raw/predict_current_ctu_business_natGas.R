### Predict CTU business natural gas usage with bias-decay blending ###
### Trains on all complete city-years, predicts all missing years 2010-2023
### Uses bidirectional bias-decay correction anchored at known data boundaries

source("R/_load_pkgs.R")
source("_energy/data-raw/_energy_emissions_factors.R")

# ── Supporting data ───────────────────────────────────────────────────────────

cprg_ctu <- read_rds("_meta/data/cprg_ctu.RDS") %>%
  filter(
    !county_name %in% c("Chisago", "Sherburne", "St. Croix", "Pierce"),
    !thrive_designation == "Non-Council Area"
  ) %>%
  mutate(thrive_designation = as.factor(if_else(
    thrive_designation == "Rural Center", #insufficient data
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

ctu_utility_mcf <- read_rds("_energy/data/ctu_utility_mcf.RDS")

ctu_utility_year_raw <- ctu_utility_mcf %>%
  filter(!utility %in% c("ST. CROIX VALLEY NATURAL GAS",
                         "WISCONSIN GAS CO")) %>%
  filter(
    utility %in% c("Minnesota Energy Resources",
                   "GREATER MINNESOTA GAS INC.",
                   "Centennial Utilities") |
      !is.na(total_mcf) | !is.na(residential_mcf) | !is.na(business_mcf)
  )

ctu_utility_year <- ctu_utility_year_raw %>%
  mutate(
    residential_mcf = if_else(
      !is.na(business_mcf) & is.na(residential_mcf) &
        !is.na(total_mcf) & total_mcf > 0 &
        abs(business_mcf - total_mcf) / total_mcf < 0.01,
      0,
      residential_mcf
    )
  ) %>%
  group_by(ctu_name, ctu_class, inventory_year) %>%
  filter(
    !any(is.na(total_mcf)),
    total_mcf > 0,
    (sum(residential_mcf, na.rm = TRUE) + sum(business_mcf, na.rm = TRUE)) /
      sum(total_mcf) >= 0.95
  ) %>%
  summarize(
    residential_mcf = sum(residential_mcf, na.rm = TRUE),
    business_mcf    = sum(business_mcf,    na.rm = TRUE),
    total_mcf       = sum(total_mcf)
  ) %>%
  ungroup()

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
                        method = "linear", rule = 2)$y) %>%
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
            by      = c("ctu_name", "ctu_class", "inventory_year"),
            relationship = "many-to-many"
  ) %>%
  mutate(
    business_mcf = if_else(multi_county,
                           business_mcf * coctu_jobs_prop,
                           business_mcf
    )
  ) %>%
  filter(!is.na(business_mcf), business_mcf > 0) %>%
  select(ctu_name, ctu_class, inventory_year, business_mcf, county_name)

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

# ── Training dataset ──────────────────────────────────────────────────────────

ng_busi_train <- coctu_busi_known %>%
  left_join(urbansim_busi, by = c("ctu_name", "ctu_class", "county_name", "inventory_year")) %>%
  left_join(mn_parcel_busi %>% select(-ctu_name), by = "ctu_id") %>%
  left_join(noaa_year, by = "inventory_year") %>%
  filter(!is.na(coctu_id_gnis))

# ── Train RF on all complete city-years ───────────────────────────────────────

set.seed(1029)

rf_busi_model <- randomForest(
  business_mcf ~
    thrive_designation +
    total_job_spaces +
    js_type_1011 + js_type_12 + js_type_13 + js_type_14 +
    max_industrial +
    jobs_sector_1 + jobs_sector_2 + jobs_sector_3 +
    jobs_sector_4 + jobs_sector_5 + jobs_sector_6 +
    jobs_sector_7 + jobs_sector_8 + jobs_sector_9 + jobs_sector_10 +
    heating_degree_days,
  data       = ng_busi_train,
  importance = TRUE,
  na.action  = na.omit
)

print(rf_busi_model)
varImpPlot(rf_busi_model, sort = TRUE)

# ── Predict ALL city-years 2010-2023 ─────────────────────────────────────────

full_pred_grid <- cprg_ctu %>%
  st_drop_geometry() %>%
  left_join(
    urbansim_busi,
    by = c("gnis" = "ctu_id", "ctu_name", "ctu_class",
           "county_name", "thrive_designation")
  ) %>%
  filter(inventory_year %in% 2010:2023) %>%
  left_join(mn_parcel_busi %>% select(-ctu_name), by = c("gnis" = "ctu_id")) %>%
  left_join(noaa_year, by = "inventory_year") %>%
  filter(!is.na(coctu_id_gnis)) %>%
  mutate(rf_predicted = predict(rf_busi_model, .))

# ── Bias-decay blending ───────────────────────────────────────────────────────

known_with_pred <- coctu_busi_known %>%
  left_join(
    full_pred_grid %>%
      select(coctu_id_gnis, ctu_name, ctu_class, county_name,
             inventory_year, rf_predicted),
    by = c("ctu_name", "ctu_class", "county_name", "inventory_year")
  ) %>%
  mutate(
    residual          = business_mcf - rf_predicted,
    percent_deviation = residual / business_mcf
  ) %>%
  filter(!is.na(rf_predicted))

missing_years <- full_pred_grid %>%
  anti_join(coctu_busi_known,
            by = c("ctu_name", "ctu_class", "county_name", "inventory_year")
  ) %>%
  select(coctu_id_gnis, ctu_name, ctu_class, county_name,
         inventory_year, rf_predicted)

half_life <- 5

blended_predictions <- missing_years %>%
  left_join(
    known_with_pred %>%
      select(ctu_name, ctu_class, county_name,
             anchor_before_year = inventory_year,
             residual_before    = residual),
    by = c("ctu_name", "ctu_class", "county_name"),
    relationship = "many-to-many"
  ) %>%
  filter(anchor_before_year < inventory_year) %>%
  group_by(coctu_id_gnis, ctu_name, ctu_class, county_name, inventory_year) %>%
  slice_max(anchor_before_year, n = 1) %>%
  ungroup() %>%
  left_join(
    known_with_pred %>%
      select(ctu_name, ctu_class, county_name,
             anchor_after_year = inventory_year,
             residual_after    = residual),
    by = c("ctu_name", "ctu_class", "county_name"),
    relationship = "many-to-many"
  ) %>%
  filter(anchor_after_year > inventory_year) %>%
  group_by(coctu_id_gnis, ctu_name, ctu_class, county_name, inventory_year) %>%
  slice_min(anchor_after_year, n = 1) %>%
  ungroup() %>%
  mutate(
    years_from_before = inventory_year - anchor_before_year,
    years_from_after  = anchor_after_year - inventory_year,
    w_before          = 0.5 ^ (years_from_before / half_life),
    w_after           = 0.5 ^ (years_from_after  / half_life),
    w_before_norm     = w_before / (w_before + w_after),
    w_after_norm      = w_after  / (w_before + w_after),
    correction        = residual_before * w_before_norm + residual_after * w_after_norm,
    business_mcf      = rf_predicted + correction,
    data_source       = "Model prediction (bias-decay)"
  ) %>%
  select(coctu_id_gnis, ctu_name, ctu_class, county_name,
         inventory_year, business_mcf, data_source)

trailing_gap <- missing_years %>%
  anti_join(blended_predictions,
            by = c("ctu_name", "ctu_class", "county_name", "inventory_year")
  ) %>%
  left_join(
    known_with_pred %>%
      select(ctu_name, ctu_class, county_name,
             anchor_before_year = inventory_year,
             residual_before    = residual),
    by = c("ctu_name", "ctu_class", "county_name"),
    relationship = "many-to-many"
  ) %>%
  filter(anchor_before_year < inventory_year) %>%
  group_by(coctu_id_gnis, ctu_name, ctu_class, county_name, inventory_year) %>%
  slice_max(anchor_before_year, n = 1) %>%
  ungroup() %>%
  mutate(
    years_from_before = inventory_year - anchor_before_year,
    decay             = 0.5 ^ (years_from_before / half_life),
    business_mcf      = rf_predicted + residual_before * decay,
    data_source       = "Model prediction (bias-decay trailing)"
  ) %>%
  select(coctu_id_gnis, ctu_name, ctu_class, county_name,
         inventory_year, business_mcf, data_source)

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
             residual_after    = residual),
    by = c("ctu_name", "ctu_class", "county_name"),
    relationship = "many-to-many"
  ) %>%
  filter(anchor_after_year > inventory_year) %>%
  group_by(coctu_id_gnis, ctu_name, ctu_class, county_name, inventory_year) %>%
  slice_min(anchor_after_year, n = 1) %>%
  ungroup() %>%
  mutate(
    years_from_after = anchor_after_year - inventory_year,
    decay            = 0.5 ^ (years_from_after / half_life),
    business_mcf     = rf_predicted + residual_after * decay,
    data_source      = "Model prediction (bias-decay leading)"
  ) %>%
  select(coctu_id_gnis, ctu_name, ctu_class, county_name,
         inventory_year, business_mcf, data_source)

no_data_cities <- full_pred_grid %>%
  anti_join(coctu_busi_known,
            by = c("ctu_name", "ctu_class", "county_name")
  ) %>%
  select(coctu_id_gnis, ctu_name, ctu_class, county_name,
         inventory_year, rf_predicted) %>%
  mutate(
    business_mcf = rf_predicted,
    data_source  = "Model prediction (RF only)"
  ) %>%
  select(-rf_predicted)

# ── Diagnostics ───────────────────────────────────────────────────────────────

blending_check <- bind_rows(
  known_with_pred %>%
    select(ctu_name, ctu_class, county_name, inventory_year, business_mcf) %>%
    mutate(data_source = "Utility report"),
  blended_predictions,
  trailing_gap,
  leading_gap
) %>%
  arrange(ctu_name, ctu_class, county_name, inventory_year)


## bring back pre-2010 RII data
known_pre2010 <- coctu_busi_known %>%
  filter(inventory_year < 2010) %>%
  left_join(
    urbansim_busi %>% distinct(ctu_name, ctu_class, county_name, coctu_id_gnis),
    by = c("ctu_name", "ctu_class", "county_name")
  ) %>%
  mutate(data_source = "RII utility data") %>%
  select(coctu_id_gnis, ctu_name, ctu_class, county_name,
         inventory_year, business_mcf, data_source)

# ── Combine and save ──────────────────────────────────────────────────────────

coctu_busi_out <- bind_rows(
  known_pre2010,
  known_with_pred %>%
    select(coctu_id_gnis, ctu_name, ctu_class, county_name,
           inventory_year, business_mcf) %>%
    mutate(data_source = "Utility report"),
  blended_predictions,
  trailing_gap,
  leading_gap,
  no_data_cities
) %>%
  filter(business_mcf > 0) %>%
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
#
#   business floor — sum of business_mcf from utilities that DID report sector
#                    splits; prediction cannot be less than observed partial
#   power plant (pp_mcf) - cities with natural gas powerplant utilities will not 
#                          have those counted here    
# Guardrails are applied only to modeled city-years, never to utility-reported data

ghgrp_pp_deduction <- read_rds("_industrial/data/fuel_combustion_activity.rds") %>%
  filter(
    power_plant        == TRUE,
    general_fuel_type  == "Natural Gas",
    !is.na(value_activity)
  ) %>%
  mutate(
    ctu_name       = str_to_title(str_replace_all(city_name, "ST\\.", "Saint")),
    inventory_year = reporting_year,
    pp_mcf         = value_activity / 1000
  ) %>%
  group_by(ctu_name, inventory_year) %>%
  summarize(pp_mcf = sum(pp_mcf, na.rm = TRUE), .groups = "drop")

## comparing these to city data and researching reveals only Shakopee's plant is likely included in utility reports
## Xcel appears to not report natural gas deliveries to their own powerplants

util_guardrails <- ctu_utility_mcf %>%
  group_by(ctu_name, ctu_class, inventory_year) %>%
  summarize(
    busi_floor  = sum(business_mcf, na.rm = TRUE),
    n_na_total  = sum(is.na(total_mcf)),
    n_na_busi   = sum(is.na(business_mcf)),
    .groups     = "drop"
  ) %>%
  # Only rows where something is missing — complete city-years are already
  # handled correctly by the main pipeline and need no guardrails
  filter(n_na_total > 0 | n_na_busi > 0) %>%
  mutate(
    apply_floor = busi_floor > 0
  ) %>%
  select(-n_na_total, -n_na_busi) %>%
  # Shakopee: subtract power plant gas from busi_floor
  left_join(
    ghgrp_pp_deduction %>% filter(ctu_name == "Shakopee"),
    by = c("ctu_name", "inventory_year")
  ) %>%
  mutate(
    pp_mcf      = replace_na(pp_mcf, 0),
    busi_floor  = pmax(0, busi_floor  - pp_mcf),
    apply_floor = busi_floor > 0
  ) %>%
  select(-pp_mcf)


## for cities split between counties
city_pred_totals <- coctu_busi_out %>%
  group_by(ctu_name, ctu_class, inventory_year) %>%
  summarize(city_pred_total = sum(business_mcf, na.rm = TRUE), .groups = "drop")


coctu_busi_adj <- coctu_busi_out %>%
  left_join(city_pred_totals,
            by = c("ctu_name", "ctu_class", "inventory_year")) %>%
  left_join(
    util_guardrails %>%
      select(ctu_name, ctu_class, inventory_year, busi_floor, apply_floor),
    by = c("ctu_name", "ctu_class", "inventory_year")
  ) %>%
  mutate(
    is_modeled   = !data_source %in% c("Utility report", "RII utility data"),
    county_share = if_else(city_pred_total > 0,
                           business_mcf / city_pred_total,
                           1),
    busi_floor   = busi_floor * county_share,
    floor_hit    = is_modeled        &
      !is.na(apply_floor) &
      apply_floor        &
      business_mcf < busi_floor,
    business_mcf = if_else(floor_hit, busi_floor, business_mcf),
    data_source  = if_else(floor_hit,
                           paste0(data_source, " [partial utility floor]"),
                           data_source)
  ) %>%
  select(-city_pred_total, -county_share, -busi_floor, -apply_floor,
         -is_modeled, -floor_hit)

stopifnot(
  coctu_busi_adj %>%
    count(ctu_name, ctu_class, county_name, inventory_year) %>%
    filter(n > 1) %>%
    nrow() == 0
)


coctu_busi_adj %>%
  filter(ctu_name == "Credit River") %>%
  ggplot(aes(inventory_year, business_mcf, color = data_source)) +
  geom_line() + geom_point() +
  theme_bw() +
  labs(title = "Business MCF -- blending check")


saveRDS(coctu_busi_adj, "_energy/data-raw/predicted_coctu_business_mcf.rds")
