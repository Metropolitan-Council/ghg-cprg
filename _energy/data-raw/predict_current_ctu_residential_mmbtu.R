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
    thrive_designation == "Rural Center",#insufficient data
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

# get complete city-years utility data

ctu_utility_year_raw <- read_rds("_energy/data/ctu_utility_mcf.RDS") %>% 
  #removing known false NAs
  filter(# these utilties do not provide gas to MN cities despite GIS analysis
    !utility %in% c("ST. CROIX VALLEY NATURAL GAS",
                        "WISCONSIN GAS CO"),
         # Keep ALL rows for known non-responders — their NA total_mcf rows
         # are a real signal that blocks their cities from entering training
         utility %in% c("Minnesota Energy Resources",
                        "GREATER MINNESOTA GAS INC.",
                        "Centennial Utilities") |
  !is.na(total_mcf) | !is.na(residential_mcf) | !is.na(business_mcf))

ctu_utility_year <- ctu_utility_year_raw %>%
  # Impute business = 0 where a utility confirms all its gas is residential
  # (residential ≈ total, nothing left over for business)
  mutate(
    business_mcf = if_else(
      !is.na(residential_mcf) & is.na(business_mcf) &
        !is.na(total_mcf) & total_mcf > 0 &
        abs(residential_mcf - total_mcf) / total_mcf < 0.01,
      0,
      business_mcf
    )
  ) %>%
  group_by(ctu_name, ctu_class, inventory_year) %>%
  filter(!any(is.na(total_mcf))) %>%
  summarize(
    residential_mcf = sum(residential_mcf, na.rm = TRUE),
    business_mcf    = sum(business_mcf,    na.rm = TRUE),
    total_mcf       = sum(total_mcf),
    .groups         = "drop"
  ) %>%
  filter(
    total_mcf > 0,
    (residential_mcf + business_mcf) / total_mcf >= 0.9 # drops edge cases where residential is an unlikely percentage of total, example: Afton has sliver of Xcel in some years which would lead to bad models
  )

# for converting natural gas mcf to mmbtu
mcf_to_mmbtu <- 1.037

# bring in liquid fuels
propane_fueloil <- readRDS("_energy/data-raw/ctu_propane_fueloil_use.RDS") %>%
  rename(inventory_year = acs_year)

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
            by      = c("ctu_name", "ctu_class", "inventory_year"),
            relationship = "many-to-many"
  ) %>%
  mutate(
    residential_mcf = if_else(multi_county,
                              residential_mcf * coctu_population_prop,
                              residential_mcf)
  ) %>%
  filter(!is.na(residential_mcf), residential_mcf > 0) %>%
  select(ctu_name, ctu_class, inventory_year, residential_mcf,
         county_name, ctu_population) %>%
  # convert NG to mmBtu and add propane/fuel oil to get total combustion
  left_join(
    propane_fueloil %>% select(ctu_name, ctu_class, county_name,
                               inventory_year, propane_mmBtu, fueloil_other_mmBtu),
    by = c("ctu_name", "ctu_class", "county_name", "inventory_year")
  ) %>%
  mutate(
    ng_mmbtu             = residential_mcf * mcf_to_mmbtu,
    propane_mmBtu        = replace_na(propane_mmBtu,       0),
    fueloil_other_mmBtu  = replace_na(fueloil_other_mmBtu, 0),
    total_res_mmbtu      = ng_mmbtu + propane_mmBtu + fueloil_other_mmBtu
  )

#predictor data 

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

# training dataset 

ng_res_train <- coctu_res_known %>%
  left_join(urbansim_res, by = c("ctu_name", "ctu_class", "county_name", "inventory_year")) %>%
  left_join(mn_parcel_res %>% select(-ctu_name), by = "ctu_id") %>%
  left_join(noaa_year, by = "inventory_year") %>%
  filter(!is.na(coctu_id_gnis), total_res_mmbtu != 0)

# train RF on all complete city-years 

set.seed(1029)

rf_res_model <- randomForest(
  total_res_mmbtu ~
    thrive_designation +
    total_pop + total_households + total_residential_units +
    single_fam_det_sl_own + single_fam_det_ll_own +
    single_fam_det_rent + single_fam_attached_own +
    single_fam_attached_rent + multi_fam_own + multi_fam_rent +
    heating_degree_days,
  data       = ng_res_train,
  importance = TRUE,
  na.action  = na.omit
)

print(rf_res_model)
varImpPlot(rf_res_model, sort = TRUE)

# ── Predict ALL city-years 2010-2023 ─────────────────────────────────────────
# Including known years so we can compute anchor residuals

full_pred_grid <- cprg_ctu %>%
  st_drop_geometry() %>%
  left_join(
    urbansim_res,
    by = c("gnis" = "ctu_id", "ctu_name", "ctu_class",
           "county_name", "thrive_designation")
  ) %>%
  filter(inventory_year %in% 2010:2023) %>%
  left_join(mn_parcel_res %>% select(-ctu_name), by = c("gnis" = "ctu_id")) %>%
  left_join(noaa_year, by = "inventory_year") %>%
  filter(!is.na(coctu_id_gnis)) %>%
  mutate(rf_predicted = predict(rf_res_model, .))

# ── Mean RF scale correction ──────────────────────────────────────────────────
# For cities with utility-reported anchor years, compute the mean ratio of
# actual to RF-predicted. Apply this scalar to all model-predicted years so
# the full series reflects the city's systematic deviation from the RF baseline.
# Cities with no anchor years (no utility data at all) get raw RF predictions.

known_with_pred <- coctu_res_known %>%
  left_join(
    full_pred_grid %>%
      select(coctu_id_gnis, ctu_name, ctu_class, county_name,
             inventory_year, rf_predicted),
    by = c("ctu_name", "ctu_class", "county_name", "inventory_year")
  ) %>%
  filter(!is.na(rf_predicted))

city_rf_scale <- known_with_pred %>%
  mutate(scale = total_res_mmbtu / rf_predicted) %>%
  group_by(ctu_name, ctu_class, county_name) %>%
  summarize(mean_scale = mean(scale, na.rm = TRUE), .groups = "drop")

missing_years_out <- full_pred_grid %>%
  anti_join(coctu_res_known,
            by = c("ctu_name", "ctu_class", "county_name", "inventory_year")) %>%
  select(coctu_id_gnis, ctu_name, ctu_class, county_name,
         inventory_year, rf_predicted) %>%
  left_join(city_rf_scale, by = c("ctu_name", "ctu_class", "county_name")) %>%
  mutate(
    total_res_mmbtu = if_else(!is.na(mean_scale),
                              rf_predicted * mean_scale,
                              rf_predicted),
    data_source     = if_else(!is.na(mean_scale),
                              "Model prediction (RF scaled)",
                              "Model prediction (RF only)")
  ) %>%
  select(coctu_id_gnis, ctu_name, ctu_class, county_name,
         inventory_year, total_res_mmbtu, data_source)


## bring back pre-2010 RII data
known_pre2010 <- coctu_res_known %>%
  filter(inventory_year < 2010) %>%
  left_join(
    urbansim_res %>% distinct(ctu_name, ctu_class, county_name, coctu_id_gnis),
    by = c("ctu_name", "ctu_class", "county_name")
  ) %>%
  mutate(data_source = "RII utility data") %>%
  select(coctu_id_gnis, ctu_name, ctu_class, county_name,
         inventory_year, total_res_mmbtu , data_source)

# add into the final bind_rows alongside the three gap types
coctu_res_out <- bind_rows(
  known_pre2010,
  known_with_pred %>%
    select(coctu_id_gnis, ctu_name, ctu_class, county_name,
           inventory_year, total_res_mmbtu) %>%
    mutate(data_source = "Utility report"),
  missing_years_out
) %>%
  filter(total_res_mmbtu > 0) %>%
  arrange(ctu_name, ctu_class, county_name, inventory_year) %>%
  left_join(
    propane_fueloil %>% select(ctu_name, ctu_class, county_name,
                               inventory_year, propane_mmBtu, fueloil_other_mmBtu),
    by = c("ctu_name", "ctu_class", "county_name", "inventory_year")
  ) %>%
  mutate(
    propane_mmBtu       = replace_na(propane_mmBtu,       0),
    fueloil_other_mmBtu = replace_na(fueloil_other_mmBtu, 0),
    ng_mmbtu            = pmax(0, total_res_mmbtu - propane_mmBtu - fueloil_other_mmBtu)
  )

stopifnot(
  coctu_res_out %>%
    count(ctu_name, ctu_class, county_name, inventory_year) %>%
    filter(n > 1) %>%
    nrow() == 0
)


# ── Partial utility guardrails ────────────────────────────────────────────────
# Cities excluded from RF training due to missing utility responses may still
# have partial observations that bound reasonable predictions.

res_guardrails <- read_rds("_energy/data/ctu_utility_mcf.RDS") %>%
  group_by(ctu_name, ctu_class, inventory_year) %>%
  summarize(
    res_floor_mcf = sum(residential_mcf, na.rm = TRUE),
    full_total    = sum(total_mcf),          # NA when any utility missing
    n_na_total    = sum(is.na(total_mcf)),
    n_na_res      = sum(is.na(residential_mcf)),
    .groups       = "drop"
  ) %>%
  filter(n_na_total > 0 | n_na_res > 0) %>%
  mutate(
    res_floor   = res_floor_mcf * mcf_to_mmbtu,
    apply_floor = res_floor > 0,
    apply_ceil  = n_na_total == 0 & !is.na(full_total) & full_total > 0
  ) %>%
  select(-res_floor_mcf, -n_na_total, -n_na_res)


# ── Apply residential floor ───────────────────────────────────────────────────

coctu_res_adj <- coctu_res_out %>%
  left_join(
    coctu_population %>%
      distinct(ctu_name, ctu_class, county_name, inventory_year,
               coctu_population_prop, multi_county),
    by = c("ctu_name", "ctu_class", "county_name", "inventory_year")
  ) %>%
  left_join(
    res_guardrails %>%
      select(ctu_name, ctu_class, inventory_year, res_floor, apply_floor),
    by = c("ctu_name", "ctu_class", "inventory_year")
  ) %>%
  mutate(
    # Proportion floor for multi-county CTUs using population share
    res_floor_prop  = res_floor * if_else(
      !is.na(multi_county) & multi_county,
      coctu_population_prop,
      1
    ),
    # Add propane and fuel oil so floor is on the same basis as total_res_mmbtu
    res_floor_total = res_floor_prop + propane_mmBtu + fueloil_other_mmBtu,
    is_modeled      = !data_source %in% c("Utility report", "RII utility data"),
    floor_hit       = is_modeled        &
      !is.na(apply_floor) &
      apply_floor         &
      total_res_mmbtu < res_floor_total,
    total_res_mmbtu = if_else(floor_hit, res_floor_total, total_res_mmbtu),
    # Keep ng_mmbtu consistent: recalculate after any floor adjustment
    ng_mmbtu        = pmax(0, total_res_mmbtu - propane_mmBtu - fueloil_other_mmBtu),
    data_source     = if_else(
      floor_hit,
      paste0(data_source, " [partial utility floor]"),
      data_source
    )
  ) %>%
  select(-coctu_population_prop, -multi_county, -res_floor, -apply_floor,
         -res_floor_prop, -res_floor_total, -is_modeled, -floor_hit)

# ── Propagate floor corrections to remaining RF-only years ────────────────────
# For cities where some years were floor-adjusted, compute the mean ratio of
# floor-adjusted total to RF-predicted total across those anchor years.
# Apply that scaling factor uniformly to all remaining RF-only years so the
# series is consistent rather than jumping between corrected and uncorrected.

floor_scale <- coctu_res_adj %>%
  filter(grepl("partial utility floor", data_source)) %>%
  left_join(
    full_pred_grid %>%
      select(coctu_id_gnis, ctu_name, ctu_class, county_name,
             inventory_year, rf_predicted),
    by = c("coctu_id_gnis", "ctu_name", "ctu_class", "county_name", "inventory_year")
  ) %>%
  mutate(scale = total_res_mmbtu / rf_predicted) %>%
  group_by(ctu_name, ctu_class, county_name) %>%
  summarize(mean_scale = mean(scale, na.rm = TRUE), .groups = "drop")

coctu_res_adj_out <- coctu_res_adj %>%
  left_join(floor_scale, by = c("ctu_name", "ctu_class", "county_name")) %>%
  mutate(
    apply_scale     = data_source == "Model prediction (RF only)" & !is.na(mean_scale),
    total_res_mmbtu = if_else(apply_scale, total_res_mmbtu * mean_scale, total_res_mmbtu),
    ng_mmbtu        = if_else(apply_scale,
                              pmax(0, total_res_mmbtu - propane_mmBtu - fueloil_other_mmBtu),
                              ng_mmbtu),
    data_source     = if_else(apply_scale,
                              "Model prediction (RF only) [floor correction propagated]",
                              data_source)
  ) %>%
  select(-mean_scale, -apply_scale)



# ── Final integrity check and save ────────────────────────────────────────────

stopifnot(
  coctu_res_out %>%
    count(ctu_name, ctu_class, county_name, inventory_year) %>%
    filter(n > 1) %>%
    nrow() == 0
)


# check - plot a city of interest
coctu_res_adj_out %>%
  filter(ctu_name == "East Bethel") %>%
  ggplot(aes(inventory_year, total_res_mmbtu , color = data_source)) +
  geom_line() + geom_point() +
  theme_bw() +
  labs(title = "Residential mmbtu -- blending check")


saveRDS(coctu_res_adj_out, "_energy/data-raw/predicted_coctu_residential_mmbtu.rds")
