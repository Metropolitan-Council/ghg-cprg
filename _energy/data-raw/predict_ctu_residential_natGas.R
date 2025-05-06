### Develop model for predicting CTU residential natural gas usage ###

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

ctu_utility_mcf <- read_rds("_energy/data/ctu_utility_mcf.RDS")

ctu_utility_year <- ctu_utility_mcf %>%
  group_by(ctu_name, ctu_class, inventory_year) %>%
  filter(!any(is.na(total_mcf))) %>%
  summarize(
    residential_mcf = sum(residential_mcf, na.rm = TRUE),
    business_mcf = sum(business_mcf, na.rm = TRUE),
    total_mcf = sum(total_mcf)
  ) %>%
  ungroup()

# split CTU to COCTU based on population for multi-county CTUs
# necessary step for county proportional based analysis
coctu_population <- ctu_population %>%
  distinct(ctu_name, ctu_class, inventory_year, county_name, ctu_population) %>% # Ensure unique rows per city-county-year
  group_by(ctu_name, ctu_class, inventory_year) %>%
  mutate(
    total_ctu_population = sum(ctu_population, na.rm = TRUE), # Sum populations across counties for each city-year
    coctu_population_prop = ctu_population / total_ctu_population,
    multi_county = n_distinct(county_name) > 1
  ) %>%
  ungroup()

#### RESIDENTIAL ####

### split residential mcf based on population splits

coctu_res_year <- ctu_utility_year %>%
  # Join city_total_population back to main dataset
  full_join(coctu_population,
    by = c("ctu_name", "ctu_class", "inventory_year"),
    relationship = "many-to-many"
  ) %>%
  # Calculate proportions and disaggregated values
  group_by(ctu_name, ctu_class, inventory_year, county_name) %>%
  mutate(
    residential_mcf = ifelse(
      multi_county,
      residential_mcf * coctu_population_prop,
      residential_mcf
    )
  ) %>%
  ungroup() %>%
  filter(!is.na(residential_mcf)) %>%
  select(ctu_name, ctu_class, inventory_year, residential_mcf, county_name, ctu_population)

# predictor data
mn_parcel <- readRDS("_meta/data/ctu_parcel_data_2021.RDS")
urbansim <- readRDS("_meta/data/urbansim_data.RDS")


# residential predictors
mn_parcel_res <- mn_parcel %>%
  filter(mc_classification %in% c(
    "single_family_home",
    "multifamily_home",
    "apartment"
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


# urbansim

residential <- c(
  "total_pop",
  "total_households",
  "total_residential_units",
  "manufactured_homes",
  "single_fam_det_sl_own",
  "single_fam_det_ll_own",
  "single_fam_det_rent",
  "single_fam_attached_own",
  "single_fam_attached_rent",
  "multi_fam_own",
  "multi_fam_rent"
)


### create 2010-2025 urbansim residential dataset
urbansim_res <- urbansim %>%
  filter(variable %in% residential) %>%
  group_by(coctu_id, variable) %>%
  complete(inventory_year = full_seq(c(2005, 2025), 1)) %>% # add interstitial years and expand to 2025
  arrange(coctu_id, variable, inventory_year) %>%
  mutate(value = approx(inventory_year, value, inventory_year, method = "linear", rule = 2)$y) %>% # allow extrapolation
  ungroup() %>%
  pivot_wider(
    id_cols = c(coctu_id, inventory_year),
    names_from = variable,
    values_from = value
  ) %>%
  filter(!is.na(coctu_id)) %>%
  mutate(
    ctu_id = str_sub(coctu_id, -7, -1),
    county_id = as.numeric(str_remove(coctu_id, paste0("0", ctu_id))),
    ctu_id = as.numeric(ctu_id)
  ) %>%
  left_join(
    cprg_ctu %>% st_drop_geometry() %>%
      distinct(ctu_name, gnis, thrive_designation),
    by = c("ctu_id" = "gnis")
  ) %>%
  left_join(
    cprg_county %>% st_drop_geometry() %>%
      mutate(geoid = as.numeric(str_sub(geoid, -3, -1))) %>%
      select(county_name, geoid),
    by = c("county_id" = "geoid")
  )


# merge into utility data
ng_res <- left_join(coctu_res_year,
  urbansim_res,
  by = c("ctu_name", "county_name", "inventory_year")
) %>%
  left_join(mn_parcel_res %>% select(-ctu_name),
    by = c("ctu_id" = "ctu_id")
  ) %>%
  # weather data
  left_join(noaa_year, by = "inventory_year")
### run residential model
#### residential RF ####

set.seed(1029)
ind <- sample(2, nrow(ng_res), replace = TRUE, prob = c(0.7, 0.3))
train_res <- ng_res[ind == 1, ]
test_res <- ng_res[ind == 2, ]

### full model
rf_res_model <- randomForest(
  residential_mcf ~
    thrive_designation +
    total_pop + total_households + total_residential_units +
    # mean_year_apartment + mean_year_multifamily_home + mean_year_single_family_home +
    # total_emv_apartment + total_emv_single_family_home + total_emv_multifamily_home +
    single_fam_det_sl_own + single_fam_det_ll_own +
    single_fam_det_rent +
    single_fam_attached_own +
    single_fam_attached_rent +
    multi_fam_own +
    multi_fam_rent +
    heating_degree_days,
  importance = T, data = ng_res,
  na.action = na.omit
)

rf_res_model
p_full <- predict(rf_res_model, ng_res)
plot(p_full, ng_res$residential_mcf)
abline(0, 1)

### save rf_res_model output
saveRDS(rf_res_model, "_energy/data/ctu_residential_ng_random_forest.RDS")

# look at top predictors
varImpPlot(rf_res_model,
  sort = T
)


### can subset predict test model?
rf_res_train <- randomForest(
  residential_mcf ~
    thrive_designation +
    total_pop + total_households + total_residential_units + mean_year_apartment +
    mean_year_multifamily_home + mean_year_single_family_home +
    total_emv_apartment + total_emv_single_family_home + total_emv_multifamily_home +
    single_fam_det_sl_own + single_fam_det_ll_own +
    single_fam_det_rent +
    single_fam_attached_own +
    single_fam_attached_rent +
    multi_fam_own +
    multi_fam_rent,
  data = train_res,
  importance = T,
  na.action = na.omit
)

print(rf_res_train)

p1 <- predict(rf_res_train, train_res)
plot(p1, train_res$residential_mcf)
abline(0, 1)


p2 <- predict(rf_res_train, test_res)
plot(p2, test_res$residential_mcf)
abline(0, 1)

importance(rf_res_train)


### linear model predictions

importance(rf_res_model)

res_simple <- lm(
  residential_mcf ~ thrive_designation + total_pop + single_fam_det_ll_own + total_households +
    total_residential_units * mean_year_single_family_home,
  data = ng_res
)
summary(res_simple) # R2 = 0.9928

### compare prediction to input data
lm_pred <- predict(res_simple, ng_res)
plot(lm_pred, ng_res$residential_mcf)
abline(0, 1)


### predict ALL cities and rollback up to counties for all years

ctu_res_predict <- cprg_ctu %>%
  left_join(urbansim_res, by = c(
    "gnis" = "ctu_id",
    "ctu_name",
    "county_name",
    "thrive_designation"
  )) %>%
  left_join(noaa_year) %>%
  mutate(mcf_predicted = predict(rf_res_model, .)) %>%
  filter(!is.na(mcf_predicted)) %>% # removes 2025 data
  st_drop_geometry() %>%
  group_by(ctu_name, ctu_class, inventory_year) %>%
  summarize(residential_mcf_predicted = sum(mcf_predicted)) %>%
  ungroup()



ctu_res_predict %>% distinct(ctu_name, ctu_class)

ctu_res <- left_join(
  ctu_res_predict,
  ctu_utility_year %>%
    select(1:4)
)

## add predicted mcf

county_res_predict <- cprg_ctu %>%
  left_join(urbansim_res, by = c(
    "gnis" = "ctu_id",
    "ctu_name",
    "county_name",
    "thrive_designation"
  )) %>%
  left_join(noaa_year) %>%
  mutate(mcf_predicted = predict(rf_res_model, .)) %>%
  filter(!is.na(mcf_predicted)) %>% # removes 2025 data
  st_drop_geometry() %>%
  group_by(county_name, inventory_year) %>%
  summarize(residential_mcf_predicted = sum(mcf_predicted)) %>%
  ungroup()

# save intermediate rds
saveRDS(ctu_res, "_energy/data-raw/predicted_ctu_residential_mcf.rds")
saveRDS(county_res_predict, "_energy/data-raw/predicted_county_residential_mcf.rds")
