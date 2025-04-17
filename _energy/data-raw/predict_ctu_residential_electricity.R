### Develop model for predicting CTU residential electricity usage ###

source("R/_load_pkgs.R")
source("_energy/data-raw/_energy_emissions_factors.R")

## load in supporting data
cprg_ctu <- read_rds("_meta/data/cprg_ctu.RDS") %>%
  filter(!county_name %in% c("Chisago", "Sherburne", "St. Croix", "Pierce"),
         !thrive_designation == "Non-Council Area")%>% 
  mutate(thrive_designation = as.factor(if_else(
    thrive_designation == "Rural Center",
    "Rural Residential", # renaming rural center as not enough cities have utility data for modeling
    thrive_designation
  )))
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

ctu_utility_mwh <- read_rds("_energy/data/ctu_utility_mwh.RDS")

# reduce to ctu-years with complete utility reporting
ctu_utility_year <- ctu_utility_mwh %>%
  group_by(ctu_name, ctu_class, inventory_year) %>%
  filter(!any(is.na(total_mwh))) %>%
  summarize(
    residential_mwh = sum(residential_mwh, na.rm = TRUE),
    business_mwh = sum(business_mwh, na.rm = TRUE),
    total_mwh = sum(total_mwh)
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

### split residential mwh based on population splits

coctu_res_year <- ctu_utility_year %>%
  # Join city_total_population back to main dataset
  full_join(coctu_population,
    by = c("ctu_name", "ctu_class", "inventory_year"),
    relationship = "many-to-many"
  ) %>%
  # Calculate proportions and disaggregated values
  group_by(ctu_name, ctu_class, inventory_year, county_name) %>%
  mutate(
    residential_mwh = ifelse(
      multi_county,
      residential_mwh * coctu_population_prop,
      residential_mwh
    )
  ) %>%
  ungroup() %>%
  filter(!is.na(residential_mwh)) %>%
  select(ctu_name, ctu_class, inventory_year, residential_mwh, county_name, ctu_population)

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

### create 2005-2050 urbansim residential dataset
urbansim_res <- urbansim %>%
  filter(variable %in% residential) %>%
  group_by(coctu_id, variable) %>%
  complete(inventory_year = full_seq(c(2005, 2050), 1)) %>%
  arrange(coctu_id, variable, inventory_year) %>%
  mutate(
    value = na_interpolation(value, option = "linear")
  ) %>%
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
      distinct(ctu_name, ctu_class, gnis, thrive_designation),
    by = c("ctu_id" = "gnis")
  ) %>%
  left_join(
    cprg_county %>% st_drop_geometry() %>%
      mutate(geoid = as.numeric(str_sub(geoid, -3, -1))) %>%
      select(county_name, geoid),
    by = c("county_id" = "geoid")
  )


# merge into utility data
electricity_res <- left_join(coctu_res_year,
  urbansim_res,
  by = c("ctu_name", "ctu_class", "county_name", "inventory_year")
) %>%
  left_join(mn_parcel_res %>% select(-ctu_name),
    by = c("ctu_id" = "ctu_id")
  ) %>%
  # weather data
  left_join(noaa_year, by = "inventory_year") %>% 
  filter(!is.na(coctu_id)) 
  
### run residential model
#### residential RF ####

set.seed(1029)
ind <- sample(2, nrow(electricity_res), replace = TRUE, prob = c(0.7, 0.3))
train_res <- electricity_res[ind == 1, ]
test_res <- electricity_res[ind == 2, ]
#
# ### log transform most common variables
# electricity <- mutate(electricity,
#                       log_total_pop = log(total_pop),
#                       log_total_households = log(total_households))

### full model
rf_res_model <- randomForest(
  residential_mwh ~
    thrive_designation +
    total_pop + total_households + total_residential_units +
    # mean_year_apartment + mean_year_multifamily_home + mean_year_single_family_home +
    # total_emv_apartment + total_emv_single_family_home + total_emv_multifamily_home +
    single_fam_det_sl_own + single_fam_det_ll_own +
    single_fam_det_rent +
    single_fam_attached_own +
    single_fam_attached_rent +
    multi_fam_own +
    multi_fam_rent,
  importance = T, data = electricity_res,
  na.action = na.omit
)

rf_res_model
p_full <- predict(rf_res_model, electricity_res)
plot(p_full, electricity_res$residential_mwh)
abline(0, 1)

electricity_res %>% mutate(p_full = p_full) %>% 
  filter(residential_mwh > 60000 & p_full < 60000) %>% 
  select(ctu_name, ctu_class, inventory_year, residential_mwh, p_full)

### save rf_res_model output
#saveRDS(rf_res_model, "_energy/data/ctu_residential_elec_random_forest.RDS")

# look at top predictors
varImpPlot(rf_res_model,
  sort = T
)


### can subset predict test model?
rf_res_train <- randomForest(
  residential_mwh ~
    thrive_designation +
    total_pop + total_households + total_residential_units + mean_year_apartment +
    mean_year_multifamily_home + mean_year_single_family_home +
    total_emv_apartment + total_emv_single_family_home + total_emv_multifamily_home +
    single_fam_det_sl_own + single_fam_det_ll_own +
    single_fam_det_rent +
    single_fam_attached_own +
    single_fam_attached_rent +
    multi_fam_own +
    multi_fam_rent +
    cooling_degree_days,
  data = train_res,
  importance = T,
  na.action = na.omit
)

print(rf_res_train)

p1 <- predict(rf_res_train, train_res)
plot(p1, train_res$residential_mwh)
abline(0, 1)


p2 <- predict(rf_res_train, test_res)
plot(p2, test_res$residential_mwh)
abline(0, 1)

importance(rf_res_train, 1)

### GAM Approach #####
# uses variables ID'ed in the rf_res_model as most important

library(mgcv)
gam_model <- gam(residential_mwh ~ s(total_pop) + s(total_households) + s(total_residential_units) + s(single_fam_det_ll_own) +
                   s(single_fam_attached_rent) + thrive_designation + inventory_year,
                 data = electricity_res, method = "REML", select = TRUE)
summary(gam_model) ## adj r2 = 0.995
plot(gam_model, pages = 1, rug = TRUE, residuals = TRUE)
gam.check(gam_model)

electricity_res$pred <- predict(gam_model)

ggplot(electricity_res, aes(x = residential_mwh, y = pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Actual MWh", y = "Predicted MWh", title = "GAM: Actual vs Predicted")

# gam_model_interaction <- gam(residential_mwh ~ s(total_pop, by = thrive_designation) + 
#                                s(total_households, by = thrive_designation) + 
#                                s(total_residential_units, by = thrive_designation) + 
#                                s(inventory_year, by = thrive_designation),
#                                          data = electricity_res, method = "REML", select = TRUE)
# summary(gam_model_interaction) ## adj r2 = 0.995
# plot(gam_model_interaction, pages = 1, rug = TRUE, residuals = TRUE)
# gam.check(gam_model_interaction)
# 
# electricity_res$pred_gam_int <- predict(gam_model_interaction)
# 
# ggplot(electricity_res, aes(x = residential_mwh, y = pred_gam_int)) +
#   geom_point(alpha = 0.5) +
#   geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
#   labs(x = "Actual MWh", y = "Predicted MWh", title = "GAM: Actual vs Predicted")


### linear model predictions ####

importance(rf_res_model)

res_simple <- lm(
  residential_mwh ~ thrive_designation * total_pop + 
    thrive_designation * single_fam_det_ll_own + 
    thrive_designation * total_households +
    thrive_designation * total_residential_units,
  data = electricity_res
)
summary(res_simple) # R2 = 0.9942

### compare prediction to input data
lm_pred <- predict(res_simple, electricity_res)
plot(lm_pred, electricity_res$residential_mwh)
abline(0, 1)


### test predictions ####

#### Random Forest
ctu_res_predict_rf <- cprg_ctu %>%
  left_join(urbansim_res, by = c(
    "gnis" = "ctu_id",
    "ctu_name",
    "ctu_class",
    "county_name",
    "thrive_designation"
  )) %>%
  mutate(mwh_predicted = predict(rf_res_model, .)) %>%
  filter(!is.na(mwh_predicted)) %>% 
  st_drop_geometry() %>%
  group_by(ctu_name, ctu_class, inventory_year) %>%
  summarize(residential_mwh_predicted_rf = sum(mwh_predicted)) %>%
  ungroup()


ctu_res_predict_gam <- cprg_ctu %>%
  left_join(urbansim_res, by = c(
    "gnis" = "ctu_id",
    "ctu_name",
    "ctu_class",
    "county_name",
    "thrive_designation"
  )) %>%
  mutate(mwh_predicted = predict(gam_model, .)) %>%
  filter(!is.na(mwh_predicted)) %>%
  st_drop_geometry() %>%
  group_by(ctu_name, ctu_class, inventory_year) %>%
  summarize(residential_mwh_predicted_gam = sum(mwh_predicted)) %>%
  ungroup()

ctu_res_predict_lm <- cprg_ctu %>%
  left_join(urbansim_res, by = c(
    "gnis" = "ctu_id",
    "ctu_name",
    "ctu_class",
    "county_name",
    "thrive_designation"
  )) %>%
  mutate(mwh_predicted = predict(res_simple, .)) %>%
  filter(!is.na(mwh_predicted)) %>%
  st_drop_geometry() %>%
  group_by(ctu_name, ctu_class, inventory_year) %>%
  summarize(residential_mwh_predicted_lm = sum(mwh_predicted)) %>%
  ungroup()

plot(residential_mwh_predicted_rf ~ residential_mwh_predicted_gam,
  data = left_join(ctu_res_predict_rf,
               ctu_res_predict_gam))
plot(residential_mwh_predicted_lm ~ residential_mwh_predicted_gam,
     data = left_join(ctu_res_predict_lm,
                      ctu_res_predict_gam))

plot(residential_mwh_predicted_lm ~inventory_year,
     data = ctu_res_predict_lm %>% filter(ctu_name == "Minneapolis"))

ctu_res_predict %>% distinct(ctu_name, ctu_class)

ctu_res_gam <- left_join(
  ctu_res_predict_gam,
  ctu_utility_year %>%
    select(1:4)
)

ctu_res_lm <- left_join(
  ctu_res_predict_lm,
  ctu_utility_year %>%
    select(1:4)
)

#plot random grab of some cities
sample_ctus <- ctu_res_gam %>%
  filter(!is.na(residential_mwh)) %>% 
  distinct(ctu_name, ctu_class) %>%
  slice_sample(n = 10) %>%
  pull(ctu_name)

mpls_ctu_id <- ctu_res_gam %>% 
  filter(ctu_name == "Minneapolis") %>% 
  pull(ctu_id)

sample_ctus <- unique(c(sample_ctus, mpls_ctu_id[[1]]))

plot_data_gam <- ctu_res_gam %>%
  filter(ctu_name %in% sample_ctus,
         ctu_class == "CITY")
plot_data_lm <- ctu_res_lm %>%
  filter(ctu_name %in% sample_ctus,
         ctu_class == "CITY")

ggplot(plot_data_gam, aes(x = inventory_year)) +
  geom_line(aes(y = residential_mwh_predicted_gam, color = ctu_name), linewidth = 0.8) +
  geom_point(
    data = filter(plot_data_gam, !is.na(residential_mwh)),
    aes(y = residential_mwh, color = ctu_name),
    shape = 1, size = 2, stroke = 1
  ) +
  facet_wrap(~ ctu_name, scales = "free_y") +
  labs(
    x = "Year",
    y = "Residential MWh",
    title = "Predicted and Observed Residential Electricity Use",
    subtitle = "GAM predictions (lines) and observed values (circles)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(plot_data_lm, aes(x = inventory_year)) +
  geom_line(aes(y = residential_mwh_predicted_lm, color = ctu_name), linewidth = 0.8) +
  geom_point(
    data = filter(plot_data_lm, !is.na(residential_mwh)),
    aes(y = residential_mwh, color = ctu_name),
    shape = 1, size = 2, stroke = 1
  ) +
  facet_wrap(~ ctu_name, scales = "free_y") +
  labs(
    x = "Year",
    y = "Residential MWh",
    title = "Predicted and Observed Residential Electricity Use",
    subtitle = "LM predictions (lines) and observed values (circles)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


#### basic linear model and anchor ####
### basic approach to determine average coefficients of several housing types,
### anchor cities to their latest mwh annual delivery,
### and back/forecast based on coefficients and estimated changes in housing stock

# fit basic model to known cities

electricity_res <- electricity_res %>% 
  mutate(mfh = multi_fam_own + multi_fam_rent,
         sfh_ll = single_fam_det_ll_own,
         sfh_sl = single_fam_det_rent + single_fam_det_sl_own + manufactured_homes,
         sf_att = single_fam_attached_own + single_fam_attached_rent)

unit_model <- lm(
  residential_mwh ~ mfh +
    sfh_ll +
    sfh_sl +
    sf_att,
  data = electricity_res
)
summary(unit_model)

# extract coefficients
unit_coefs <- data.frame(term = names(unit_model$coefficients),
                         estimate = unit_model$coefficients) %>%
  select(term, estimate) %>%
  filter(term != "(Intercept)") 

# use latest year of data for each city (that has data) as 'intercept'

latest_data <- electricity_res %>%
  group_by(coctu_id) %>% 
  filter(inventory_year == max(inventory_year, na.rm = TRUE))

base_mwh <- latest_data %>%
  select(coctu_id, inventory_year, base_year_mwh = residential_mwh)

# get base values per coctu_id
base_urbansim <- latest_data %>%
  select(coctu_id, base_year = inventory_year, base_mwh = residential_mwh) %>%
  inner_join(urbansim_res %>% 
               mutate(mfh = multi_fam_own + multi_fam_rent,
                      sfh_ll = single_fam_det_ll_own,
                      sfh_sl = single_fam_det_rent + single_fam_det_sl_own + manufactured_homes,
                      sf_att = single_fam_attached_own + single_fam_attached_rent), 
             by = "coctu_id") %>%
  filter(inventory_year == base_year)

# calculate urbansim deltas from base year to each other year
delta_units <- urbansim_res %>% 
  mutate(mfh = multi_fam_own + multi_fam_rent,
         sfh_ll = single_fam_det_ll_own,
         sfh_sl = single_fam_det_rent + single_fam_det_sl_own + manufactured_homes,
         sf_att = single_fam_attached_own + single_fam_attached_rent) %>%
  select(ctu_name, ctu_id, county_name,coctu_id, inventory_year,
         mfh, sfh_ll, sfh_sl, sf_att) %>%
  pivot_longer(cols = -c(ctu_name, ctu_id, county_name,coctu_id, inventory_year), names_to = "unit_type", values_to = "unit_count") %>%
  inner_join(
    base_urbansim %>%
      pivot_longer(cols = mfh:sf_att,
                   names_to = "unit_type", values_to = "base_unit_count") %>%
      select(coctu_id, unit_type, base_unit_count),
    by = c("coctu_id", "unit_type")
  ) %>%
  mutate(unit_delta = unit_count - base_unit_count)


  # apply coefficients to delta and sum
delta_mwh <- delta_units %>% 
  left_join(unit_coefs, by = c("unit_type" = "term")) %>%
  mutate(delta_mwh = unit_delta * estimate) %>%
  group_by(ctu_name, county_name, coctu_id, inventory_year) %>%
  summarise(delta_total_mwh = sum(delta_mwh, na.rm = TRUE), .groups = "drop") %>%
  # add delta to base year MWh
  left_join(base_urbansim %>% select(coctu_id, ctu_id, base_mwh), by = "coctu_id") %>%
  mutate(projected_mwh = base_mwh + delta_total_mwh)

## graph as check

ctu_res_delta <- left_join(
  left_join(cprg_ctu %>% 
              st_drop_geometry() %>% 
              select(ctu_id = gnis, ctu_class),
            delta_mwh,
            by = "ctu_id"),
  ctu_utility_year %>%
    select(1:4)
)

#plot random grab of some cities
sample_ctus <- ctu_res_delta %>%
  filter(!is.na(projected_mwh)) %>% 
  distinct(ctu_name, ctu_id) %>%
  slice_sample(n = 10) %>%
  pull(ctu_id)

mpls_ctu_id <- ctu_res_delta %>% 
  filter(ctu_name == "Minneapolis") %>% 
  pull(ctu_id)

sample_ctus <- unique(c(sample_ctus, mpls_ctu_id[[1]]))

plot_data_delta <- ctu_res_delta %>%
  filter(ctu_id %in% sample_ctus)

ggplot(plot_data_delta, aes(x = inventory_year)) +
  geom_line(aes(y = projected_mwh, color = ctu_name), linewidth = 0.8) +
  geom_point(
    data = filter(plot_data_delta, !is.na(residential_mwh)),
    aes(y = residential_mwh, color = ctu_name),
    shape = 1, size = 2, stroke = 1
  ) +
  facet_wrap(~ ctu_name, scales = "free_y") +
  labs(
    x = "Year",
    y = "Residential MWh",
    title = "Predicted and Observed Residential Electricity Use",
    subtitle = "Housing coefficient predictions (lines) and observed values (circles)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

delta_units %>% filter(ctu_name == "Rosemount", inventory_year %in% c(2010, 2020, 2030, 2040, 2050))
delta_units %>% filter(ctu_name == "Minneapolis", inventory_year %in% c(2010, 2020, 2030, 2040, 2050))
delta_units %>% filter(ctu_name == "New Trier", inventory_year %in% c(2010, 2020, 2030, 2040, 2050))



## add predicted mwh

county_res_predict <- cprg_ctu %>%
  left_join(urbansim_res, by = c(
    "gnis" = "ctu_id",
    "ctu_name",
    "county_name",
    "thrive_designation"
  )) %>%
  left_join(noaa_year) %>%
  mutate(mwh_predicted = predict(rf_res_model, .)) %>%
  filter(!is.na(mwh_predicted)) %>% # removes 2025 data
  st_drop_geometry() %>%
  group_by(county_name, inventory_year) %>%
  summarize(residential_mwh_predicted = sum(mwh_predicted)) %>%
  ungroup()

# save intermediate rds
saveRDS(ctu_res, "_energy/data-raw/predicted_ctu_residential_mwh.rds")
saveRDS(county_res_predict, "_energy/data-raw/predicted_county_residential_mwh.rds")
