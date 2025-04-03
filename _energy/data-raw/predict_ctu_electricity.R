### Develop model for predicting CTU residential electricity usage ###

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

#bring in weather data
#weather data
noaa <- readRDS("_meta/data/noaa_weather_2015-2021.rds")

noaa_year <- noaa %>% 
  group_by(inventory_year) %>% 
  summarize(heating_degree_days = sum(heating_degree_days),
            cooling_degree_days = sum(cooling_degree_days),
            temperature = mean(dry_bulb_temp))

ctu_utility_year <- read_rds("_energy/data/ctu_utility_mwh.RDS") %>% 
  group_by(ctu_name, ctu_class, inventory_year) %>%
  filter(!any(is.na(total_mwh))) %>%
  summarize(
    residential_mwh = sum(residential_mwh, na.rm = TRUE),
    business_mwh = sum(business_mwh, na.rm = TRUE),
    total_mwh = sum(total_mwh)) %>% 
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
      residential_mwh  * coctu_population_prop,
      residential_mwh )
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


### create 2010-2025 urbansim residential dataset
urbansim_res <- urbansim %>%
  filter(variable %in% residential) %>%
  group_by(coctu_id, variable) %>% 
  complete(inventory_year = full_seq(c(min(inventory_year), 2025), 1)) %>% # add interstitial years and expand to 2025
  arrange(coctu_id, variable, inventory_year) %>% 
  mutate(value = approx(inventory_year, value, inventory_year, method = "linear", rule = 2)$y) %>% # allow extrapolation
  ungroup() %>% 
  pivot_wider(
    id_cols = c(coctu_id, inventory_year),
    names_from = variable,
    values_from = value
  ) %>%
  filter(!is.na(coctu_id)) %>% 
  mutate(ctu_id = str_sub(coctu_id, -7, -1),
         county_id = as.numeric(str_remove(coctu_id, paste0("0", ctu_id))),
         ctu_id = as.numeric(ctu_id)
  ) %>% 
  left_join(cprg_ctu %>% st_drop_geometry() %>% 
              distinct(ctu_name, gnis),
            by = c("ctu_id" = "gnis")) %>% 
  left_join(cprg_county %>% st_drop_geometry() %>% 
              mutate(geoid = as.numeric(str_sub(geoid, -3, -1))) %>% 
              select(county_name, geoid),
            by = c("county_id" = "geoid"))


# merge into utility data
electricity_res <- left_join(coctu_res_year,
  urbansim_res,
  by = c("ctu_name", "county_name", "inventory_year")
) %>%
  left_join(mn_parcel_res %>% select(-ctu_name),
    by = c("ctu_id" = "ctu_id")
  ) %>% 
  #weather data
  left_join(noaa_year, by = "inventory_year") %>% 
  ### filter to 2020 and earlier for now. See if we can predict back to 2021+ later
  filter(inventory_year <= 2020)
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
  residential_mwh  ~
    ctu_class + # get community designation here
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
  importance = T, data = electricity_res,
  na.action = na.omit
)

rf_res_model
p_full <- predict(rf_res_model, electricity_res)
plot(p_full, electricity_res$residential_mwh)
abline(0, 1)
# struggles with two big cities

# look at top predictors
varImpPlot(rf_res_model,
  sort = T
)


### can subset predict test model?
rf_res_train <- randomForest(
  residential_mwh ~
    ctu_class + # get community designation here
    total_pop + total_households + total_residential_units + mean_year_apartment +
    mean_year_multifamily_home + mean_year_single_family_home +
    total_emv_apartment + total_emv_single_family_home + total_emv_multifamily_home +
    single_fam_det_sl_own + single_fam_det_ll_own +
    single_fam_det_rent +
    single_fam_attached_own +
    single_fam_attached_rent +
    multi_fam_own +
    multi_fam_rent +
    cooling_degree_days ,
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

importance(rf_res_train)


### linear model predictions

importance(rf_res_model)

res_simple <- lm(residential_mwh ~ total_pop + multi_fam_own + total_households +
                   total_residential_units *  mean_year_single_family_home ,
                 data = electricity_res)
summary(res_simple) # R2 = 0.9889 

### compare prediction to input data
lm_pred <- predict(res_simple, electricity_res)
plot(lm_pred, electricity_res$residential_mwh)
abline(0, 1)

### predict out to all CTUs 2021 and on

elec_res_2021 <- left_join(coctu_res_year,
                 urbansim_res,
                  by = c("ctu_name", "county_name", "inventory_year")
) %>%
  left_join(mn_parcel_res %>% select(-ctu_name),
            by = c("ctu_id" = "ctu_id")
  ) %>% 
  left_join(noaa_year, by = "inventory_year") %>% 
  ### filter to 2020 and earlier for now. See if we can predict back to 2021+ later
  filter(inventory_year >= 2021)

lm_pred_2021 <- predict(res_simple, elec_res_2021)
plot(lm_pred_2021, elec_res_2021$residential_mwh)
abline(0, 1)

coctu_res_year %>% filter(ctu_name == "Minneapolis")

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
  group_by(coctu_id, variable) %>% 
  complete(inventory_year = full_seq(c(min(inventory_year), 2025), 1)) %>% # add interstitial years and expand to 2025
  arrange(coctu_id, variable, inventory_year) %>% 
  mutate(value = approx(inventory_year, value, inventory_year, method = "linear", rule = 2)$y) %>% # allow extrapolation
  ungroup() %>% 
  pivot_wider(
    id_cols = c(coctu_id, inventory_year),
    names_from = variable,
    values_from = value
  ) %>%
  filter(!is.na(coctu_id)) %>% 
  mutate(ctu_id = str_sub(coctu_id, -7, -1),
         county_id = as.numeric(str_remove(coctu_id, paste0("0", ctu_id))),
         ctu_id = as.numeric(ctu_id)
  ) %>% 
  left_join(cprg_ctu %>% st_drop_geometry() %>% 
              distinct(ctu_name, gnis),
            by = c("ctu_id" = "gnis")) %>% 
  left_join(cprg_county %>% st_drop_geometry() %>% 
              mutate(geoid = as.numeric(str_sub(geoid, -3, -1))) %>% 
              select(county_name, geoid),
            by = c("county_id" = "geoid"))

#create jobs proportions for coctu
coctu_jobs <- urbansim_busi %>%
  distinct(ctu_name, ctu_id, inventory_year, county_name, total_job_spaces) %>% # Ensure unique rows per city-county-year
  group_by(ctu_name, ctu_id, inventory_year) %>%
  mutate(
    ctu_class = if_else(nchar(ctu_id) == 7, "CITY", "TOWNSHIP"),
    total_ctu_jobs = sum(total_job_spaces, na.rm = TRUE), # Sum populations across counties for each city-year
    coctu_jobs_prop = total_job_spaces / total_ctu_jobs,
    multi_county = n_distinct(county_name) > 1
  ) %>%
  ungroup()

## split out business mwh based on job proportions for cities in two counties
coctu_busi_year <- ctu_utility_year %>%
  # Join city_total_population back to main dataset
  full_join(coctu_jobs,
            by = c("ctu_name", "ctu_class", "inventory_year"),
            relationship = "many-to-many"
  ) %>%
  # Calculate proportions and disaggregated values
  group_by(ctu_name, ctu_class, inventory_year, county_name) %>%
  mutate(
    business_mwh = ifelse(
      multi_county,
      business_mwh  * coctu_jobs_prop,
      business_mwh)
  ) %>%
  ungroup() %>% 
  filter(!is.na(business_mwh)) %>% 
  select(ctu_name, ctu_class, inventory_year, business_mwh, county_name)

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

# merge into utility data
electricity_busi <- left_join(coctu_busi_year,
                             urbansim_busi,
                             by = c("ctu_name", "county_name", "inventory_year")
) %>%
  left_join(mn_parcel_busi %>% select(-ctu_name),
            by = c("ctu_id" = "ctu_id")
  ) %>% 
  #weather data
  left_join(noaa_year, by = "inventory_year") %>% 
  ### filter to 2020 and earlier for now. See if we can predict back to 2021+ later
  filter(inventory_year <= 2020)



### linear model predictions

busi_simple <- lm(business_mwh ~ total_job_spaces + total_emv_commercial + total_emv_industrial +
                    total_emv_public_building +
                   cooling_degree_days,
                 data = electricity_busi)
summary(busi_simple) # R2 = 0.9772

### compare prediction to input data
lm_pred_busi <- predict(busi_simple, electricity_busi)
plot(lm_pred_busi, electricity_busi$business_mwh)
abline(0, 1)

### predict out to all CTUs 2021 and on

elec_busi_2021 <- left_join(coctu_busi_year,
                            urbansim_busi,
                            by = c("ctu_name", "county_name", "inventory_year")
) %>%
  left_join(mn_parcel_busi %>% select(-ctu_name),
            by = c("ctu_id" = "ctu_id")
  ) %>% 
  #weather data
  left_join(noaa_year, by = "inventory_year") %>% 
  ### filter to 2020 and earlier for now. See if we can predict back to 2021+ later
  filter(inventory_year == 2021)

lm_pred_busi_2021 <- predict(busi_simple, elec_busi_2021)
plot(lm_pred_busi_2021, elec_busi_2021$business_mwh)
abline(0, 1)



#### deprecated ####

ctu_res_predict <- cprg_ctu %>%
  left_join(urbansim_res, by = c("gnis" = "ctu_id")) %>%
  left_join(mn_parcel_res, by = c("gnis" = "ctu_id")) %>%
  mutate(mwh_predicted = predict(rf_res_model, .)) %>%
  filter(!is.na(mwh_predicted))

county_res_predict <- ctu_res_predict %>%
  filter(!is.na(mwh_predicted)) %>%
  st_drop_geometry() %>%
  group_by(county_name) %>%
  summarize(mwh_predicted = sum(mwh_predicted)) %>%
  mutate(
    # apply emission factor and convert to metric tons
    co2 = (mwh_predicted * eGRID_MROW_emissionsFactor_CO2_2021) %>%
      units::as_units("lb") %>%
      units::set_units("ton") %>%
      as.numeric(),
    ch4 = (mwh_predicted * eGRID_MROW_emissionsFactor_CH4_2021) %>%
      units::as_units("lb") %>%
      units::set_units("ton") %>%
      as.numeric(),
    n2o = (mwh_predicted * eGRID_MROW_emissionsFactor_N2O_2021) %>%
      units::as_units("lb") %>%
      units::set_units("ton") %>%
      as.numeric(),
    co2e =
      co2 +
        (ch4 * gwp$n2o) +
        (n2o * gwp$n2o)
  )



nrel_predict_res <- read_rds("_energy/data/electric_natgas_nrel_proportioned.RDS") %>%
  filter(
    source == "Electricity",
    category == "Residential",
    year == 2021
  )

prediction_comparison_res <- rbind(
  county_res_predict %>%
    select(county_name, co2e) %>%
    mutate(source = "MC_model"),
  nrel_predict_res %>%
    select(county_name = county, co2e = emissions_metric_tons_co2e) %>%
    filter(county_name %in% county_res_predict$county_name) %>%
    mutate(source = "NREL")
)

ggplot(prediction_comparison_res, aes(x = county_name, y = co2e, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw()

ggplot(ctu_res_predict, aes(x = total_pop, y = mwh_predicted, col = county_name)) +
  geom_point() +
  theme_bw()

ggplot(electricity_res, aes(x = total_pop, y = mWh_delivered, col = total_households)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

### does a linear fit perform better?




# nonresidential processing ####

nonresidential_2021 <- xcel_only %>%
  filter(
    year == 2021,
    sector != "Residential"
  ) %>%
  group_by(ctu_name, gnis) %>%
  summarize(mWh_delivered = sum(mWh_delivered))

## load predictors



# urbansim


commercial <- c(
  "total_job_spaces",
  "max_office",
  "max_commercial",
  "max_institutional",
  "max_school",
  "js_type_1011",
  "js_type_13",
  "js_type_14",
  "jobs_sector_4",
  "jobs_sector_5",
  "jobs_sector_6",
  "jobs_sector_7",
  "jobs_sector_8",
  "jobs_sector_9",
  "jobs_sector_10"
)

industrial <- c(
  "total_job_spaces",
  "max_industrial",
  "js_type_12",
  "jobs_sector_1",
  "jobs_sector_2",
  "jobs_sector_3"
)


# operating at ctu not coctu for now
urbansim_nonres <- urbansim %>%
  filter(Variable %in% nonresidential) %>%
  group_by(Variable, ctu_id) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = ctu_id,
    names_from = Variable,
    values_from = value
  )

# merge into xcel
electricity_nonres <- left_join(nonresidential_2021,
  urbansim_nonres,
  by = c("gnis" = "ctu_id")
) %>%
  left_join(mn_parcel_nonres %>% select(-ctu_name),
    by = c("gnis" = "ctu_id")
  ) %>%
  filter(
    !is.na(mWh_delivered), !is.na(gnis),
    !ctu_name %in% c(
      "Shakopee",
      "Coon Rapids",
      "Blaine",
      "White Bear Lake"
    )
  ) # shared utility!

### run model

#### non residential RF ####

set.seed(1029)
ind <- sample(2, nrow(electricity_nonres), replace = TRUE, prob = c(0.7, 0.3))
train_nonres <- electricity_nonres[ind == 1, ]
test_nonres <- electricity_nonres[ind == 2, ]


### full model
rf_nonres_model <- randomForest(
  mWh_delivered ~
    total_job_spaces +
    max_office +
    max_commercial +
    max_institutional +
    max_school +
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
    jobs_sector_3,
  importance = T, data = electricity_nonres
)

rf_nonres_model
p_full <- predict(rf_nonres_model, electricity_nonres)
plot(p_full, electricity_nonres$mWh_delivered)
abline(0, 1)
# struggles with two big cities

# look at top predictors
varImpPlot(rf_nonres_model,
  sort = T
)


### can subset predict test model?
rf_nonres <- randomForest(
  mWh_delivered ~
    total_job_spaces +
    max_office +
    max_commercial +
    max_institutional +
    max_school +
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
    jobs_sector_3,
  data = train_nonres, importance = T
)

print(rf_nonres)

p1 <- predict(rf_nonres, train_nonres)
plot(p1, train_nonres$mWh_delivered)
abline(0, 1)

# train %>% cbind(p1) %>% filter(mWh_delivered < 10000 & p1 > 20000)

p2 <- predict(rf, test_nonres)
plot(p2, test_nonres$mWh_delivered)
abline(0, 1)

importance(rf)


### predict out to all CTUs

# urbansim_coctu_prop <- readRDS( "_meta/data/urban_sim_2020.RDS" ) %>%
#   filter(Variable ==  "total_pop" ) %>%
#   group_by(ctu_id) %>%
#   mutate(ctu_pop = sum(value),
#          pop_prop = value/ctu_pop)

ctu_nonres_predict <- cprg_ctu %>%
  left_join(urbansim_nonres, by = c("gnis" = "ctu_id")) %>%
  left_join(mn_parcel_nonres, by = c("gnis" = "ctu_id")) %>%
  mutate(mwh_predicted = predict(rf_nonres_model, .)) %>%
  filter(!is.na(mwh_predicted))

county_nonres_predict <- ctu_nonres_predict %>%
  filter(!is.na(mwh_predicted)) %>%
  st_drop_geometry() %>%
  group_by(county_name) %>%
  summarize(mwh_predicted = sum(mwh_predicted)) %>%
  mutate(
    # apply emission factor and convert to metric tons
    co2 = (mwh_predicted * eGRID_MROW_emissionsFactor_CO2_2021) %>%
      units::as_units("lb") %>%
      units::set_units("ton") %>%
      as.numeric(),
    ch4 = (mwh_predicted * eGRID_MROW_emissionsFactor_CH4_2021) %>%
      units::as_units("lb") %>%
      units::set_units("ton") %>%
      as.numeric(),
    n2o = (mwh_predicted * eGRID_MROW_emissionsFactor_N2O_2021) %>%
      units::as_units("lb") %>%
      units::set_units("ton") %>%
      as.numeric(),
    co2e =
      co2 +
        (ch4 * gwp$n2o) +
        (n2o * gwp$n2o)
  )

nrel_predict_nonres <- read_rds("_energy/data/electric_natgas_nrel_proportioned.RDS") %>%
  filter(
    source == "Electricity",
    category %in% c("Commercial", "Industrial"),
    year == 2021
  ) %>%
  group_by(county) %>%
  summarize(emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e))

prediction_comparison_nonres <- rbind(
  county_nonres_predict %>%
    select(county_name, co2e) %>%
    mutate(source = "MC_model"),
  nrel_predict_nonres %>%
    select(county_name = county, co2e = emissions_metric_tons_co2e) %>%
    filter(county_name %in% county_nonres_predict$county_name) %>%
    mutate(source = "NREL")
)

ggplot(prediction_comparison_nonres, aes(x = county_name, y = co2e, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw()

### look at total mwh

mwh_county <- read_rds("_energy/data/minnesota_elecUtils_ActivityAndEmissions.RDS")

mwh_xcel_county <- mwh_county %>%
  filter(!is.na(mWh_delivered), utility == "XcelEnergy") %>%
  mutate(source = "Xcel County Report") %>%
  select(county_name = county, mwh = mWh_delivered, source) %>%
  filter(!county_name %in% c("Sherburne", "Chisago"))

xcel_ctu_county <- xcel %>%
  left_join(cprg_ctu, by = c("ctu_class", "ctu_name")) %>%
  filter(year == 2021, !is.na(county_name)) %>%
  group_by(county_name) %>%
  summarize(mwh = sum(mWh_delivered, na.rm = TRUE)) %>%
  mutate(source = "Xcel CTU Report") %>%
  select(county_name, mwh, source)

ggplot(
  rbind(mwh_xcel_county, xcel_ctu_county),
  aes(x = county_name, y = mwh, fill = source)
) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7)

mn_mwh <- read_rds("_energy/data/minnesota_elecUtils_ActivityAndEmissions.RDS") %>%
  group_by(county) %>%
  summarize(mwh = sum(mWh_delivered, na.rm = TRUE)) %>%
  mutate(
    sector = "Total",
    source = "Utility Report"
  ) %>%
  filter(!county %in% c("Chisago", "Sherburne", "St. Croix", "Pierce"))


### bring in NREL splits
nrel <- read_rds("_energy/data-raw/nrel_slope/nrel_slope_proportions.RDS")

mn_mwh_sector <- rbind(
  left_join(mn_mwh, nrel %>%
    filter(
      year == 2021,
      source == "Electricity"
    ),
  by = "county"
  ) %>%
    mutate(
      mwh = mwh * residential,
      sector = "Residential"
    ) %>%
    select(county, mwh, sector, source = source.x),
  left_join(mn_mwh, nrel %>%
    filter(
      year == 2021,
      source == "Electricity"
    ),
  by = "county"
  ) %>%
    mutate(
      mwh = mwh * (commercial + industrial),
      sector = "Commercial/Industrial"
    ) %>%
    select(county, mwh, sector, source = source.x)
)

ctu_mwh_predict <- rbind(
  ctu_res_predict %>% st_drop_geometry() %>%
    select(
      ctu_name = ctu_name.x,
      mwh_predicted,
      county_name
    ) %>%
    mutate(sector = "Residential"),
  ctu_nonres_predict %>% st_drop_geometry() %>%
    select(
      ctu_name = ctu_name.x,
      mwh_predicted,
      county_name
    ) %>%
    mutate(sector = "Commercial/Industrial")
) %>%
  mutate(source = "MC Model - RF")


mwh <- rbind(mn_mwh_sector, ctu_mwh_predict %>%
  group_by(county_name, sector, source) %>%
  summarize(mwh = sum(mwh_predicted)) %>%
  rename(county = county_name))

ggplot(mwh, aes(x = source, y = mwh, fill = sector)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.7) +
  facet_wrap(~county, scales = "free_y") + # Side-by-side sources
  # scale_fill_manual(values = c("Total" = "blue", "Commercial/Industrial" = "orange")) + # Customize colors
  labs(
    title = "Electricity Usage by County",
    x = "County",
    y = "MWh",
    fill = "Sector"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )



varImpPlot(rf_nonres_model)

nonres_simple <- lm(mWh_delivered ~ total_job_spaces, data = electricity_nonres)
summary(nonres_simple) # R2 = 0.9704

plot(mWh_delivered ~ total_job_spaces, data = electricity_nonres)
abline(nonres_simple)

ctu_predict_linear <- rbind(
  cprg_ctu %>%
    left_join(urbansim_res, by = c("gnis" = "ctu_id")) %>%
    left_join(mn_parcel_res, by = c("gnis" = "ctu_id", "ctu_name")) %>%
    mutate(mwh_predicted = predict(res_simple, .)) %>%
    mutate(sector = "Residential") %>%
    select(ctu_name, county_name, mwh_predicted, sector),
  cprg_ctu %>%
    left_join(urbansim_nonres, by = c("gnis" = "ctu_id")) %>%
    # left_join(mn_parcel_res, by = c("gnis" = "ctu_id")) %>%
    mutate(mwh_predicted = predict(nonres_simple, .)) %>%
    mutate(sector = "Commercial/Industrial") %>%
    select(ctu_name, county_name, mwh_predicted, sector)
)


county_predict_linear <- ctu_predict_linear %>%
  filter(!is.na(mwh_predicted)) %>%
  st_drop_geometry() %>%
  group_by(county_name, sector) %>%
  summarize(mwh_predicted = sum(mwh_predicted)) %>%
  mutate(
    # apply emission factor and convert to metric tons
    co2 = (mwh_predicted * eGRID_MROW_emissionsFactor_CO2_2021) %>%
      units::as_units("lb") %>%
      units::set_units("ton") %>%
      as.numeric(),
    ch4 = (mwh_predicted * eGRID_MROW_emissionsFactor_CH4_2021) %>%
      units::as_units("lb") %>%
      units::set_units("ton") %>%
      as.numeric(),
    n2o = (mwh_predicted * eGRID_MROW_emissionsFactor_N2O_2021) %>%
      units::as_units("lb") %>%
      units::set_units("ton") %>%
      as.numeric(),
    co2e =
      co2 +
        (ch4 * gwp$n2o) +
        (n2o * gwp$n2o)
  )

prediction_comparison_linear <- rbind(
  county_predict_linear %>%
    select(county_name, mwh = mwh_predicted, sector) %>%
    mutate(source = "MC model linear"),
  mn_mwh_sector %>%
    select(county_name = county, mwh, sector) %>%
    filter(county_name %in% county_res_predict$county_name) %>%
    mutate(source = "NREL")
)

mwh_add <- rbind(mwh, county_predict_linear %>%
  select(county = county_name, mwh = mwh_predicted, sector) %>%
  mutate(source = "MC model linear"))

mwh_predict_total <- ggplot(mwh_add, aes(x = source, y = mwh, fill = sector)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.7) +
  facet_wrap(~county, scales = "free_y") + # Side-by-side sources
  scale_fill_manual(
    values = c("Residential" = "cornflowerblue", "Commercial/Industrial" = "cornflowerblue"),
    guide = "none"
  ) + # Customize colors
  labs(
    title = "Electricity Usage by County - Total",
    x = "County",
    y = "MWh",
    fill = "Sector"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )

mwh_predict_total

mwh_predict_res <- ggplot(
  mwh_add %>% filter(sector == "Residential"),
  aes(x = source, y = mwh)
) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.7, fill = "firebrick4", col = "black") +
  facet_wrap(~county, scales = "free_y") + # Side-by-side sources
  labs(
    title = "Electricity Usage by County - Residential",
    x = "County",
    y = "MWh",
    fill = "Sector"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )

mwh_predict_res

mwh_predict_nonres <- ggplot(
  mwh_add %>% filter(sector != "Residential"),
  aes(x = source, y = mwh)
) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.7, fill = "#FFDB58", col = "black") +
  facet_wrap(~county, scales = "free_y") + # Side-by-side sources
  labs(
    title = "Electricity Usage by County - Non-Residential",
    x = "County",
    y = "MWh",
    fill = "Sector"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )

mwh_predict_nonres
