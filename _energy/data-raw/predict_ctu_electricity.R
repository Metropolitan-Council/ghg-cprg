### Develop Random Forests model for predicting CTU residential electricity usage ###

source("R/_load_pkgs.R")
source("_energy/data-raw/_energy_emissions_factors.R")
cprg_ctu <- read_rds("_meta/data/cprg_ctu.RDS") %>%
  filter(!county_name %in% c("Chisago", "Sherburne", "St. Croix", "Pierce"))


## load Xcel community reports - response to train on
xcel <- read_csv("_energy/data-raw/Xcel_activityData_2015_2023.csv")
utility_id <- read_csv("_energy/data-raw/sql_utility_id.csv")
ctu_utility_id <- read_csv("_energy/data-raw/sql_utility_id_ctu_id.csv")

# predictor data
mn_parcel <- readRDS("_meta/data/ctu_parcel_data_2021.RDS")
urbansim <- readRDS("_meta/data/urban_sim_2020.RDS")

ctu_utility <- ctu_utility_id %>%
  left_join(utility_id, by = c("utility_id" = "mn_doc_utility_id")) %>%
  distinct(utility_id, utility_name, ctu_id)

### reduce xcel list down to cities that don't have second utility service
ctu_multiple <- ctu_utility %>%
  mutate(duplicate = duplicated(ctu_id)) %>%
  filter(duplicate == TRUE) %>%
  distinct(ctu_id)

xcel_only <- xcel %>%
  left_join(
    cprg_ctu %>% select(ctu_name, ctu_class, gnis) %>%
      st_drop_geometry() %>%
      distinct(ctu_name, ctu_class, gnis),
    by = c(
      "ctu_name",
      "ctu_class"
    )
  ) %>%
  filter(!gnis %in% c(ctu_multiple$ctu_id))

xcel_map <- xcel_only %>%
  group_by(ctu_name, ctu_class) %>%
  summarize(mwh = sum(mWh_delivered, na.rm = TRUE)) %>%
  left_join(cprg_ctu %>% select(ctu_name, ctu_class, geometry),
    by = c(
      "ctu_name",
      "ctu_class"
    )
  ) %>%
  st_as_sf()

ggplot(xcel_map) +
  geom_sf(aes(fill = mwh), color = "black", size = 0.2) +
  scale_fill_viridis_c(option = "plasma", name = "mWh_delivered (Xcel)") +
  theme_minimal()

# for this first approach we are only looking at residential electricity delivery
# in 2021

residential_2021 <- xcel_only %>%
  filter(
    year == 2021,
    sector == "Residential"
  )

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
  "total_households",
  "total_residential_units",
  "total_pop",
  "max_detached",
  "max_lrglot",
  "max_attached",
  "max_multifam",
  "manufactured_homes",
  "single_fam_det_sl_own",
  "single_fam_det_ll_own",
  "single_fam_det_rent",
  "single_fam_attached_own",
  "single_fam_attached_rent",
  "multi_fam_own",
  "multi_fam_rent"
)

# operating at ctu not coctu for now

urbansim_res <- urbansim %>%
  filter(Variable %in% residential) %>%
  group_by(Variable, ctu_id) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = ctu_id,
    names_from = Variable,
    values_from = value
  )

# merge into xcel
electricity_res <- left_join(residential_2021,
  urbansim_res,
  by = c("gnis" = "ctu_id")
) %>%
  left_join(mn_parcel_res %>% select(-ctu_name),
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
  mWh_delivered ~
    ctu_class + # get community designation here
    total_pop + total_households + total_residential_units + mean_year_apartment +
    mean_year_multifamily_home + mean_year_single_family_home +
    total_emv_apartment + total_emv_single_family_home + total_emv_multifamily_home +
    # max_attached + max_detached + max_lrglot + max_multifam +
    single_fam_det_sl_own + single_fam_det_ll_own +
    single_fam_det_rent +
    single_fam_attached_own +
    single_fam_attached_rent +
    multi_fam_own +
    multi_fam_rent,
  importance = T, data = electricity_res
)

rf_res_model
p_full <- predict(rf_res_model, electricity_res)
plot(p_full, electricity_res$mWh_delivered)
abline(0, 1)
# struggles with two big cities

# look at top predictors
varImpPlot(rf_res_model,
  sort = T
)


### can subset predict test model?
rf_res_train <- randomForest(
  mWh_delivered ~
    ctu_class + # get community designation here
    total_pop + total_households + total_residential_units + mean_year_apartment +
    mean_year_multifamily_home + mean_year_single_family_home +
    total_emv_apartment + total_emv_single_family_home + total_emv_multifamily_home +
    max_attached + max_detached + max_lrglot + max_multifam +
    single_fam_det_sl_own + single_fam_det_ll_own +
    single_fam_det_rent +
    single_fam_attached_own +
    single_fam_attached_rent +
    multi_fam_own +
    multi_fam_rent,
  data = train_res,
  importance = T
)

print(rf_res_train)

p1 <- predict(rf_res_train, train_res)
plot(p1, train_res$mWh_delivered)
abline(0, 1)

train_res %>%
  cbind(p1) %>%
  filter(mWh_delivered < 10000 & p1 > 20000)

p2 <- predict(rf_res_train, test_res)
plot(p2, test_res$mWh_delivered)
abline(0, 1)

importance(rf_res_train)

### predict out to all CTUs

# urbansim_coctu_prop <- readRDS("_meta/data/urban_sim_2020.RDS") %>%
#   filter(Variable == "total_pop") %>%
#   group_by(ctu_id) %>%
#   mutate(ctu_pop = sum(value),
#          pop_prop = value/ctu_pop)

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

# parcel data

# using single family homes data for simplicity for now
mn_parcel_nonres <- mn_parcel %>%
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

nonresidential <- c(
  "total_job_spaces",
  "max_office",
  "max_commercial",
  "max_institutional",
  "max_school",
  "js_type_1011",
  "js_type_13",
  "js_type_14",
  "zones_total_jobs_20_minutes_tt",
  "zones_total_jobs_45_minutes_tt",
  "total_job_spaces",
  "max_industrial",
  "js_type_12",
  "jobs_sector_4",
  "jobs_sector_5",
  "jobs_sector_6",
  "jobs_sector_7",
  "jobs_sector_8",
  "jobs_sector_9",
  "jobs_sector_10",
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


### linear model predictions

importance(rf_res_model)

res_simple <- lm(mWh_delivered ~ total_pop + mean_year_single_family_home, data = electricity_res)
summary(res_simple) # R2 = 0.9863
plot(mWh_delivered ~ total_pop, data = electricity_res)
abline(res_simple)

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
