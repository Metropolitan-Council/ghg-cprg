### Develop model for predicting CTU residential electricity usage ###
# This script should be rerun after all updates to ctu_utility_mwh.RDS 
# from script _energy/data-raw/compile_ctu_electricity_records.R

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
  #remove known errant ctu-utility assignments
  filter(!(ctu_name == "Andover" &
             utility == "Anoka Municipal Utility"),
         !(ctu_name == "Cottage Grove" &
             utility == "Dakota Electric Association"),
         !(ctu_name == "Anoka" &
             utility == "Xcel Energy")) %>% 
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
    mean_year_apartment + mean_year_multifamily_home + mean_year_single_family_home +
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

### zoom in on smaller cities
plot(p_full, electricity_res$residential_mwh,
     xlim = c(0,50000),
     ylim = c(0,50000))
abline(0, 1)

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

### zoom in on smaller cities
plot(p2, test_res$residential_mwh,
     xlim = c(0,50000),
     ylim = c(0,50000))
abline(0, 1)

importance(rf_res_train, 1)


### predict 2020, 2021, 2022 data for unknown cities


coctu_res_predict_rf <- cprg_ctu %>%
  left_join(urbansim_res, by = c(
    "gnis" = "ctu_id",
    "ctu_name",
    "ctu_class",
    "county_name",
    "thrive_designation"
  )) %>%
  filter(!coctu_id %in% electricity_res$coctu_id,
         inventory_year %in% c(2020:2022))%>%
  left_join(mn_parcel_res %>% select(-ctu_name),
            by = c("gnis" = "ctu_id")
  ) %>%
  # weather data
  left_join(noaa_year, by = "inventory_year") %>% 
  mutate(mwh_predicted = predict(rf_res_model, .)) %>%
  filter(!is.na(mwh_predicted)) %>% 
  st_drop_geometry() 

ctu_res_predict_rf <- coctu_res_predict_rf %>%
  group_by(ctu_name, ctu_class, inventory_year) %>%
  summarize(residential_mwh_predicted_rf = sum(mwh_predicted)) %>%
  ungroup() %>% 
  mutate(data_source = "Model prediction")

ctu_res_out <- bind_rows(ctu_utility_year %>% 
                           select(ctu_name,
                                  ctu_class,
                                  inventory_year,
                                  residential_mwh) %>% 
                           mutate(data_source = "Utility report"),
                         ctu_res_predict_rf %>% 
                           rename(residential_mwh = residential_mwh_predicted_rf)
                         )

## county

coctu_res_year
county_res_predict <-  coctu_res_predict_rf %>%
  group_by(county_name, inventory_year) %>%
  summarize(residential_mwh_predicted_rf = sum(mwh_predicted)) %>%
  ungroup() %>% 
  mutate(data_source = "Model prediction")

county_res_out <- bind_rows(coctu_res_year %>% 
                           select(county_name,
                                  inventory_year,
                                  residential_mwh) %>% 
                           mutate(data_source = "Utility report"),
                           county_res_predict %>% 
                           rename(residential_mwh = residential_mwh_predicted_rf)
) %>% 
  filter(inventory_year %in% c(2020:2022)) %>% 
  group_by(county_name, inventory_year, data_source) %>%
  summarize(residential_mwh = sum(residential_mwh)) %>%
  ungroup()

# save intermediate rds
saveRDS(ctu_res_out, "_energy/data-raw/predicted_ctu_residential_mwh.rds")
saveRDS(county_res_out, "_energy/data-raw/predicted_county_residential_mwh.rds")
