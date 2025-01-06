### Develop Random Forests model for predicting CTU residential electricity usage ###

source("R/_load_pkgs.R")
library(randomForest)
library(caret)
cprg_ctu <- read_rds("_meta/data/cprg_ctu.RDS") %>% 
  filter(!county_name %in% c("Chisago","Sherburne", "St. Croix", "Pierce"))


## load Xcel community reports - response to train on
xcel <- read_csv("_energy/data-raw/Xcel_activityData_2015_2023.csv")
utility_id <- read_csv("_energy/data-raw/sql_utility_id.csv")
ctu_utility_id <- read_csv("_energy/data-raw/sql_utility_id_ctu_id.csv")

ctu_utility <- ctu_utility_id %>% 
  left_join(utility_id, by = c("utility_id" = "mn_doc_utility_id")) %>% 
  distinct(utility_id, utility_name, ctu_id)

### reduce xcel list down to cities that don't have second utility service
ctu_multiple <- ctu_utility %>% 
  mutate(duplicate = duplicated(ctu_id)) %>% 
  filter(duplicate == TRUE) %>% 
  distinct(ctu_id)

xcel_only <- xcel %>% 
  left_join(cprg_ctu %>% select(ctu_name, ctu_class, gnis) %>% 
              st_drop_geometry() %>% 
              distinct(ctu_name,ctu_class,gnis),
            by = c("ctu_name",
                   "ctu_class")) %>% 
  filter(!gnis %in% c(ctu_multiple$ctu_id))

# for this first approach we are only looking at residential electricity delivery
# in 2021

residential_2021 <- xcel_only %>% 
  filter(year == 2021,
         sector == "Residential")

## load predictors 

# parcel data

# using single family homes data for simplicity for now
mn_parcel_res <- readRDS("_meta/data/ctu_parcel_data_2021.RDS") %>% 
  filter(mc_classification %in% c("single_family_home",
                                  "multifamily_home",
                                  "apartment")) %>%
  group_by(ctu_name, ctu_id, mc_classification) %>% 
  summarize(total_emv = sum(total_emv), mean_year = mean(mean_year)) %>% 
  pivot_wider(id_cols = c(ctu_name, ctu_id),
              names_from = mc_classification,
              values_from = c(total_emv, mean_year)) %>% 
  na_replace() %>% ungroup()


#urbansim 

residential <- c("total_households",
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
urbansim_res <- readRDS("_meta/data/urban_sim_2020.RDS") %>% 
  filter(Variable %in% residential) %>% 
  group_by(Variable,ctu_id) %>% 
  summarize(value = sum(value)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = ctu_id,
              names_from = Variable,
              values_from = value)

# merge into xcel
electricity <- left_join(residential_2021,
                         urbansim_res, 
                         by = c("gnis" = "ctu_id")) %>% 
  left_join(mn_parcel_res %>% select(-ctu_name),
            by = c("gnis" = "ctu_id")) %>% 
  filter(!is.na(mWh_delivered), !is.na(gnis),
         !ctu_name %in% c("Shakopee",
                          "Coon Rapids",
                          "Blaine",
                          "White Bear Lake")) # shared utility! 
  
### run model

set.seed(1029)
ind <- sample(2, nrow(electricity), replace = TRUE, prob = c(0.7, 0.3))
train <- electricity[ind==1,]
test <- electricity[ind==2,]

### log transform most common variables
electricity <- mutate(electricity,
                      log_total_pop = log(total_pop),
                      log_total_households = log(total_households))

### full model
rf_res_model <- randomForest(mWh_delivered ~ 
                               ctu_class + #get community designation here
                               total_pop + total_households + total_residential_units +mean_year_apartment +
                               mean_year_multifamily_home + mean_year_single_family_home + 
                               total_emv_apartment + total_emv_single_family_home + total_emv_multifamily_home +
                               max_attached + max_detached + max_lrglot + max_multifam +
                               single_fam_det_sl_own + single_fam_det_ll_own +
                             single_fam_det_rent +
                             single_fam_attached_own +
                             single_fam_attached_rent +
                             multi_fam_own +
                             multi_fam_rent,
                             importance = T, data = electricity)

rf_res_model
p_full <- predict(rf_res_model, electricity)
plot(p_full, electricity$mWh_delivered)
abline(0,1)
# struggles with two big cities

# look at top predictors
varImpPlot(rf_res_model,
           sort = T) 


### can subset predict test model?
rf <- randomForest(mWh_delivered ~ 
                     ctu_class + #get community designation here
                     total_pop + total_households + total_residential_units +mean_year_apartment +
                     mean_year_multifamily_home + mean_year_single_family_home + 
                     total_emv_apartment + total_emv_single_family_home + total_emv_multifamily_home +
                     max_attached + max_detached + max_lrglot + max_multifam +
                     single_fam_det_sl_own + single_fam_det_ll_own +
                     single_fam_det_rent +
                     single_fam_attached_own +
                     single_fam_attached_rent +
                     multi_fam_own +
                     multi_fam_rent, data=train,importance = T) 

print(rf)

p1 <- predict(rf, train)
plot(p1, train$mWh_delivered)
abline(0,1)

train %>% cbind(p1) %>% filter(mWh_delivered < 10000 & p1 > 20000)

p2 <- predict(rf, test)
plot(p2, test$mWh_delivered)
abline(0,1)

importance(rf)


### predict out to all CTUs

urbansim_coctu_prop <- readRDS("_meta/data/urban_sim_2020.RDS") %>% 
  filter(Variable == "total_pop") %>% 
  group_by(ctu_id) %>% 
  mutate(ctu_pop = sum(value),
         pop_prop = value/ctu_pop) 

ctu_res_predict <- cprg_ctu %>% 
  left_join(urbansim_res, by = c("gnis" = "ctu_id")) %>% 
  left_join(mn_parcel_res, by = c("gnis" = "ctu_id")) %>% 
  mutate(mwh_predicted = predict(rf_res_model, .))

county_res_predict <- ctu_res_predict %>% 
  filter(!is.na(mwh_predicted)) %>% 
  st_drop_geometry() %>% 
  group_by(county_name) %>%
  summarize(mwh_predicted = sum(mwh_predicted))  %>%
  mutate(
    # apply emission factor and convert to metric tons
    co2 = (mwh_predicted * eGRID_MROW_emissionsFactor_CO2) %>%
      units::as_units("lb") %>%
      units::set_units("ton") %>%
      as.numeric(),
    ch4 = (mwh_predicted * eGRID_MROW_emissionsFactor_CH4) %>%
      units::as_units("lb") %>%
      units::set_units("ton") %>%
      as.numeric(),
    n2o = (mwh_predicted * eGRID_MROW_emissionsFactor_N2O) %>%
      units::as_units("lb") %>%
      units::set_units("ton") %>%
      as.numeric(),
    co2e =
      co2 +
      (ch4 * gwp$n2o) +
      (n2o * gwp$n2o)
  )
  
source("_energy/data-raw/_energy_emissions_factors.R")

nrel_predict_res <- read_rds("_energy/data/electric_natgas_nrel_proportioned.RDS") %>% 
  filter(source == "Electricity",
         category == "Residential",
         year == 2021)

prediction_comparison <- rbind(county_res_predict %>% 
                                 select(county_name, co2e) %>% 
                                 mutate(source = "MC_model"),
                               nrel_predict_res %>% 
                                 select(county_name = county, co2e = emissions_metric_tons_co2e) %>% 
                                 filter(county_name %in% county_res_predict$county_name) %>% 
                                 mutate(source = "NREL"))

ggplot(prediction_comparison, aes(x = county_name, y = co2e, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") + theme_bw()
