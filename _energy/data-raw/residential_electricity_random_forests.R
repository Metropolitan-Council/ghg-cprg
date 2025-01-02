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
mn_parcel <- readRDS("_meta/data/ctu_parcel_data_2021.RDS") %>% 
  filter(mc_classification == "single_family_home")

#urbansim 

residential <- c("total_households",
                 "total_residential_units",
                 "total_pop",
                 "max_detached",
                 "max_lrglot",
                 "max_attached",
                 "max_multifam",
                 "manufactured_homes")

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
  left_join(mn_parcel %>% select(mean_sq_ft, total_sq_ft, mean_emv, total_emv, mean_year, ctu_id),
            by = c("gnis" = "ctu_id")) %>% 
  filter(!is.na(mWh_delivered), !is.na(gnis),
         !ctu_name %in% c("Shakopee",
                          "Coon Rapids",
                          "Blaine")) # shared utility! 
  
### run model

set.seed(1029)
ind <- sample(2, nrow(electricity), replace = TRUE, prob = c(0.7, 0.3))
train <- electricity[ind==1,]
test <- electricity[ind==2,]


### full model
rf_res_model <- randomForest(mWh_delivered ~ 
                               ctu_class + #get community designation here
                               total_pop + total_households + total_residential_units +
                               mean_year + total_emv + mean_emv +
                               max_attached + max_detached + max_lrglot + max_multifam,
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
rf <- randomForest(mWh_delivered ~ ctu_class + #get community designation here
                     total_pop + total_households + total_residential_units +
                     mean_year + total_emv + mean_emv +
                     max_attached + max_detached + max_lrglot + max_multifam, data=train, proximity=TRUE) 

print(rf)

p1 <- predict(rf, train)
plot(p1, train$mWh_delivered)
abline(0,1)

train %>% cbind(p1) %>% filter(mWh_delivered < 100000 & p1 > 150000)

p2 <- predict(rf, test)
plot(p2, test$mWh_delivered)
abline(0,1)

test %>% cbind(p2) %>% filter(mWh_delivered > 250000)

hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "darkgreen")

varImpPlot(rf,
           sort = T)

importance(rf)

