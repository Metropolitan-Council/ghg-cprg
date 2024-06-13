source("R/_load_pkgs.R")
source("R/global_warming_potential.R")

library(tidyUSDA)

cprg_county <- readRDS("_meta/data/cprg_county.RDS")

enteric <- read_csv('_agriculture/data-raw/enteric-fermentation.csv')
manure_n2o <- read_csv('_agriculture/data-raw/manure-n2o.csv')
manure_ch4 <- read_csv('_agriculture/data-raw/manure-ch4.csv')
mn_feedlots <- read_csv('_agriculture/data-raw/mn-feedlots.csv') %>% 
  filter(county_name %in% cprg_county$NAME)

key <- keyring::key_get('usda_key')

counties <- toupper(cprg_county$NAME)

### this is an API to get livestock data (mammals) from the USDA.
### USDA has year survey data that provides heads of animals
### USDA also has 5 year census data with more detailed information (2007, 2012, 2017, 2022). This should be examined for validation later
usda_livestock <- tidyUSDA::getQuickstat(
  sector="ANIMALS & PRODUCTS",
  group="LIVESTOCK",
  commodity=NULL,
  category= "INVENTORY",
  domain= NULL,
  county= counties,
  key = key,
  program = "SURVEY",
  data_item = NULL,
  geographic_level = 'COUNTY',
  year = as.character(2005:2022),
  state = c("MINNESOTA","WISCONSIN"),
  geometry = TRUE,
  lower48 = TRUE, 
  weighted_by_area = T)


usda_livestock_use <- usda_livestock %>% 
  filter(!is.na(Value), unit_desc == "HEAD")  # exclude missing values, take only head count surveys now. May be useful to validate by operations census later.

usda_livestock_use %>% 
  filter(commodity_desc != "CATTLE") %>% 
  distinct(year)
## there is no non-cattle survey data past 2013. Will need to use inventory data to in-fill other livestock

unique(usda_livestock_use$short_desc)

### other live stock have head estimates from 2005:2013, and 2017

### operations censuses are benchmarks at 2007,2012,2017, 2022

### format enteric fermentation emissions factors

enteric_formatted <- enteric  %>% 
  select(c(2,3,6)) %>% 
  setNames(c("Year","Livestock","Emission_factor_kg_ch4_per_head")) %>% 
  filter(!(is.na(Year) | is.na(Emission_factor_kg_ch4_per_head))) %>% 
  filter(Year >= 2005) %>% 
  mutate(Emission_factor_kg_ch4_per_head = as.numeric(Emission_factor_kg_ch4_per_head))

unique(enteric_formatted$Livestock)

### USDA data has heads of dairy cattle, beef cattle, and 'cattle including calves' - My interpretation is last category double counts previous
### EPA enteric fermentation has many categories, and multiple estimates for 'replacements' (calves). Taking the average of calves as they are similar

enteric_agg <- enteric_formatted %>% 
  filter(!grepl("mos.",Livestock)) %>% 
  mutate(livestock_type = if_else(grepl("Replacement",Livestock), "Calves",
                                  if_else(grepl("Feedlot",Livestock),"Feedlot cattle",
                                          Livestock))) %>% 
  group_by(Year, livestock_type) %>% 
  summarize(kg_ch4_per_head = mean(Emission_factor_kg_ch4_per_head ))

unique(enteric_agg$livestock_type)
unique(usda_livestock_use$short_desc)

usda_livestock_agg <- usda_livestock_use %>%
  mutate(livestock_type = if_else(grepl("MILK", short_desc), "Dairy Cows", 
                                  if_else(grepl("BEEF", short_desc), "Beef Cows",
                                          if_else(grepl("FEED", short_desc), "Feedlot cattle",
                                                  if_else(grepl("CALVES",short_desc), "Calves",
                                                  if_else(grepl("HOGS", short_desc),"Swine",
                                                          if_else(grepl("SHEEP",short_desc),"Sheep",
                                                                  short_desc))))))) %>% 
  group_by(year, county_name) %>% 
  summarise(head_count = sum(Value))

### match livestock labels to enteric

#is TOTAL overlapping with non-TOTAL fields?
usda_livestock_use %>% filter(domain_desc == "TOTAL", grepl("CATTLE", short_desc)) %>% 
  group_by(year) %>% 
  summarize(cattle_count = sum(Value)) %>% print(n=50)

usda_livestock_use %>% filter(domain_desc != "TOTAL", grepl("CATTLE", short_desc)) %>% 
  group_by(year) %>% 
  summarize(cattle_count = sum(Value))

## non-TOTAL fields are from 5 year inventories, omitting inventory for now. TOTAL fields roughly triple in inventory years, digging down further


usda_livestock_use


usda_livestock_enteric <- usda_livestock_use %>% 
  

### make the feedlot data long form
mn_feedlots_long <- mn_feedlots %>% 
  mutate(start_year = year(as.Date(start_d_reg)),
         end_year = year(as.Date(end_d_reg))) %>%
  filter(!is.na(start_year)) %>% 
  rowwise() %>%
  mutate(years = list(seq(start_year, end_year))) %>%
  unnest(years) %>% 
  pivot_longer(cols = c(
    "cattle_dl", "heifer_d", "calf_d", "cattle_db", "steer_b", "heifer_b", "cow_calf_b", "calf_b", 
    "bull_mature", "veal_calf", "swine_big", "swine_medium", "swine_little", "horse", "sheep", 
    "chx_lm", "chx_b_big", "chx_b_little", "chx_l_big", "chx_l_little", "turkey_big", "turkey_little", "duck", "duck_lm", 
    "alpacas", "bison", "bison_calf", "camels", "deer", "donkey_mule", "elk", "emus_ostriches", "peacocks", "fowl", 
    "foxes", "geese", "goats", "goats_small", "ponies", "llamas", "mink", "rabbits", "reindeer_caribou", "unknown"
  ),
  names_to = "animal",
  values_to = "count",
  values_drop_na = TRUE
  ) %>%
  filter(count >0) %>% 
  group_by(county_name, years, animal) %>% 
  summarize(county_count = sum(count)) 


### format n20 emissions factors

manure_n2o_formatted <- manure_n2o  %>% 
  select(c(2,3,6)) %>% 
  setNames(c("Year","Livestock","Emission_factor_kg_ch4_per_head")) %>% 
  filter(!(is.na(Year) | is.na(Emission_factor_kg_ch4_per_head))) %>% 
  filter(Year >= 2005) %>% 
  mutate(Emission_factor_kg_ch4_per_head = as.numeric(Emission_factor_kg_ch4_per_head))


usda_poultry <- tidyUSDA::getQuickstat(
  sector="ANIMALS & PRODUCTS",
  group="POULTRY",
  commodity=NULL,
  category= "INVENTORY",
  domain= NULL,
  county= counties,
  key = key,
  program = NULL,
  data_item = NULL,
  geographic_level = 'COUNTY',
  year = as.character(2005:2021),
  state = c("MINNESOTA","WISCONSIN"),
  geometry = TRUE,
  lower48 = TRUE, 
  weighted_by_area = T)


  

