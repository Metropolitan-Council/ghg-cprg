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
counties <- if_else(counties == 'ST. CROIX', "ST CROIX", counties)


#### Survey data ####
### this is an API to get livestock data (mammals) from the USDA.
### USDA has yearly survey data that provides heads of animals - only cattle appear to be available after 2013.
### USDA also has 5 year census data with more detailed information (2007, 2012, 2017, 2022). Census data will be needed for hogs, sheep, and feedlots
### Cattle survey and census data should be compared in 2017 and 2022
usda_survey <- tidyUSDA::getQuickstat(
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
  year = as.character(2005:2021),
  state = c("MINNESOTA","WISCONSIN"),
  geometry = TRUE,
  lower48 = TRUE, 
  weighted_by_area = T)


usda_cattle <- usda_survey %>% 
  filter(!is.na(Value), # exclude missing values, this omits Ramsey county (the UofM has some cattle, but this is low importance)
         commodity_desc == "CATTLE",  # only CATTLE extends to 2021 for survey data, will use census data and interpolation for other livestock
         !grepl("FEED", short_desc)) %>%  ## feedlot data also stops in 2013, so going to use census
  as.data.frame() %>% select(-geometry) #Don't need this to be a spatial object

## there is no non-cattle survey data past 2013. Will need to use inventory data to in-fill other livestock

unique(usda_cattle$short_desc)

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
  mutate(livestock_type = 
           case_when(
             grepl("Replacement", Livestock) ~ "Calves",
             grepl("Feedlot",Livestock) ~ "Feedlot Cattle",
             TRUE ~ Livestock
           )) %>% 
  group_by(Year, livestock_type) %>% 
  summarize(kg_ch4_per_head = mean(Emission_factor_kg_ch4_per_head ))

unique(enteric_agg$livestock_type)
unique(usda_cattle$short_desc)

### match livestock labels to enteric and summarize to county-year-livestockType
usda_cattle_agg <- usda_cattle %>%
  mutate(livestock_type = 
           case_when(
             grepl("MILK", short_desc) ~ "Dairy Cows",
             grepl("BEEF", short_desc) ~ "Beef Cows",
             grepl("FEED", short_desc) ~ "Feedlot Cattle",
             grepl("CALVES",short_desc) ~ "Calves",
             grepl("HOGS", short_desc) ~ "Swine",
             grepl("SHEEP",short_desc) ~ "Sheep",
             TRUE ~ short_desc
           )) %>% 
  group_by(year, county_name, livestock_type) %>% 
  summarise(head_count = sum(Value))


### Calf category appears to be all cattle plus calves. The documentation is scarce, but subtracting away adult cattle appears to be correct
usda_cattle_corrected <- left_join(usda_cattle_agg,
                      usda_cattle_agg %>%
  filter(grepl("Cows$", livestock_type)) %>%
  group_by(year, county_name) %>%
  summarise(cows_sum = sum(head_count)),
  by =  c("year", "county_name")) %>%
  mutate(head_count = ifelse(livestock_type == "Calves", head_count - cows_sum, head_count)) %>%
  select(-cows_sum)

ggplot(usda_cattle_corrected, aes(x = year, y = head_count, col = county_name)) + geom_line() + facet_wrap(.~livestock_type)


### merge enteric cattle data with head count survey data

cow_burps <- left_join(usda_cattle_corrected, enteric_agg, by = c("year" = "Year", "livestock_type" = "livestock_type")) %>% 
  mutate(kg_ch4 = kg_ch4_per_head * head_count,
         MT_ch4 = kg_ch4 / 1000,
         CO2e = MT_ch4 * gwp$ch4)

ggplot(cow_burps %>% group_by(year,county_name) %>% summarise(CO2e = sum(CO2e)),
       aes(x = year, y = CO2e, col = county_name)) + geom_line() + theme_bw()

#### Census data ####

usda_census <- tidyUSDA::getQuickstat(
  sector="ANIMALS & PRODUCTS",
  group="LIVESTOCK",
  commodity=NULL,
  category= "INVENTORY",
  domain= NULL,
  county= counties,
  key = key,
  program = "CENSUS",
  data_item = NULL,
  geographic_level = 'COUNTY',
  year = as.character(2002:2022),
  state = c("MINNESOTA","WISCONSIN"),
  geometry = TRUE,
  lower48 = TRUE, 
  weighted_by_area = T) %>% 
  as.data.frame() %>% select(-geometry)

### what are the various descriptors that are needed here (metadata is lacking or hard to find)
unique(usda_census$unit_desc) # "HEAD"       "OPERATIONS"
unique(usda_census$short_desc) # 32 categories, broadly broken down into operations vs inventory; types of cattle, hogs, sheep, goats
unique(usda_census$class_desc) # just the species modifier (e.g. 'cows, beef', 'ewes, breeding')
unique(usda_census$group_desc) #all "LIVESTOCK"
unique(usda_census$commodity_desc) #"CATTLE" "GOATS"  "HOGS"   "SHEEP" 
unique(usda_census$statisticcat_desc) #all "INVENTORY"
unique(usda_census$domaincat_desc) # like short_desc but with range of number of head, e.g. "INVENTORY OF CATTLE ON FEED: (1 TO 19 HEAD)"  
unique(usda_census$domain_desc) # 8 inventory types (e.g. cattle on feed) and "TOTAL" 
usda_census %>% filter(domain_desc == "TOTAL", Value != "NA") %>% distinct(short_desc) # "NOT SPECFIED"
usda_census %>% filter(unit_desc == "HEAD", Value != "NA") %>% distinct(short_desc) # "NOT SPECFIED"

usda_census %>% filter(county_name == "DAKOTA", year == 2017, commodity_desc == "CATTLE", unit_desc == "HEAD", Value != "NA",
                       domain_desc == "TOTAL") %>% 
  group_by(short_desc) %>% 
  summarize(value = sum(Value))
# comparing to USDA county profile, CATTLE, INCL CALVES - INVENTORY is the total number of cattle, other categories are subsets
# So CATTLE, (EXCL COWS) - INVENTORY is a calf inventory, CATTLE, COWS, BEEF - INVENTORY is adult beef cows, CATTLE, COWS, MILK - INVENTORY
# are dairy cows, and CATTLE, ON FEED - INVENTORY are calves on feed (documentation says explicitly these are not COWS)
# We'll create four categories - dairy cows, beef cows, calves, feedlot cattle

usda_census %>% filter(commodity_desc == "HOGS", unit_desc == "HEAD", Value != "NA",
                       domain_desc == "TOTAL") %>% 
  group_by(year,short_desc) %>% 
  summarize(value = sum(Value))

usda_census %>% filter( commodity_desc == "SHEEP", unit_desc == "HEAD", Value != "NA",
                       domain_desc == "TOTAL") %>% 
  group_by(year,short_desc,county_name) %>% 
  summarize(value = sum(Value)) %>% 
  filter

### rename and aggregate
usda_census_agg <- usda_census %>%
  filter(domain_desc == "TOTAL",!is.na(Value),
         short_desc %in% c("CATTLE, (EXCL COWS) - INVENTORY",
                           "CATTLE, ON FEED - INVENTORY",
                           "CATTLE, COWS, BEEF - INVENTORY",
                           "CATTLE, COWS, MILK - INVENTORY",
                           "GOATS - INVENTORY",
                           "HOGS - INVENTORY",
                           "SHEEP, INCL LAMBS - INVENTORY")) %>% 
  mutate(livestock_type = 
           case_when(
             grepl("MILK", short_desc) ~ "Dairy Cows",
             grepl("BEEF", short_desc) ~ "Beef Cows",
             grepl("FEED", short_desc) ~ "Feedlot Cattle",
             grepl("(EXCL COWS)",short_desc) ~ "Calves",
             grepl("HOGS", short_desc) ~ "Swine",
             grepl("SHEEP",short_desc) ~ "Sheep",
             grepl("GOATS",short_desc) ~ "Goats",
             TRUE ~ short_desc
           )) %>% 
  group_by(year, county_name, livestock_type) %>% 
  summarise(head_count = sum(Value))

### subtract feedlot calves from total calves
usda_census_corrected <- left_join(usda_census_agg,
                                   usda_census_agg %>%
                                     filter(grepl("Feedlot", livestock_type)) %>%
                                     group_by(year, county_name) %>%
                                     summarise(feedlot_sum = sum(head_count)),
                                   by =  c("year", "county_name")) %>%
  mutate(head_count = ifelse(livestock_type == "Calves", head_count - feedlot_sum, head_count)) %>%
  select(-feedlot_sum)

### interpolate between census years for all animal types
census_interpolated <- left_join( # this creates an empty grid of all desired year,livestock combinations
    expand.grid(
    year = seq(2002, 2022, by = 1),
    county_name = unique(usda_census_corrected$county_name),
    livestock_type = unique(usda_census_corrected$livestock_type)
    ),
    usda_census_corrected) %>%  # merged with our populated census data, it creates NAs wherever data is missing
  mutate(head_count = if_else(year %in% c(2002,2007,2012,2017,2022) & is.na(head_count),0, head_count)) %>%  # override census years to be 0 if livestock-county combo is missing
  group_by(county_name, livestock_type) %>%
  arrange(year) %>%
  mutate(
    head_count = zoo::na.approx(head_count, na.rm = FALSE), # this function linearly interpolates NAs between known values, following the group_by
    status = ifelse(year %in% c(2002,2007,2012,2017,2022), 'census', 'interpolated') # marking whether values are from the census or interpolation
  )

## merge this with enteric fermentation data by year
animal_burps <- left_join(census_interpolated %>% filter(year >=2005 & year <= 2021), enteric_agg, by = c("year" = "Year", "livestock_type" = "livestock_type")) %>% 
  mutate(kg_ch4 = kg_ch4_per_head * head_count,
         MT_ch4 = kg_ch4 / 1000,
         CO2e = MT_ch4 * gwp$ch4)

ggplot(animal_burps %>% 
         group_by(year,county_name) %>% 
         summarize(CO2e = sum(CO2e)),
       aes(x = year, y = CO2e, col = county_name)) + geom_line(size = 1.5) + theme_bw()

animal_burps %>% filter(year == 2021) %>% ungroup %>%  summarize(CO2e = sum(CO2e))

#### manure and lagoons ####

### get labels from N2O and CH4 emissions

### format ch4 emissions factors

manure_ch4_formatted  <- manure_ch4  %>% 
  select(c(2,3,4,18)) %>% 
  setNames(c("Year","Livestock","Heads_thousands","Metric_tons_ch4")) %>% 
  filter(!(is.na(Year) | is.na(Metric_tons_ch4) | Livestock == "TOTAL")) %>% 
  filter(Year >= 2005) %>% 
  mutate(heads = as.numeric(str_remove_all(Heads_thousands,",")) * 1000, mt_ch4 = as.numeric(str_remove_all(Metric_tons_ch4,",")), 
  Emission_factor_mt_ch4_per_head = mt_ch4/heads) %>% 
  mutate(livestock_type = 
           case_when(
             grepl("Replacement", Livestock) ~ "Calves",
             grepl("Feedlot",Livestock) ~ "Feedlot Cattle",
             grepl("Hens",Livestock) ~ "Layers",
             grepl("Chickens",Livestock) ~ "Layers",
             TRUE ~ Livestock
           )) %>% 
  group_by(Year, livestock_type) %>% 
  summarize(ch4_per_head = mean(Emission_factor_mt_ch4_per_head))

### format n20 emissions factors

manure_n2o_formatted <- manure_n2o  %>% 
  select(c(2,3,4,16)) %>% 
  setNames(c("Year","Livestock","Heads_thousands","kg_n2o")) %>% 
  filter(!(is.na(Year) | is.na(kg_n2o) | Livestock == "TOTAL")) %>% 
  filter(Year >= 2005) %>% 
  mutate(heads = as.numeric(str_remove_all(Heads_thousands,",")) * 1000, kg_n2o = as.numeric(str_remove_all(kg_n2o,",")), 
         Emission_factor_kg_n2o_per_head = kg_n2o/heads) %>% 
  mutate(livestock_type = 
           case_when(
             grepl("Replacement", Livestock) ~ "Calves",
             grepl("Feedlot",Livestock) ~ "Feedlot Cattle",
             grepl("Hens",Livestock) ~ "Layers",
             grepl("Chickens",Livestock) ~ "Layers",
             TRUE ~ Livestock
           )) %>% 
  group_by(Year, livestock_type) %>% 
  summarize(kg_n2o_per_head = mean(Emission_factor_kg_n2o_per_head))


usda_poultry <- tidyUSDA::getQuickstat(
  sector="ANIMALS & PRODUCTS",
  group="POULTRY",
  commodity=NULL,
  category= "INVENTORY",
  domain= NULL,
  county= counties,
  key = key,
  program = "CENSUS",
  data_item = NULL,
  geographic_level = 'COUNTY',
  year = as.character(2002:2022),
  state = c("MINNESOTA","WISCONSIN"),
  geometry = TRUE,
  lower48 = TRUE, 
  weighted_by_area = T) %>% 
  as.data.frame() %>% select(-geometry)

### rename and aggregate
usda_poultry_agg <- usda_poultry %>%
  filter(domain_desc == "TOTAL",!is.na(Value),
         short_desc %in% c("CHICKENS, BROILERS - INVENTORY",
                           "CHICKENS, LAYERS - INVENTORY",
                           "CHICKENS, PULLETS, REPLACEMENT - INVENTORY",
                           "CHICKENS, ROOSTERS - INVENTORY",
                           "TURKEYS - INVENTORY")) %>% 
  mutate(livestock_type = 
           case_when(
             grepl("BROILERS", short_desc) ~ "Broilers",
             grepl("LAYERS", short_desc) ~ "Layers",
             grepl("PULLETS", short_desc) ~ "Pullets",
             grepl("ROOSTERS",short_desc) ~ "Broilers",
             grepl("TURKEYS", short_desc) ~ "Turkeys",
             TRUE ~ short_desc
           )) %>% 
  group_by(year, county_name, livestock_type) %>% 
  summarise(head_count = sum(Value))

### subtract feedlot calves from total calves
usda_census_corrected <- left_join(usda_census_agg,
                                   usda_census_agg %>%
                                     filter(grepl("Feedlot", livestock_type)) %>%
                                     group_by(year, county_name) %>%
                                     summarise(feedlot_sum = sum(head_count)),
                                   by =  c("year", "county_name")) %>%
  mutate(head_count = ifelse(livestock_type == "Calves", head_count - feedlot_sum, head_count)) %>%
  select(-feedlot_sum)

### interpolate between census years for all animal types
census_interpolated <- left_join( # this creates an empty grid of all desired year,livestock combinations
  expand.grid(
    year = seq(2002, 2022, by = 1),
    county_name = unique(usda_census_corrected$county_name),
    livestock_type = unique(usda_census_corrected$livestock_type)
  ),
  usda_census_corrected) %>%  # merged with our populated census data, it creates NAs wherever data is missing
  mutate(head_count = if_else(year %in% c(2002,2007,2012,2017,2022) & is.na(head_count),0, head_count)) %>%  # override census years to be 0 if livestock-county combo is missing
  group_by(county_name, livestock_type) %>%
  arrange(year) %>%
  mutate(
    head_count = zoo::na.approx(head_count, na.rm = FALSE), # this function linearly interpolates NAs between known values, following the group_by
    status = ifelse(year %in% c(2002,2007,2012,2017,2022), 'census', 'interpolated') # marking whether values are from the census or interpolation
  )


#is TOTAL overlapping with non-TOTAL fields?
usda_livestock_use %>% filter(domain_desc == "TOTAL", grepl("CATTLE", short_desc)) %>% 
  group_by(year) %>% 
  summarize(cattle_count = sum(Value)) %>% print(n=50)

usda_livestock_use %>% filter(domain_desc != "TOTAL", grepl("CATTLE", short_desc)) %>% 
  group_by(year) %>% 
  summarize(cattle_count = sum(Value))

## non-TOTAL fields are from 5 year inventories, omitting inventory for now. TOTAL fields roughly triple in inventory years, digging down further


usda_livestock_use



  

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



  

