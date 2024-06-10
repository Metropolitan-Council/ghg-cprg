source("R/_load_pkgs.R")
source("R/global_warming_potential.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")

enteric <- read_csv('_agriculture/data-raw/enteric-fermentation.csv')
manure_n2o <- read_csv('_agriculture/data-raw/manure-n2o.csv')
manure_ch4 <- read_csv('_agriculture/data-raw/manure-ch4.csv')
mn_feedlots <- read_csv('_agriculture/data-raw/mn-feedlots.csv') %>% 
  filter(county_name %in% cprg_county$NAME)

### format enteric fermentation emissions factors

enteric_formatted <- enteric  %>% 
  select(c(2,3,6)) %>% 
  setNames(c("Year","Livestock","Emission_factor_kg_ch4_per_head")) %>% 
  filter(!(is.na(Year) | is.na(Emission_factor_kg_ch4_per_head))) %>% 
  filter(Year >= 2005) %>% 
  mutate(Emission_factor_kg_ch4_per_head = as.numeric(Emission_factor_kg_ch4_per_head))



### format n20 emissions factors

manure_n2o_formatted <- manure_n2o  %>% 
  select(c(2,3,6)) %>% 
  setNames(c("Year","Livestock","Emission_factor_kg_ch4_per_head")) %>% 
  filter(!(is.na(Year) | is.na(Emission_factor_kg_ch4_per_head))) %>% 
  filter(Year >= 2005) %>% 
  mutate(Emission_factor_kg_ch4_per_head = as.numeric(Emission_factor_kg_ch4_per_head))


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
  

