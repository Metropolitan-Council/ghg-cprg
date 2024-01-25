# get data from the 2020 national emissions inventory
source("R/_load_pkgs.R")


cprg_county <- readRDS("R/data/cprg_county.RDS")

nei_onroad <- read.csv("_transportation/data-raw/epa/nei2020/2020nei_onroad_byregion/onroad_5.csv") %>% 
  clean_names() %>% 
  filter(fips_code %in% cprg_county$GEOID,
         pollutant_type_s == "GHG")

nei_search <- read.csv("_transportation/data-raw/epa/nei2020/nei_search.csv") %>% 
  clean_names() %>% 
  mutate(county_fips = paste0(state_fips, stringr::str_pad(as.character(county_fips), width = 3,
                                        side = "left", "0"))) %>% 
  filter(county_fips %in% cprg_county$GEOID)

