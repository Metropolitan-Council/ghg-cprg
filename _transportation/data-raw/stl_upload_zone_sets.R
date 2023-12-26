# run once
# Upload county and city geographies to StreetLight, using their API
source("R/_load_pkgs.R")
library(streetlightR)

# fetch login email
login_email <- keyring::key_get("StL_email")

# set key for session
streetlightR::streetlight_api_key(key = keyring::key_get("StreetLightAPI"))


# counties -----
cprg_counties <- readRDS("R/data/cprg_county.RDS")

# create zone name, passthrough = TRUE
stl_counties <- cprg_counties %>% 
  mutate(name = NAME,
         is_pass = 0) %>% 
  select(name, is_pass)

# upload counties
upload_zone_set(
  login_email = login_email,
  geom_type = "polygon",
  zones = stl_counties,
  zone_set_name = "CPRG_Counties_NP",
)


# ctus -----
cprg_ctu <- readRDS("R/data/cprg_ctu.RDS")

# create zone name, passthrough = TRUE
stl_ctu <- cprg_ctu %>% 
  mutate(name = paste0(CTU_NAME, "_", CTU_CLASS, "_", COUNTY_NAM),
         is_pass = 0) %>% 
  select(name, is_pass)

# double check that zone names are unique
nrow(unique(stl_ctu)) == nrow(cprg_ctu)

# upload
upload_zone_set(
  login_email = login_email,
  geom_type = "polygon",
  zones = stl_ctu,
  zone_set_name = "CPRG_CTUs_NP",
)

