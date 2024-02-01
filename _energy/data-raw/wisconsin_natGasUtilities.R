source("R/_load_pkgs.R")
library(readr)

wi_counties <- read_rds("_meta/data/cprg_county.RDS") %>%
  select(STATEFP, COUNTYFP, GEOID, NAME, NAMELSAD, geometry) %>%
  filter(STATEFP == 55)

# Wisconsin maintains state-level natural gas service area maps. Downloaded from https://psc.wi.gov/Pages/ForConsumers/Maps.aspx.
WI_natGasUtils <- st_read(here("_energy", "data-raw", "NG_PSCW_ServTerr", "NG_PSCW_ServTerr.shp")) %>%
  select(Util_Name, Util_ID, CITY, geometry)

# reproject WI counties to same projection as wi_natGasUtils to perform intersection
wi_counties <- st_transform(wi_counties, st_crs(WI_natGasUtils)) %>%
  select(GEOID, NAME, NAMELSAD, geometry)

# identify utilities that operate in study area
WIutilities_in_scope <- st_intersection(WI_natGasUtils, wi_counties) %>%
  rename(utility_name = Util_Name, county_name = NAME) %>%
  select(utility_name, county_name, geometry)

distinct_natGas_util_WI <- WIutilities_in_scope %>%
  distinct(utility_name)

write_rds(WI_natGasUtils, here("_energy", "data", "WI_natGasUtils.RDS"))
write_rds(WIutilities_in_scope, here("_energy", "data", "WI_natGas_inScope_utilityCountyPairs.RDS"))
write_rds(distinct_natGas_util_WI, here("_energy", "data", "distinct_natGas_util_WI.RDS"))
