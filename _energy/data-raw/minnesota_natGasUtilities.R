source("R/_load_pkgs.R")
library(readr)

mn_counties <- read_rds("_meta/data/cprg_county.RDS") %>%
  select(STATEFP,COUNTYFP,GEOID, NAME, NAMELSAD, geometry) %>%
  filter(STATEFP == 27) 

fed_natGasUtils <- st_read(here("_energy", "data-raw", "Natural_Gas_Service_Territories", "NG_Service_Terr.shp")) %>%
  select(NAME, TYPE, COUNTY, COUNTYFIPS, SOURCE)

#reproject MN counties to same projection as mn_elecUtils to perform intersection
mn_counties <- st_transform(mn_counties, st_crs(fed_natGasUtils))

#identify utility-county combos present that operate in study area
MNutilities_in_scope <- st_intersection(fed_natGasUtils, mn_counties) %>%
  rename(utility_name = NAME, utility_type = TYPE, county_name = COUNTY, dataSource = SOURCE) %>%
  select(utility_name, utility_type, county_name, dataSource, geometry)


#identify distinct utilities by that operate in study area
distinct_natGas_util_type_MN <- MNutilities_in_scope %>%
  distinct(utility_name, utility_type)

write_rds(fed_natGasUtils, here("_energy", "data","MN_fed_natGasUtils.RDS"))
write_rds(MNutilities_in_scope, here("_energy", "data", "MN_natGas_inScope_utilityCountyPairs.RDS"))
write_rds(distinct_natGas_util_type_MN, here("_energy", "data", "distinct_natGas_util_type_MN.RDS"))
