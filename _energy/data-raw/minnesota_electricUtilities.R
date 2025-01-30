# read and save Minnesota electric utility areas
source("R/_load_pkgs.R")

mn_counties <- read_rds("_meta/data/cprg_county.RDS") %>%
  filter(statefp == 27)


# Downloaded from https://gisdata.mn.gov/dataset/util-eusa. Minnesota provides
# all three utility types (IOU, Muni, Co-op) in one file
mn_elecUtils <- st_read(here(
  "_energy", "data-raw",
  "shp_util_eusa_MN", "Service_Areas.shp"
)) %>%
  select(
    comments, municipal, mmua_name, full_name, type, utility, street, city,
    state, zip, website, mpuc_name, mn_utility, eia_utilit, geometry
  )

# reproject MN counties to same projection as mn_elecUtils to perform intersection
mn_counties <- st_transform(mn_counties, st_crs(mn_elecUtils))

# identify utilities that operate in study area
MNutilities_in_scope <- st_intersection(mn_elecUtils, mn_counties) %>%
  select(
    comments, municipal, type, street, city, state, zip, website, mpuc_name,
    mn_utility, eia_utilit, county_name_full, county_name, state_name, statefp, geometry
  ) %>%
  rename(
    utility_type = type,
    eia_utility = eia_utilit,
    county_name = county_name_full,
    county = county_name,
    mn_utility_id = mn_utility
  )

distinct_util_type_MN <- MNutilities_in_scope %>%
  distinct(mpuc_name, utility_type)

write_rds(mn_elecUtils, here(
  "_energy",
  "data",
  "MN_elecUtils.RDS"
))

write_rds(MNutilities_in_scope, here(
  "_energy",
  "data",
  "MN_electricity_inScope_utilityCountyPairs.RDS"
))

write_rds(distinct_util_type_MN, here(
  "_energy",
  "data",
  "distinct_electricity_util_type_MN.RDS"
))
