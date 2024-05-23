source("R/_load_pkgs.R")
#source cprg county
# --> end result here gets added to cprg proportions


#read in utility service area spatial files -- unique utility-county pairs, and total service areas

#WI
WI_elecUtilities_area_in_scope <- readRDS(here(
  "_energy",
  "data",
  "WI_electricity_inScope_utilityCountyPairs.RDS"
))

inScope_WI_elecUtils_fullServTerr <- readRDS(here(
  "_energy",
  "data",
  "WI_inScope_elecUtils_fullServTerr.RDS"
))


#MN
MN_elecUtilities_area_in_scope <- readRDS(here(
  "_energy",
  "data",
  "MN_electricity_inScope_utilityCountyPairs.RDS"
))

#need to condense Xcel into one file, and dissolve all Great River Energy-supplied utilities into one polygon, if we are going to perform spatial analysis
inScope_MN_elecUtils_fullServTerr <- readRDS(here(
  "_energy",
  "data",
  "MN_elecUtils.RDS"
)) %>%
  filter(mn_utility %in% MN_elecUtilities_area_in_scope$mn_utility_id)



# Transform CRS of the population data to match the service area data
wi_pop_2005 <- st_transform(wi_pop_2005, st_crs(fullServiceArea_inScopeUtilities_WI))
wi_pop_2021 <- st_transform(wi_pop_2021, st_crs(fullServiceArea_inScopeUtilities_WI))

# Spatial join between total utility areas and population data
utility_pop_totals_2005 <- fullServiceArea_inScopeUtilities_WI %>%
  st_join(wi_pop_2005, join = st_intersects) %>%
  group_by(utility_name) %>%  # Assuming utility name column is 'utility_name'
  summarize(total_pop_served = sum(estimate), .groups = 'drop')

utility_pop_totals_2021 <- fullServiceArea_inScopeUtilities_WI %>%
  st_join(wi_pop_2021, join = st_intersects) %>%
  group_by(utility_name) %>%  # Assuming utility name column is 'utility_name'
  summarize(total_pop_served = sum(estimate), .groups = 'drop')

# Spatial join between in-scope utility areas and population data
utility_pop_county_2005 <- WIutilities_in_scope_CensusBlockSums %>%
  st_join(wi_pop_2005, join = st_intersects) %>%
  group_by(utility_name) %>%  # Assuming utility name column is 'utility_name'
  summarize(total_pop_served = sum(estimate), .groups = 'drop')

utility_pop_county_2021 <- WIutilities_in_scope_CensusBlockSums %>%
  st_join(wi_pop_2021, join = st_intersects) %>%
  group_by(utility_name) %>%  # Assuming utility name column is 'utility_name'
  summarize(total_pop_served = sum(estimate), .groups = 'drop')
