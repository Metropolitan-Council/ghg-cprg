source("R/_load_pkgs.R")
source("_meta/data-raw/cprg_geography.R")
library(tidycensus)
options(tidycensus.cache = TRUE)


#2005 Wisconsin popualtion blocks
# Fetch data for the 2000 decennial census for Wisconsin 
population_data_2000_wi <- get_decennial(
  geography = "block",
  variables = "P001001",  # Total population
  state = "WI",
  year = 2000,
  geometry = TRUE,  # Include geometry for spatial operations
  output = "wide"  # Outputs data in a 'wide' format, each variable as a separate column
)

# Fetch data for the 2010 decennial census for Wisconsin
population_data_2010_wi <- get_decennial(
  geography = "block",
  variables = "P001001",  # Total population
  state = "WI",
  year = 2010,
  geometry = TRUE,  # Include geometry for spatial operations
  output = "wide"  # Outputs data in a 'wide' format, each variable as a separate column
)


# Calculating geographic crosswalks from 2000 blocks to 2010 blocks
# 1) Obtain data of interest for 2000 blocks (see population_data_2000 above) and download crosswalk

crosswalkWI <- read_csv(here("_energy","data-raw","nhgis_blk2000_blk2010_WI","nhgis_blk2000_blk2010_ge_55.csv")) %>%
  mutate(
    GEOID00 = as.character(GEOID00),
    GEOID10 = as.character(GEOID10)
  )

# 2) Join the 2000-block-to-2010-block crosswalk to the 2000 block data of interest
crosswalkPop_2000_to_2010 <- population_data_2000_wi %>%
  left_join(crosswalkWI,
            by = join_by(GEOID == GEOID00)) %>%
  # 3) Multiply the 2000 block counts by the crosswalk's interpolation weights, producing estimated counts for all 2000-2010 block intersections, or "atoms"
  mutate(
    pop2000_inAtom = P001001 * WEIGHT
  ) %>%
  # 4) Sum these atom counts for each 2010 block, join to 2010 data of interest (population_data_2010)
  group_by(GEOID10) %>%
  summarise(
    popIn2000_on2010blocks = sum(pop2000_inAtom)
  )

crosswalkPop_2000_to_2010_centroids <- st_centroid(crosswalkPop_2000_to_2010) %>%
  mutate(
    GEOID10 = as.character(GEOID10)
  ) %>%
  mutate(
    state = "WI"
    )


GEOID10_2000_2005_2010_population_WI <- crosswalkPop_2000_to_2010_centroids %>%
  left_join(st_drop_geometry(population_data_2010_wi),
            by = join_by(GEOID10 == GEOID)
  ) %>%
  rename(
    totalPop2010 = P001001,
    totalPop2000 = popIn2000_on2010blocks
  ) %>%
  mutate(
    totalPop2005_interpolated = ((totalPop2000 + totalPop2010) / 2)
  )

GEOID10_2005_population_WI <- GEOID10_2000_2005_2010_population_WI %>%
  select(-totalPop2000, -NAME, -totalPop2010)

est_state_pop_2005 <- st_drop_geometry(GEOID10_2005_population_WI) %>%
  group_by(state) %>%
  summarize(
    state_population = sum(totalPop2005_interpolated)
  )


#2021 -- using 2020 decennial as best approximation --> blocks to utility service areas
population_data_2020_wi <- get_decennial(
  geography = "block",
  variables = "P1_001N",  # Total population
  state = "WI",
  year = 2020,
  geometry = TRUE,  # Include geometry for spatial operations
  output = "wide"  # Outputs data in a 'wide' format, each variable as a separate column
)


#read in utility service area spatial files -- unique utility-county pairs, and total service areas
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


# Transform CRS of the population data to match the service area data before spatial joins
wi_pop_2005 <- st_transform(GEOID10_2005_population_WI, st_crs(inScope_WI_elecUtils_fullServTerr))
wi_pop_2021 <- st_transform(population_data_2020_wi, st_crs(inScope_WI_elecUtils_fullServTerr))


# Spatial join between total utility areas and population data to enable estimation of utility service area popualtion
utility_pop_totals_2005 <- inScope_WI_elecUtils_fullServTerr %>%
  st_join(wi_pop_2005, join = st_intersects) %>%
  group_by(utility_name) %>%  
  summarize(total_pop_served = sum(totalPop2005_interpolated), .groups = 'drop') %>%
  mutate(year = 2005)

utility_pop_totals_2021 <- inScope_WI_elecUtils_fullServTerr %>%
  st_join(wi_pop_2021, join = st_intersects) %>%
  group_by(utility_name) %>% 
  summarize(total_pop_served = sum(P1_001N), .groups = 'drop') %>%
  mutate(year = 2021)

# Spatial join between in-scope utility areas and population data
utility_pop_county_2005 <- WI_elecUtilities_area_in_scope %>%
  st_join(wi_pop_2005, join = st_intersects) %>%
  group_by(utility_name, county) %>%
  summarize(total_pop_served = sum(totalPop2005_interpolated), .groups = 'drop') %>%
  mutate(year = 2005)

utility_pop_county_2021 <- WI_elecUtilities_area_in_scope %>%
  st_join(wi_pop_2021, join = st_intersects) %>%
  group_by(utility_name, county) %>%  
  summarize(total_pop_served = sum(P1_001N), .groups = 'drop') %>%
  mutate(year = 2021)
