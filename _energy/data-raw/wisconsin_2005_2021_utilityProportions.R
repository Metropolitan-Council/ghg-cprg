source("R/_load_pkgs.R")
source("_meta/data-raw/cprg_geography.R")
library(tidycensus)
library(tigris)
options(tidycensus.cache = TRUE)


#2005 Wisconsin population blocks
# Fetch data for the 2000 decennial census for Wisconsin 
population_data_2000_wi <- get_decennial(
  geography = "block",
  variables = "P001001",  # Total population
  state = "WI",
  year = 2000,
  geometry = TRUE,  # Include geometry for spatial operations
  output = "wide"  # Outputs data in a 'wide' format, each variable as a separate column
) %>%
  rename(
    pop2000 = P001001
  )

# Fetch data for the 2010 decennial census for Wisconsin
population_data_2010_wi <- get_decennial(
  geography = "block",
  variables = "P001001",  # Total population
  state = "WI",
  year = 2010,
  geometry = TRUE,  # Include geometry for spatial operations
  output = "wide"  # Outputs data in a 'wide' format, each variable as a separate column
) %>%
  rename(
    pop2010 = P001001
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
    pop2000_inAtom = pop2000 * WEIGHT
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
    pop2000 = popIn2000_on2010blocks
  ) %>%
  mutate(
    pop2005_interpolated = ((pop2000 + pop2010) / 2)
  )

GEOID10_2005_population_WI <- GEOID10_2000_2005_2010_population_WI %>%
  select(-pop2000, -NAME, -pop2010)

est_state_pop_2005 <- st_drop_geometry(GEOID10_2005_population_WI) %>%
  group_by(state) %>%
  summarize(
    state_population = sum(pop2005_interpolated)
  )


#2021 -- using 2020 decennial as best approximation --> blocks to utility service areas
population_data_2020_wi <- get_decennial(
  geography = "block",
  variables = "P1_001N",  # Total population
  state = "WI",
  year = 2020,
  geometry = TRUE,  # Include geometry for spatial operations
  output = "wide"  # Outputs data in a 'wide' format, each variable as a separate column
) %>%
  rename(
    pop2020 = P1_001N
  )  %>%
  mutate(
    state = "WI"
  ) %>%
  st_centroid()

state_pop_2020 <- st_drop_geometry(population_data_2020_wi) %>%
  group_by(state) %>%
  summarize(
    state_population = sum(pop2020)
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



#snag correct county spatial reference to join to 
counties10 <- counties(state = "WI", year = 2010) %>% st_transform(st_crs(wi_pop_2005))
counties20 <- counties(state = "WI", year = 2020) %>% st_transform(st_crs(wi_pop_2021))

# add county info and limit the spatial extent of the block-level population to the extent of full in-scope utility areas, to conserve memory
wi_pop_2005_filtered <- wi_pop_2005 %>%
  st_filter(inScope_WI_elecUtils_fullServTerr, .predicate = st_intersects) %>%
  st_join(counties10 %>% select(county = NAME10), join = st_intersects) %>%
  st_join(inScope_WI_elecUtils_fullServTerr, join = st_intersects)

wi_pop_2021_filtered <- wi_pop_2021 %>%
  st_filter(inScope_WI_elecUtils_fullServTerr, .predicate = st_intersects) %>%
  st_join(counties20 %>% select(county = NAME), join = st_intersects) %>%
  st_join(inScope_WI_elecUtils_fullServTerr, join = st_intersects) 


#store population files in data-raw -- these shouldn't be tracked on git bcuz they are large
write_rds(wi_pop_2005_filtered, here("_energy",
                            "data-raw",
                            "wi_popBlocks_2005_withUtility.RDS")
)
  
write_rds(wi_pop_2021_filtered, here("_energy",
                            "data-raw",
                            "wi_popBlocks_2021_withUtility.RDS")
)


# clean up intermediate dfs/sfs before heavy duty joins to conserve memory
rm(GEOID10_2005_population_WI)
rm(GEOID10_2000_2005_2010_population_WI)
rm(population_data_2000_wi)
rm(population_data_2010_wi)
rm(population_data_2020_wi)
rm(crosswalkPop_2000_to_2010)
rm(crosswalkPop_2000_to_2010_centroids)
rm(crosswalkWI)

#add state pop reference in mutate statement
#Summarize to utility
wi_pop_2005_utility <- st_drop_geometry(wi_pop_2005_filtered) %>%
  group_by(utility_name) %>%
  summarize(
    estTotalServiceAreaPop = sum(pop2005_interpolated),
    .groups = 'keep'
  ) %>%
  ungroup() %>%
  mutate(
    year = 2005
  )

wi_pop_2021_utility <- st_drop_geometry(wi_pop_2021_filtered) %>%
  group_by(utility_name) %>%
  summarize(
    estTotalServiceAreaPop = sum(pop2020),
    .groups = 'keep'
  ) %>%
  ungroup() %>%
  mutate(
    year = 2021
  )

wi_pop_utility <- rbind(wi_pop_2005_utility, wi_pop_2021_utility)

#Summarize to utility-county

wi_pop_2005_utilityCounty <- st_drop_geometry(wi_pop_2005_filtered) %>%
  group_by(utility_name, county) %>%
  summarize(
    estServiceAreaPop = sum(pop2005_interpolated),
    .groups = 'keep'
  ) %>%
  ungroup() %>%
  mutate(
    year = 2005
  ) %>%
  filter(
    county %in% c('St. Croix', 'Pierce')
  )


wi_pop_2021_utilityCounty <- st_drop_geometry(wi_pop_2021_filtered) %>%
  group_by(utility_name, county) %>%
  summarize(
    estServiceAreaPop = sum(pop2020),
    .groups = 'keep'
  ) %>%
  ungroup() %>%
  mutate(
    year = 2021
  ) %>%
  filter(
    county %in% c('St. Croix', 'Pierce')
  )

wi_pop_utilityCounty <- rbind(wi_pop_2005_utilityCounty, wi_pop_2021_utilityCounty)


#Combine tables, calculate proportions
wi_2005_2021_utilityCounty_popProp <- wi_pop_utilityCounty %>%
  left_join(wi_pop_utility,
            by = join_by(utility_name, year),
            relationship = 'many-to-many') %>%
  mutate(
    estServiceAreaPop = round(estServiceAreaPop),
    estTotalServiceAreaPop = round(estTotalServiceAreaPop)
    ) %>%
  mutate(
    propUtilityPopInCounty = estServiceAreaPop / estTotalServiceAreaPop
  )

write_rds(wi_2005_2021_utilityCounty_popProp, here("_energy",
                                                   "data-raw",
                                                   "wi_2005_2021_utilityCounty_popProp.RDS")
)



  
