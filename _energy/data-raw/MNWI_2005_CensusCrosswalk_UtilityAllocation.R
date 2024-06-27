source("R/_load_pkgs.R")
source("_meta/data-raw/cprg_geography.R")
library(tidycensus)
options(tidycensus.cache = TRUE)


# Fetch data for the 2000 decennial census for Wisconsin and Minnesota
population_data_2000 <- get_decennial(
  geography = "block",
  variables = "P001001",  # Total population
  state = c("WI", "MN"),
  year = 2000,
  geometry = TRUE,  # Include geometry for spatial operations
  output = "wide"  # Outputs data in a 'wide' format, each variable as a separate column
)

# Fetch data for the 2010 decennial census for Wisconsin and Minnesota
population_data_2010 <- get_decennial(
  geography = "block",
  variables = "P001001",  # Total population
  state = c("WI", "MN"),
  year = 2010,
  geometry = TRUE,  # Include geometry for spatial operations
  output = "wide"  # Outputs data in a 'wide' format, each variable as a separate column
)


# Calculating geographic crosswalks from 2000 blocks to 2010 blocks
# 1) Obtain data of interest for 2000 blocks (see population_data_2000 above) and download crosswalk
crosswalkMN <- read_csv(here("_energy","data-raw","nhgis_blk2000_blk2010_MN","nhgis_blk2000_blk2010_ge_27.csv"))
crosswalkWI <- read_csv(here("_energy","data-raw","nhgis_blk2000_blk2010_WI","nhgis_blk2000_blk2010_ge_55.csv"))
CombinedCrosswalk <- rbind(crosswalkMN,crosswalkWI) %>%
  mutate(
    GEOID00 = as.character(GEOID00),
    GEOID10 = as.character(GEOID10)
  )

# 2) Join the 2000-block-to-2010-block crosswalk to the 2000 block data of interest
crosswalkPop_2000_to_2010 <- population_data_2000 %>%
  left_join(CombinedCrosswalk,
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
    state = case_when(
      substr(as.character(GEOID10), 1, 2) == "27" ~ "MN",
      substr(as.character(GEOID10), 1, 2) == "55" ~ "WI",
      TRUE ~ NA_character_
    )
  )


GEOID10_2000_2005_2010_population_MNWI <- crosswalkPop_2000_to_2010_centroids %>%
  left_join(st_drop_geometry(population_data_2010),
            by = join_by(GEOID10 == GEOID)
            ) %>%
  rename(
    totalPop2010 = P001001,
    totalPop2000 = popIn2000_on2010blocks
  ) %>%
  mutate(
    totalPop2005_interpolated = ((totalPop2000 + totalPop2010) / 2)
  )

GEOID10_2005_population_MNWI <- GEOID10_2000_2005_2010_population_MNWI %>%
  select(-totalPop2000, -NAME, -totalPop2010)

est_state_pop_2005 <- st_drop_geometry(GEOID10_2005_population_MNWI) %>%
  group_by(state) %>%
  summarize(
    state_population = sum(totalPop2005_interpolated)
  )


#rejoin back to cprg_county to hold onto necessary reference data
cprg_county_population2005_export <- cprg_county %>%
  left_join((st_drop_geometry(intercensal_pop_2005_MNWI)),
            by = join_by(NAMELSAD == county_name)
  ) %>%
  mutate(
    year = 2005,
    county_population = round(population_2005_censusInterp)
  ) %>%
  group_by(STATE) %>%
  mutate(
    state_population = sum(county_population)
  ) %>%
  ungroup() %>%
  mutate(
    county_proportion_of_state_pop = county_population / state_population,
    population_data_source = "Interpolation of data from Summmary File 1, 2000/2010 Decennial Census based on 2000-2010 Geographic Crosswalk from IPUMS NHGIS, University of Minnesota"
  ) %>%
  select(-population_2005_censusInterp)


#write_sf(crosswalkPop_2000_to_2010,here("_energy", "data-raw", "geoCrosswalk","crosswalkPop_2000_to_2010_blocks_MNWI.shp"))
#write_sf(GEOID10_2000_2005_2010_population_MNWI,here("_energy", "data-raw", "geoCrosswalk","GEOID10_2000_2005_2010_population_MNWI.shp"))
