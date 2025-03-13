# Process and clean sector-level data for municipal utilities
source("R/_load_pkgs.R")

raw_7610_elecMunis <- readRDS(here("_energy", "data", "combined_MNelecMunis_elecByClass_raw7610.RDS")) %>%
  filter(!utility %in% c("Elk River Municipal Utilities","New Prague Utilities Commission")) %>%
  mutate(
    sector_mapped = case_when(
      grepl("(?i)non[- ]?farm[- ]?residential", sector) ~ "residential",
      grepl("(?i)commercial", sector) ~ "commercial",
      grepl("(?i)industrial", sector) ~ "industrial",
      grepl("(?i)street.*highway.*lighting", sector) ~ "commercial",
      grepl("(?i)all other", sector) ~ "residential", # strongest assumption, includes "residential with space heating", "irrigation" and "other (includes municipals)"
      TRUE ~ NA_character_) # Assign NA for unknown categories
  ) %>%
  filter(is.na(sector_mapped)) # functionally removes the "Entered Total" records only

# ctu and county reference, incl. population -- necessary for disaggregation to COCTU
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_ctu <- readRDS("_meta/data/cprg_ctu.RDS")
ctu_population <- readRDS("_meta/data/ctu_population.RDS") %>%
  filter(inventory_year > 2014) %>%
  left_join(cprg_county %>% select(geoid, county_name, state_abb), by = "geoid") %>%
  filter(state_abb == "MN") %>%
  rename(year = inventory_year)

# Calculate unique total population by individual COCTU unit, as well as CTU (across all counties), and finally across ALL units (city AND township)
ctu_total_population <- ctu_population %>%
  distinct(ctu_name, ctu_class, year, county_name, ctu_population) %>% # Ensure unique rows per city-county-year
  group_by(ctu_name, ctu_class, year) %>%
  mutate(
    total_ctu_population = sum(ctu_population,
                               na.rm = TRUE), # Sum populations across counties for each city-year
    multi_county = n_distinct(county_name) > 1
  ) %>%
  ungroup() %>%
  group_by(ctu_name, year) %>%
  mutate(total_population_across_all_units = sum(ctu_population,
                                                 na.rm = TRUE),
         same_named = n_distinct(ctu_class) > 1) %>%
  ungroup()
