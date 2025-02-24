# Process Xcel Community Reports data
source("R/_load_pkgs.R")
source("_energy/data-raw/_energy_emissions_factors.R")


# root directory with folders for each utility in scope (with each folder containing subfolders for all years which reporting to the state is available)
dir_centerpoint_reports <- here("_energy", "data-raw", "centerpoint_reports")

# ctu and county reference, incl. population -- necessary for disaggregation to COCTU
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_ctu <- readRDS("_meta/data/cprg_ctu.RDS")
ctu_population <- readRDS("_meta/data/ctu_population.RDS") %>%
  filter(inventory_year > 2014) %>%
  left_join(cprg_county %>% select(geoid, county_name, state_abb), by = "geoid") %>%
  filter(state_abb == "MN") %>%
  rename(year = inventory_year)




