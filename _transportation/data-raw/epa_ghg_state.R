# get data from state GHG emissions and removals
# https://www.epa.gov/ghgemissions/state-ghg-emissions-and-removals
source("R/_load_pkgs.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")


# ipcc sectors -----
ipcc_sectors <- readxl::read_xlsx("_transportation/data-raw/epa/state_ghg/allstateghgdatapy2023readme_100323_0/AllStateGHGDataPY2023_100323.xlsx",
  sheet = 2
) %>%
  clean_names()


ipcc <- ipcc_sectors %>%
  filter(
    state %in% c("MN", "WI"),
    category == "Transportation"
  ) %>%
  pivot_longer(starts_with("y"),
    names_to = "year",
    values_to = "value"
  ) %>%
  # reported in millions of metric tons
  mutate(emissions_metric_tons_co2e = value * 1000000)


# economic sectors -----
econ_sectors <- readxl::read_xlsx("_transportation/data-raw/epa/state_ghg/allstateghgdatapy2023readme_100323_0/AllStateGHGDataPY2023_100323.xlsx",
  sheet = 3
) %>%
  clean_names()

econ <- econ_sectors %>%
  filter(state %in% c("MN", "WI")) %>%
  pivot_longer(starts_with("y"),
    names_to = "year",
    values_to = "value"
  ) %>%
  filter(econ_sector == "Transportation") %>%
  # reported in millions of metric tons
  mutate(emissions_metric_tons_co2e = value * 1000000)
