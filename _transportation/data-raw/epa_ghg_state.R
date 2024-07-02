# get data from state GHG emissions and removals
# https://www.epa.gov/ghgemissions/state-ghg-emissions-and-removals
source("R/_load_pkgs.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")


# ipcc sectors -----
ipcc_sectors <- readxl::read_xlsx("_transportation/data-raw/epa/state_ghg/allstateghgdatapy2023readme_100323_0/AllStateGHGDataPY2023_100323.xlsx",
                                  sheet = 2
) %>%
  clean_names()

ipcc_sector_category <- ipcc_sectors %>% 
  select(sector, subsector, category, subcategory1, subcategory2, 
         subcategory3, subcategory4) %>% 
  unique()

ipcc <- ipcc_sectors %>%
  filter(
    state %in% c("MN", "WI")
    # category == "Transportation"
  ) %>%
  pivot_longer(starts_with("y"),
               names_to = "year",
               values_to = "value"
  ) %>%
  # reported in millions of metric tons
  mutate(emissions_metric_tons = value * 1000000) %>% 
  # select(-subcategory2, -subcategory3, -subcategory4) %>% 
  mutate(
    gwp_factor = case_when(ghg == "CH4" ~ gwp$ch4,
                           ghg == "CO2" ~ gwp$co2,
                           ghg == "N2O" ~ gwp$n2o,
                           ghg == "HFC-152a" ~ gwp$`HFC-152a`
    ),
    ghg_equiv = emissions_metric_tons * gwp_factor,
    inventory_year = stringr::str_remove(year, "y"))


ipcc %>% 
  group_by(inventory_year, state, sector, category, subsector, subcategory1,subcategory2, 
           subcategory3, subcategory4, fuel) %>% 
  summarize(emissions_metric_tons_co2e = sum(ghg_equiv))


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
  mutate(emissions_metric_tons = value * 1000000)
