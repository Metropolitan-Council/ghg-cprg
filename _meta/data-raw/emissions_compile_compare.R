# compile and compare emissions from various projects and sources
source("R/_load_pkgs.R")

cprg_county_emissions <- readRDS("_meta/data/cprg_county_emissions.RDS")
inventory_2018 <- readRDS("_meta/data/inventory_2018.RDS")
nei_county_emissions <- readRDS("_transportation/data/epa_nei_envirofacts.RDS")

cprg_county_emissions %>%
  group_by(year, sector) %>%
  summarize(
    emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e)
  )

inventory_2018 %>%
  group_by(Year, sector, units_Emissions) %>%
  summarize(value_Emissions = sum(value_Emissions, na.rm = T))

nei_county_emissions %>%
  group_by(nei_inventory_year) %>%
  summarise(emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e))
