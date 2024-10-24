# Quick and dirty compilation for the RDG 90% draft
source("R/_load_pkgs.R")
# read in pre-processed proportions
epa_nei_county_proportions <- readRDS("_transportation/data/epa_nei_envirofacts_county_proportions.RDS")
epa_nei <- readRDS("_transportation/data/epa_nei_envirofacts.RDS")
cprg_county_emission <- readRDS("_meta/data/cprg_county_emissions.RDS")

cprg_county_emission %>%
  group_by(year, sector) %>%
  summarize(emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e))

transport_baseline_current <- epa_nei_county_proportions %>%
  ungroup() %>%
  filter(
    nei_inventory_year %in% c(2008, 2020)
  ) %>%
  select(nei_inventory_year, county_name, county_emissions_metric_tons_co2e) %>%
  group_by(
    nei_inventory_year,
    county_name
  ) %>%
  summarize(
    emissions_metric_tons_co2e = sum(county_emissions_metric_tons_co2e),
    .groups = "keep"
  ) %>%
  mutate(year = ifelse(nei_inventory_year == 2008, 2005, 2021))


saveRDS(transport_baseline_current, "_transportation/data/county_emissions_RDG.RDS")


# with subsectors

transport_baseline_subsectors <- epa_nei %>%
  ungroup() %>%
  filter(
    nei_inventory_year %in% c(2008, 2020)
  ) %>%
  select(
    nei_inventory_year, vehicle_weight_label,
    county_name, emissions_metric_tons_co2e
  ) %>%
  group_by(
    nei_inventory_year,
    county_name,
    vehicle_weight_label
  ) %>%
  summarize(
    emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e),
    .groups = "keep"
  ) %>%
  mutate(year = ifelse(nei_inventory_year == 2008, 2005, 2021))

saveRDS(transport_baseline_subsectors, "_transportation/data/county_emissions_subsectors.RDG.RDS")
