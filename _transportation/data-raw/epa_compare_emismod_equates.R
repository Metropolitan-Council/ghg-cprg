# EQUATES and air emis modeling comparison
# ON road
source("R/_load_pkgs.R")
source("R/global_warming_potential.R")
source("_transportation/data-raw/epa_source_classification_codes.R")
source("_meta/data-raw/county_geography.R")


epa_nei <- readRDS("_transportation/data/epa_nei.RDS")


nei_summary <- epa_nei %>%
  filter(vehicle_group == "On-Road") %>%
  group_by(geoid, county_name, nei_inventory_year) %>%
  summarise(emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e)) %>%
  mutate(calc_year = as.character(nei_inventory_year)) %>%
  select(-nei_inventory_year)


equates_cprg <- read_rds("_transportation/data-raw/epa/air_emissions_modeling/EQUATES/equates_cprg.RDS") %>%
  select(-equates_path) %>%
  left_join(
    scc_equates,
    join_by(scc, scc6)
  )


equates_cprg_summary <- equates_cprg %>%
  group_by(geoid, county_name, calc_year, poll) %>%
  summarize(emissions_short_tons = sum(emissions_short_tons)) %>%
  mutate(ann_value_grams = emissions_short_tons %>%
    units::as_units("short_ton") %>%
    units::set_units("gram") %>%
    as.numeric()) %>%
  select(-emissions_short_tons) %>%
  pivot_wider(
    names_from = poll,
    values_from = ann_value_grams
  ) %>%
  clean_names() %>%
  rowwise() %>%
  mutate(
    co2_co2_equivalent =
      sum(co2, (ch4 * gwp$ch4), na.rm = T),
    emissions_metric_tons_co2e = co2_co2_equivalent / 1000000
  ) %>%
  select(calc_year, geoid, county_name, co2, emissions_metric_tons_co2e)


emis_onroad <- read_rds("_transportation/data-raw/epa/air_emissions_modeling/onroad_mn_wi_15_18.RDS") %>%
  bind_rows(read_rds("_transportation/data-raw/epa/air_emissions_modeling/onroad_mn_wi_19_22.RDS")) %>%
  bind_rows(read_rds("_transportation/data-raw/epa/air_emissions_modeling/onroad_mn_wi_11_14.RDS")) %>%
  left_join(scc_equates) %>%
  mutate(geoid = region_cd) %>%
  left_join(counties_light) %>%
  filter(cprg_area == TRUE)

emis_onroad_summary <- emis_onroad %>%
  filter(emis_type == "RPD") %>%
  group_by(geoid, county_name, calc_year, poll) %>%
  summarize(emissions_short_tons = sum(emissions_short_tons)) %>%
  mutate(ann_value_grams = emissions_short_tons %>%
    units::as_units("short_ton") %>%
    units::set_units("gram") %>%
    as.numeric()) %>%
  select(-emissions_short_tons) %>%
  pivot_wider(
    names_from = poll,
    values_from = ann_value_grams
  ) %>%
  clean_names() %>%
  rowwise() %>%
  mutate(
    co2_co2_equivalent =
      sum(co2, (ch4 * gwp$ch4), na.rm = T),
    emissions_metric_tons_co2e = co2_co2_equivalent / 1000000
  ) %>%
  select(calc_year, geoid, county_name, co2, emissions_metric_tons_co2e)


equates_onroad_compare <-
  emis_onroad_summary %>%
  mutate(source = "AirEmis") %>%
  bind_rows(equates_cprg_summary %>%
    mutate(source = "EQUATES")) %>%
  bind_rows(nei_summary %>%
    mutate(source = "NEI")) %>%
  left_join(counties_light) %>%
  filter(cprg_area == TRUE)

equates_onroad_compare %>%
  select(-co2) %>%
  group_by(calc_year, geoid, county_name) %>%
  pivot_wider(
    names_from = source,
    values_from = emissions_metric_tons_co2e
  ) %>%
  filter(
    !is.na(EQUATES),
    !is.na(AirEmis)
  )


equates_onroad_compare %>%
  # 2017 is the only year we have complete data for
  filter(calc_year == "2017") %>%
  ggplot() +
  aes(
    y = reorder(county_name, emissions_metric_tons_co2e),
    x = emissions_metric_tons_co2e,
    colour = source
  ) +
  geom_point(
    size = 3,
    alpha = 0.5
  ) +
  labs(title = "2017 emissions by county")


equates_onroad_compare %>%
  filter(county_name == "Hennepin") %>%
  ggplot() +
  aes(
    x = as.numeric(calc_year),
    y = emissions_metric_tons_co2e,
    color = source,
    group = source
  ) +
  geom_point() +
  geom_line()
