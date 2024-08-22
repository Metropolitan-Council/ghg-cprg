# EQUATES and air emis modeling comparison 
# ON road
source("R/_load_pkgs.R")
source("R/global_warming_potential.R")
source("_transportation/data-raw/epa_source_classification_codes.R")
source("_meta/data-raw/county_geography.R")

equates_cprg <- read_rds("_transportation/data-raw/epa/air_emissions_modeling/EQUATES/equates_cprg.RDS") %>% 
  select(-equates_path) %>% 
  left_join(scc_equates,
            join_by(scc, scc6))

emis_onroad <- read_rds("_transportation/data-raw/epa/air_emissions_modeling/onroad_mn_wi_15_28.RDS") %>% 
  bind_rows(read_rds("_transportation/data-raw/epa/air_emissions_modeling/onroad_mn_wi_19_22.RDS")) %>% 
  left_join(scc_equates) %>% 
  mutate(geoid = region_cd) %>% 
  left_join(counties_light)


equates_cprg_summary <- equates_cprg %>% 
  group_by(geoid, county_name, calc_year, poll) %>% 
  summarize(emissions_short_tons = sum(emissions_short_tons)) %>% 
  mutate(ann_value_grams = emissions_short_tons %>% 
           units::as_units("short_ton") %>% 
           units::set_units("gram") %>% 
           as.numeric) %>% 
  select(-emissions_short_tons) %>% 
  pivot_wider(names_from = poll,
              values_from = ann_value_grams) %>% 
  clean_names() %>% 
  rowwise() %>% 
  mutate(
    co2_co2_equivalent =
      sum(co2, (ch4 * gwp$ch4), na.rm = T),
    emissions_metric_tons_co2e = co2_co2_equivalent / 1000000
  ) %>% 
  select(calc_year, geoid, county_name, co2, emissions_metric_tons_co2e) %>% 
  filter(county_name == "Hennepin")


emis_onroad_summary <- emis_onroad %>% 
  group_by(geoid, county_name, calc_year, poll) %>% 
  summarize(emissions_short_tons = sum(emissions_short_tons)) %>% 
  mutate(ann_value_grams = emissions_short_tons %>% 
           units::as_units("short_ton") %>% 
           units::set_units("gram") %>% 
           as.numeric) %>% 
  select(-emissions_short_tons) %>% 
  pivot_wider(names_from = poll,
              values_from = ann_value_grams) %>% 
  clean_names() %>% 
  rowwise() %>% 
  mutate(
    co2_co2_equivalent =
      sum(co2, (ch4 * gwp$ch4), na.rm = T),
    emissions_metric_tons_co2e = co2_co2_equivalent / 1000000
  ) %>% 
  select(calc_year, geoid, county_name, co2, emissions_metric_tons_co2e) %>% 
  filter(county_name == "Hennepin")
  
