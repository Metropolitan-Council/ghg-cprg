# verify that the NEI summary datasets match their associated SMOKE flat files
# Only year 2020 is consistent. Years 2017, 2014, and 2011 are not consistent.
# The differences are generally relatively small.
# We are only using year 2020 NEI for our regional emissions values, so it is
# the most important year to ensure we have matching.
# 
# All the individual year data are independent files, so differences found in one 
# year do not indicate differences in other years. 
# It is not likely that the underlying processing is causing the differences.
source("R/_load_pkgs.R")
source("_meta/data-raw/county_geography.R")
source("R/global_warming_potential.R")
scc_combine <- read_rds("_transportation/data/scc_combine.RDS")
epa_nei_onroad_regional <- readr::read_rds("_transportation/data-raw/epa/epa_nei_onroad_emissions.RDS")
epa_nei_onroad_smoke <- readRDS("_transportation/data-raw/epa/nei/epa_nei_smoke_ff.RDS")

# compile the onroad regional summary data
onroad_regional <- epa_nei_onroad_regional %>% 
  left_join(counties_light) %>%
  filter(cprg_area == TRUE,
         pollutant_code %in% c("CH4", "CO", "CO2", "N2O", "NH3", "NOX", "PM10-PRI", "PM25-PRI", 
                               "SO2", "VOC")) %>% 
  left_join(scc_combine) %>% 
  group_by(geoid, county_name, nei_inventory_year, pollutant_code, scc6, scc6_desc) %>%
  summarize(emissions_short_tons = sum(total_emissions),
            .groups = "keep") %>%
  mutate(ann_value_grams = emissions_short_tons %>%
           units::as_units("short_ton") %>%
           units::set_units("gram") %>%
           as.numeric()) %>%
  select(-emissions_short_tons) %>%
  pivot_wider(
    names_from = pollutant_code,
    values_from = ann_value_grams
  ) %>%
  clean_names() %>%
  rowwise() %>%
  mutate(
    co2_co2_equivalent =
      sum(co2, (ch4 * gwp$ch4), na.rm = T),
    emissions_metric_tons_co2e = co2_co2_equivalent / 1000000,
    
    co2_co2_n2o_equivalent =
      sum(co2, (ch4 * gwp$ch4), (n2o * gwp$n2o), na.rm = T),
    emissions_metric_tons_co2e_n2o = co2_co2_n2o_equivalent / 1000000
  ) %>%
  select(nei_inventory_year, geoid, county_name,
         emissions_metric_tons_co2e, emissions_metric_tons_co2e_n2o,
         everything())


onroad_smoke <- epa_nei_onroad_smoke %>% 
  mutate(geoid = region_cd,
         pollutant_code = poll) %>%
  left_join(counties_light) %>%
  filter(cprg_area == TRUE,
         emis_type %in% c("RPD", ""),
         poll %in% c("CH4", "CO", "CO2", "N2O", "NH3", "NOX", "PM10-PRI", "PM25-PRI", 
                      "SO2", "VOC")) %>% 
  left_join(scc_combine) %>% 
  group_by(geoid, county_name, calc_year, poll, scc6, scc6_desc) %>%
  summarize(emissions_short_tons = sum(emissions_short_tons),
            .groups = "keep") %>%
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
    emissions_metric_tons_co2e = co2_co2_equivalent / 1000000,
    
    co2_co2_n2o_equivalent =
      sum(co2, (ch4 * gwp$ch4), (n2o * gwp$n2o), na.rm = T),
    emissions_metric_tons_co2e_n2o = co2_co2_n2o_equivalent / 1000000
  ) %>%
  select(calc_year, geoid, county_name,
         emissions_metric_tons_co2e, emissions_metric_tons_co2e_n2o,
         everything()) %>% 
  filter(calc_year %in% onroad_regional$nei_inventory_year)


onroad_regional %>% 
  select(geoid, county_name, calc_year = nei_inventory_year,emissions_metric_tons_co2e,
         scc6, scc6_desc) %>% 
  mutate(data_source = "Regional") %>% 
  bind_rows(onroad_smoke %>% 
              select(geoid, calc_year, emissions_metric_tons_co2e, scc6, scc6_desc) %>% 
              mutate(data_source = "SMOKE")) %>% 
  group_by(geoid, county_name, calc_year, scc6, scc6_desc) %>% 
  pivot_wider(names_from = data_source,
              values_from = emissions_metric_tons_co2e) %>% 
  mutate(diff = SMOKE - Regional,
         pct_diff = diff / Regional) %>% 
  arrange(pct_diff) %>% 
  # filter(calc_year == 2020) %>%
  View
# 2020 is correct
# 2017 is off
# 2014 is off
# 2011 doesn't have any CO2  



