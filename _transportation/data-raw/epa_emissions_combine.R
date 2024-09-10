# compile EPA emissions datasets and interpolate between them

source("R/_load_pkgs.R")
source("R/global_warming_potential.R")
source("_transportation/data-raw/epa_source_classification_codes.R")
source("_meta/data-raw/county_geography.R")


epa_nei <- readRDS("_transportation/data/epa_nei.RDS")
epa_nei_onroad <- readRDS("_transportation/data-raw/epa/nei_onroad_emissions.RDS") %>% 
  left_join(
    scc_complete_road %>% 
      select(-starts_with("scc_level_"), -scc6_desc)
  ) %>% 
  left_join(scc_equates %>% 
              select(-scc))



epa_equates <- readRDS("_transportation/data-raw/epa/air_emissions_modeling/EQUATES/equates_cprg.RDS") %>% 
  left_join(
    scc_equates,
    join_by(scc, scc6)
  )

epa_emismod <- 
  bind_rows(readRDS("_transportation/data-raw/epa/air_emissions_modeling/onroad_mn_wi_19_22.RDS"),
            readRDS("_transportation/data-raw/epa/air_emissions_modeling/onroad_mn_wi_15_18.RDS"),
            readRDS("_transportation/data-raw/epa/air_emissions_modeling/onroad_mn_wi_11_14.RDS")) %>% 
  mutate(geoid = region_cd) %>%
  left_join(counties_light) %>%
  filter(cprg_area == TRUE) %>% 
  left_join(scc_equates)
  

epa_nei_onroad_summary <- epa_nei_onroad %>% 
  group_by(geoid, county_name, nei_inventory_year, pollutant_code, scc6, scc6_desc, alt_vehicle_type) %>%
  summarize(emissions_tons = sum(total_emissions),
            .groups = "keep") %>%
  mutate(ann_value_grams = emissions_tons %>%
           units::as_units("ton") %>%
           units::set_units("gram") %>%
           as.numeric()) %>%
  select(-emissions_tons) %>%
  pivot_wider(
    names_from = pollutant_code,
    values_from = ann_value_grams
  ) %>%
  clean_names() %>%
  rowwise() %>%
  mutate(
    co2_co2_equivalent =
      sum(co2, (ch4 * gwp$ch4), na.rm = T),
    emissions_metric_tons_co2e = co2_co2_equivalent / 1000000
  ) %>%
  select(nei_inventory_year, geoid, county_name, co2, emissions_metric_tons_co2e, everything())



epa_emismod_summary <- epa_emismod %>% 
  filter(emis_type == "RPD",
         cprg_area == TRUE) %>% 
  select(-equates_path) %>% 
  group_by(geoid, county_name, calc_year, poll, scc6, scc6_desc, alt_vehicle_type) %>%
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
    emissions_metric_tons_co2e = co2_co2_equivalent / 1000000
  ) %>%
  select(calc_year, geoid, county_name, co2, emissions_metric_tons_co2e, everything())
  

# compile EQUATES data  
epa_equates_summary <- epa_equates %>% 
  filter(emis_type == "RPD",
         cprg_area == TRUE) %>% 
  select(-equates_path) %>% 
  group_by(geoid, county_name, calc_year, poll, scc6, scc6_desc, alt_vehicle_type) %>%
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
    emissions_metric_tons_co2e = co2_co2_equivalent / 1000000
  ) %>%
  select(calc_year, geoid, county_name, co2, emissions_metric_tons_co2e, everything())


# combine specific years to get a full time series
epa_emissions_combine <- bind_rows(epa_nei_onroad_summary %>% 
            filter(calc_year == "2020") %>% 
              mutate(source = "NEI"),
          epa_emismod_summary %>% 
            filter(calc_year %in% c("2021", "2022")) %>% 
            mutate(source = "Air Emissions Modeling")
          epa_equates_summary)
