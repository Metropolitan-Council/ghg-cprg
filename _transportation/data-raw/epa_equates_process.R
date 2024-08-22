# process epa equates
source("R/_load_pkgs.R")
source("R/global_warming_potential.R")
source("R/download_read_table.R")
source("_transportation/data-raw/epa_source_classification_codes.R")
source("_transportation/data-raw/epa_nei_transportation.R")

nei_vmt <- readRDS("_transportation/data-raw/epa/nei/nei_vmt.RDS")

equates <- read_rds("_transportation/data-raw/epa/air_emissions_modeling/EQUATES/equates_mn_wi.RDS")
equates_cprg <- read_rds("_transportation/data-raw/epa/air_emissions_modeling/EQUATES/equates_cprg.RDS") %>% 
  select(-equates_path) %>% 
  left_join(scc_equates,
            join_by(scc, scc6))


equates_cprg_summary <- equates_cprg %>% 
  group_by(geoid, county_name, cprg_area, 
           poll, calc_year,
  ) %>% 
  summarize(emissions_short_tons = sum(emissions_short_tons),
            .groups = "keep") %>% 
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
  )


epa_nei %>% 
  filter(vehicle_group == "On-Road") %>% 
  rowwise() %>% 
  mutate(
    co2_co2_equivalent =
      sum(total_co2, (total_ch4 * gwp$ch4), na.rm = T),
    emissions_metric_tons_co2e = co2_co2_equivalent / 1000000
  ) %>% 
  ungroup() %>% 
  group_by(geoid, county_name, nei_inventory_year) %>% 
  summarize(total_ch4 = sum(total_ch4),
            total_co2 = sum(total_co2),
            total_n2o = sum(total_n2o),
            total_co2_w_equiv = sum(total_co2_w_equiv),
    emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e)) %>% 
  filter(county_name == "Hennepin") %>% 
  View

# visualize over time -----

equates_cprg %>% 
  filter(poll == "CO2") %>% 
  filter(fuel_type != "Compressed Natural Gas (CNG)",
         scc_level_six != "Brake and Tire Wear") %>% 
ggplot() +
  aes(x = calc_year,
      y = ann_value,
      color = alt_vehicle_type) +
  geom_jitter(
    alpha = 0.5
  )

equates_cprg %>% 
  group_by(geoid, county_name,  calc_year, poll, fuel_type, 
           scc_level_three) %>% 
  summarize(ann_value = sum(ann_value)) %>% 
  filter(geoid == "27053") %>% View

