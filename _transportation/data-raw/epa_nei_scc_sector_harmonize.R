# scc sector mappings
source("R/_load_pkgs.R")
source("_transportation/data-raw/epa_source_classification_codes.R")
nei_onroad_emissions <- readRDS("_transportation/data-raw/epa/nei_onroad_emissions.RDS")
nei_nonroad_emissions <- readRDS("_transportation/data-raw/epa/nei_nonroad_emissions.RDS")



# we need to match the NEI sector groupings with individual SCCs
# First, lets get all the SCCs that are observed in the most detailed 
# emissions data for on-road and non-road
nei_scc <- nei_nonroad_emissions %>% 
  bind_rows(nei_onroad_emissions) %>% 
  select(scc, data_category) %>% 
  unique()

# now join our observed SCCs
# with our more complete on_non_road_scc
scc_sector <- nei_scc %>% 
  left_join(scc_complete_road) 

scc_ei_sectors <- scc_sector %>% 
  left_join(mobile_sectors,
            by = c("sector" = "ei_sector"))

scc_sector %>% 
  filter(is.na(sector)) %>% 
  nrow()

scc_forward <- scc_sector %>% 
  filter(scc_new != scc)


scc_forward_combine <- scc_sector %>% 
  select(scc = scc_new, 
         scc6  = scc6_new,
         data_category) %>% 
  unique() %>% 
  left_join(scc_sector %>% 
              select(-map_to))


nonroad_correct <- nei_nonroad_emissions %>% 
  # filter(scc %in% scc_forward$scc) %>% 
  left_join(scc_sector) %>% 
  select(geoid, state_name, county_name, scc, scc_new, scc6, scc6_new, nei_inventory_year, 
         total_emissions, pollutant_code, emissions_uom,
         pollutant_desc, data_set) %>% 
  mutate(scc = ifelse(scc != scc_new, scc_new, scc),
         scc6 = ifelse(scc6 != scc6_new, scc6_new, scc6)) %>% 
  select(-scc_new, -scc6_new) %>% 
  left_join(scc_sector)

# test that all emissions are still accounted for

testthat::expect_identical(
  nei_nonroad_emissions %>% 
    left_join(scc_sector) %>% 
    group_by(geoid, nei_inventory_year, pollutant_code, pollutant_desc) %>% 
    summarise(total_emissions = sum(total_emissions)),
  nonroad_correct %>% 
    left_join(scc_sector) %>% 
    group_by(geoid, nei_inventory_year, pollutant_code, pollutant_desc) %>% 
    summarise(total_emissions = sum(total_emissions))
)


# combine scc level emissions

nei_scc_emissions <- bind_rows(nei_onroad_emissions %>% 
                                 left_join(scc_ei_sectors),
                               nei_nonroad_emissions %>% 
                                 left_join(scc_ei_sectors)) %>% 
  select(-map_to, -scc_new, -scc6_new)


nei_emissions_detailed <- nei_scc_emissions %>% 
  filter(pollutant_code %in% c("CH4", "CO2", "N2O")) %>% 
  mutate(vehicle_type = vehicle_type.y) %>% 
  group_by(geoid, county_name, cprg_area,
           nei_inventory_year, data_category,
           scc6, 
           fuel_type_detect,
           vehicle_fuel_label, vehicle_group, 
           vehicle_type,
           vehicle_weight_label, 
           nei_sector_code,
           pollutant_code
  ) %>% 
  summarize(total_emissions = sum(total_emissions),
            .groups = "keep") %>% 
  ungroup() %>% 
  mutate(emissions_tons = total_emissions %>% 
           units::as_units("ton") %>% 
           units::set_units("gram") %>% 
           as.numeric()) %>%
  select(-total_emissions) %>% 
  pivot_wider(names_from = pollutant_code,
              values_from = emissions_tons) %>% 
  clean_names() %>% 
  rowwise() %>% 
  mutate(
    co2_co2_equivalent =
      sum(co2, (ch4 * gwp$ch4), (n2o * gwp$n2o), na.rm = T),
    emissions_metric_tons_co2e = co2_co2_equivalent / 1000000
  ) %>% 
  left_join(county_geography %>%
              select(geoid,
                     state_name,
                     county_name,
                     county_fips = countyfp
              ))

nei_detailed_summary <- nei_emissions_detailed %>% 
  group_by(
    geoid, county_name,
    vehicle_weight_label,
    vehicle_type,
    vehicle_group,
    vehicle_fuel_label,
    nei_sector_code,
    state_name, cprg_area,
    county_fips,
    nei_inventory_year
  ) %>%
  summarize(
    total_co2 = sum(co2, na.rm = T),
    total_ch4 = sum(ch4, na.rm = T),
    total_n2o = sum(n2o, na.rm = T),
    total_co2_w_equiv = sum(co2_co2_equivalent, na.rm = T),
    emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e, na.rm = T),
    .groups = "keep"
  ) %>% 
  ungroup() %>% 
  arrange(geoid, nei_inventory_year) %>% 
  mutate(nei_inventory_year = as.numeric(nei_inventory_year))


waldo::compare(
  nei_detailed_summary,
  epa_nei %>% 
    arrange(geoid, nei_inventory_year)
)

anti_join(nei_detailed_summary,
          epa_nei,
          by = join_by(geoid, county_name, vehicle_weight_label, vehicle_type, vehicle_group,
                       vehicle_fuel_label, nei_sector_code, state_name, cprg_area, county_fips, nei_inventory_year)) %>% View

nei_emissions_summary <- nei_scc_emissions %>% 
  group_by(geoid, county_name, cprg_area, nei_inventory_year, data_category,
           pollutant_code, pollutant_desc, emissions_uom,
           alt_mode_truck
  ) %>% 
  summarize(total_emissions = sum(total_emissions),
            .groups = "keep")


nei_emissions_summary %>% 
  filter(county_name == "Hennepin",
         pollutant_code == "CO2",
         data_category == "Onroad") %>% 
  plot_ly(
    type = "scatter",
    x = ~nei_inventory_year,
    y = ~total_emissions,
    color = ~alt_mode_truck
  )


# try to summarize based on sectors provided in the NEI API


epa_nei
