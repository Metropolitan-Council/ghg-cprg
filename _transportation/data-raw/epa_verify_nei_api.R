source("R/_load_pkgs.R")
source("_meta/data-raw/county_geography.R")
source("R/global_warming_potential.R")
scc_combine <- readRDS("_transportation/data/scc_combine.RDS")

epa_nei_envirofacts <- readRDS("_transportation/data/epa_nei_envirofacts.RDS")
epa_nei_onroad <- readRDS("_transportation/data-raw/epa/epa_nei_onroad_emissions.RDS")

# how do the NEI summary values from the EnviroFacts API compare with
# the emissions values pulled from the regional summary files?

envirofacts_summary <- epa_nei_envirofacts %>% 
  # filter(nei_inventory_year == 2020) %>% 
  group_by(geoid, county_name, sector_two, nei_inventory_year) %>% 
  summarize(emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e)) %>% 
  ungroup()

onroad_summary <- epa_nei_onroad %>% 
  left_join(scc_combine) %>% 
  # filter(nei_inventory_year == 2020) %>% 
  group_by(geoid, county_name, nei_inventory_year,
           pollutant_code) %>%
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
      sum(co2, (ch4 * gwp$ch4), (n2o * gwp$n2o), na.rm = T),
    emissions_metric_tons_co2e = co2_co2_equivalent / 1000000
  ) %>%
  ungroup() %>% 
  select(geoid, county_name, 
         nei_inventory_year,
         emissions_metric_tons_co2e)


# the maximum difference I am getting between these two 
# is about 0.7%
envirofacts_summary  %>% 
  filter(sector_two == "On-Road") %>% 
  select(-sector_two) %>% 
  mutate(data_source = "EnviroFacts",
         nei_inventory_year = as.character(nei_inventory_year)) %>% 
  bind_rows(
    onroad_summary %>% 
      mutate(data_source = "Other")
  ) %>% 
  pivot_wider(names_from = data_source,
              values_from = emissions_metric_tons_co2e) %>% 
  mutate(diff = EnviroFacts - Other,
         pct_diff = round(diff/Other, 4)) %>% 
  View
              
