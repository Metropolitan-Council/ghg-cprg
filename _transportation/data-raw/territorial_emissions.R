# compile territorial, historical emissions for the 7-county metro
source("R/_load_pkgs.R")
library(imputeTS)
# read in pre-processed proportions
dot_vmt_county_proportions <- readRDS("_transportation/data/dot_vmt_county_proportions.RDS")
cprg_county_proportions <- readRDS("_meta/data/cprg_county_proportions.RDS")
epa_nei_county_proportions <- readRDS("_transportation/data/epa_nei_county_proportions.RDS")




county_vmt_pop_emissions <- cprg_county_proportions %>% 
  left_join(dot_vmt_county_proportions,
            by = c("GEOID", "year",
                   "name" = "county",
                   "STATE" = "state"
            )) %>% 
  left_join(epa_nei_county_proportions %>% 
              mutate(nei_inventory_year = as.character(nei_inventory_year)),
            by = c("year" = "nei_inventory_year",
                   # "GEOID",
                   "name" = "county_name",
                   "STATE" = "state_name")) %>% 
  mutate(
    GEOID = GEOID.x,
    vmt_per_capita = annual_vmt/county_population,
    emissions_per_capita = county_emissions_metric_tons_co2e/county_population
  ) %>% 
  select(-GEOID.y) %>% 
  filter(year >= 2005)


county_prop_long <- county_vmt_pop_emissions %>% 
  select(GEOID, name, year, county_proportion_annual_vmt, 
         county_proportion_of_state_pop, 
         county_proportion_emissions) %>% 
  pivot_longer(4:6,
               names_to = "proportion_base",
               values_to = "value") %>% 
  mutate(proportion_base = case_when(
    proportion_base == "county_proportion_annual_vmt" ~ "VMT",
    proportion_base == "county_proportion_of_state_pop" ~ "population",
    proportion_base == "county_proportion_emissions" ~ "emissions"
  ))





county_prop_long %>% 
  group_by(name) %>% 
  plot_ly(
    type = "scatter",
    mode = "lines+markers",
    x = ~year,
    y = ~value,
    symbol = ~proportion_base,
    color = ~name)

county_vmt_pop_emissions %>% 
  group_by(name) %>% 
  filter(!is.na(emissions_per_capita)) %>% 
  plot_ly(
    mode = "lines+markers",
    type = "scatter",
    x = ~year,
    y = ~emissions_per_capita,
    color = ~name
  )

# impute using imputeTS
# basic interpolation between years, grouped by county name

county_interp <- county_vmt_pop_emissions %>% 
  group_by(name) %>% 
  arrange(desc(year)) %>% 
  mutate(emis_interp = imputeTS::na_kalman(
    county_emissions_metric_tons_co2e,
    smooth = TRUE,
    type = "trend"),
    is_interp = ifelse(is.na(county_emissions_metric_tons_co2e), 
                       TRUE, FALSE),
    emissions_per_capita_interp = emis_interp/county_population,
    emissions_proportion_interp = emis_interp/state_emissions_metric_tons_co2e)


county_interp %>% 
  plot_ly(
    type = "scatter",
    mode = "lines",
    x = ~year,
    y = ~emis_interp,
    color = ~name
  ) %>% 
  add_trace(
    mode = "markers",
    y = ~county_emissions_metric_tons_co2e,
    color = ~name
  )
  
county_interp %>% 
  plot_ly(
    type = "scatter",
    mode = "lines",
    x = ~year,
    y = ~emissions_per_capita_interp,
    color = ~name
  )  


county_interp %>% 
  ggplot() +
  aes(x = year,
      y = emis_interp,
      shape = is_interp,
      color = name,
      group = name) +
  geom_point(size =2) +
  geom_line() +
  theme_minimal()


