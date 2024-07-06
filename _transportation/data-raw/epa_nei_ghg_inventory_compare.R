source("_transportation/data-raw/epa_nei_transportation.R")
source("_transportation/data-raw/epa_ghg_inventory.R")

county_proportions <- read_rds("_meta/data/cprg_county_proportions.RDS")

county_proportions_summary <- county_proportions %>% 
  group_by(STATE, year) %>% 
  summarize(region_proportion_of_state_pop = sum(county_proportion_of_state_pop))

state_ipcc_summary <- state_ipcc %>% 
  filter(
    # inventory_year %in% epa_nei$nei_inventory_year,
    Sector == "Mobile Combustion") %>% 
  group_by(state, inventory_year) %>% 
  summarize(state_emissions = sum(emissions_metric_tons_co2e))

state_economic_summary <- state_economic %>% 
  filter(
    # inventory_year %in% epa_nei$nei_inventory_year,
    sector_group == "Transportation") %>% 
  group_by(state, inventory_year) %>% 
  summarize(state_emissions = sum(emissions_metric_tons_co2e))

nei_summary <-  epa_nei %>% 
  mutate(nei_inventory_year = as.character(nei_inventory_year)) %>% 
  left_join(cprg_county %>% 
              mutate(state = STATE) %>% 
              sf::st_drop_geometry(),
            by = c("county_name" = "NAME")) %>% 
  group_by(nei_inventory_year, state) %>% 
  summarize(region_emissions = sum(emissions_metric_tons_co2e),
            .groups = "keep") 


inventory_comp <- state_economic_summary %>% 
  left_join(nei_summary, 
            by = c("state", 
                   "inventory_year" = "nei_inventory_year")) %>% 
  left_join(county_proportions_summary, by= c("inventory_year" = "year",
                                      "state"= "STATE")) %>% 
  # ungroup() %>% 
  mutate(regional_proportion = region_emissions/state_emissions)


state_economic_summary

plot_ly(
  type = "scatter",
  mode = "lines+markers",
  data = inventory_comp,
  x = ~inventory_year,
  y = ~regional_proportion,
  color = ~state
) %>% 
  add_trace(
    type = "scatter",
    mode = "markers",
    data = inventory_comp,
    x = ~inventory_year,
    y = ~region_proportion_of_state_pop
  )


plot_ly(
  type = "scatter",
  mode = "lines+markers",
  data=  state_economic_summary,
  x = ~inventory_year,
  y = ~ state_emissions,
  color = ~state
) %>% 
  add_trace(
    mode = "lines+markers",
    data = nei_summary,
    x = ~nei_inventory_year,
    y = ~region_emissions,
    color = ~state
  )
