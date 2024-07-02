# uses AR5 GWP values
# https://www.epa.gov/ghgemissions/inventory-us-greenhouse-gas-emissions-and-sinks-1990-2022
source("R/_load_pkgs.R")
clean_raw <- function(state, path){
  read_xlsx(path,
            skip = 2) %>% 
    mutate(sector_group = c(
      "remove",
      rep("Energy", 10),
      "remove",
      rep("Industrial Processes and Product Use", 26),
      "remove",
      rep("Agriculture", 7),
      "remove",
      rep("Waste", 4),
      "remove",
      "remove",
      rep("Land Use, Land-Use Change, and Forestry", 5),
      "remove",
      rep("remove", 10)
    )) %>% 
    filter(sector_group != "remove") %>% 
    select(-...3, -...19) %>% 
    pivot_longer(cols = 2:33,
                 names_to = "inventory_year",
                 values_to = "emissions") %>% 
    mutate(emissions_metric_tons_co2e = emissions * 1000000,
           state = state)
}

state_ipcc <- 
  bind_rows(
    clean_raw(path = "_transportation/data-raw/epa/state_ghg/State-Level-GHG-data/Minnesota.xlsx",
              state = "Minnesota"),
    clean_raw(state = "Wisconsin",
              path = "_transportation/data-raw/epa/state_ghg/State-Level-GHG-data/Wisconsin.xlsx"))
    

state_ipcc %>% 
  group_by(sector_group, inventory_year, state) %>% 
  summarize(emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e)) %>%
  ungroup() %>% 
  plot_ly(
    y= ~inventory_year,
    x  = ~emissions_metric_tons_co2e,
    color = ~sector_group
  )
