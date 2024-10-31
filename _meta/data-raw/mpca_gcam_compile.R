source("R/_load_pkgs.R")

demographer_state_population <- readRDS("_meta/data/state_population_demographer.RDS")

state_projections <- read_xlsx("_meta/data-raw/Minnesota GCAM Modeling Data.xlsx") %>% 
  pivot_longer(cols = 6:27,
               names_to = "emissions_year",
               values_to = "values_emissions") %>% 
  clean_names() %>% 
  # sum different GHG gases
  group_by(scenario, region, sector, source, units,emissions_year) %>% 
  summarize(values_emissions = sum(values_emissions)) %>% 
  ungroup()

scenarios_annual <- state_projections %>% 
  filter(sector != "Offsets Needed") %>% 
  mutate(source_sink = if_else(values_emissions < 0, "Sequestration","Emission")) %>% 
  group_by(emissions_year, scenario, source_sink, units) %>% 
  summarize(values_emissions = sum(values_emissions))
  
#### homogenize subsectors to match MC inventory work

unique(state_projections$source)

livestock <- c("American Bison", "Beef Cattle","Dairy Cattle","Dairy Heifers",
               "Goats", "Horses", "Swine", "Poultry", "Sheep")

cropland <- c("Aboveground")

state_projections <- state_projections %>% 
  mutate(subsector_mc = case_when(
    source %in% c("American Bison",
                  "Beef Cattle",
                  )
  ))
