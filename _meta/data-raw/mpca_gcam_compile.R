source("R/_load_pkgs.R")

demographer_state_population <- readRDS("_meta/data/state_population_demographer.RDS")

#for comparison
county_emissions <- readRDS("_meta/data/cprg_county_emissions.RDS")

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
  summarize(values_emissions = sum(values_emissions)) %>% 
  #### add proportion relative to 2005
  group_by(scenario, source_sink, units) %>% 
  mutate(value_2005 = values_emissions[emissions_year == 2005]) %>% 
  # Calculate the proportion
  mutate(proportion_of_2005 = values_emissions / value_2005) %>% 
  ungroup() %>% select(-value_2005) %>% 
  rename(units_emission = units)

scenarios_annual_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "emissions_year", class(scenarios_annual$emissions_year), "Year of emissions - inventoried or projected",
    "scenario", class(scenarios_annual$scenario), "Modeled scenario - MN GCAM",
    "source_sink", class(scenarios_annual$source_sink), "Emissions or sequestration activities",
    "units_emission", class(scenarios_annual$units_emission), "Units of emissions or sequestration",
    "values_emissions", class(scenarios_annual$values_emissions), "Value of emissions or sequestration",
    "proportion_of_2005", class(scenarios_annual$proportion_of_2005), "Proportion of emissions or sequestration relative to 2005 baseline"
  )

saveRDS(scenarios_annual, "_meta/data/gcam/mpca_economy_wide_gcam.RDS")
saveRDS(scenarios_annual_meta, "_meta/data/gcam/mpca_economy_wide_gcam_meta.RDS")


scenarios_sector_annual <- state_projections %>% 
  filter(sector != "Offsets Needed") %>% 
  mutate(source_sink = if_else(values_emissions < 0, "Sequestration","Emission")) %>% 
  group_by(emissions_year, scenario, source_sink, sector, units) %>% 
  summarize(values_emissions = sum(values_emissions)) %>% 
  #### add proportion relative to 2005
  group_by(scenario, source_sink,sector, units) %>% 
  mutate(value_2005 = values_emissions[emissions_year == 2005]) %>% 
  # Calculate the proportion
  mutate(proportion_of_2005 = values_emissions / value_2005) %>% 
  ungroup() %>% select(-value_2005) %>% 
  rename(units_emission = units)

scenarios_sector_annual_meta <-
  bind_rows(
    scenarios_annual_meta,
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "sector", class(scenarios_sector_annual$sector), "Sector of emission of sequestration",
    )
  )

saveRDS(scenarios_sector_annual, "_meta/data/gcam/mpca_sector_gcam.RDS")
saveRDS(scenarios_sector_annual_meta, "_meta/data/gcam/mpca_sector_gcam_meta.RDS")
  
#### homogenize subsectors to match MC inventory work

state_projections %>% filter(sector == "Agriculture") %>% distinct(source) %>% 
  print(n = 50)


#only labeling categories where we have current emissions
livestock <- c("American Bison", "Beef Cattle","Dairy Cattle","Dairy Heifers",
               "Goats", "Horses", "Swine", "Poultry", "Sheep", "Mules and Asses")

cropland <- c("Biofuels","Cereals","Cropland") # omitting liming, mineral soils, grasslands (emitters?), rice, as these are not accounted for in our inv
              

state_projections %>% filter(sector == "Transportation") %>% distinct(source) %>% 
  print(n = 50)

county_emissions %>% filter(sector == "Transportation") %>% distinct(category) %>% 
  print(n = 50)

passenger_vehicles <- c("Light-duty trucks", "Motorcycles", "Passenger cars")

state_projections %>% filter(sector == "Waste") %>% distinct(source) %>% 
  print(n = 50)

county_emissions %>% filter(sector == "Energy") %>% distinct(category) %>% 
  print(n = 50)

waste <- c("MMSW landfills", "MMSW composting", "MMSW compost preprocessing",
        "Yard waste composting", "Yard waste preprocessing")

state_projections %>% filter(sector == "LULUCF", values_emissions < 0) %>% distinct(source) %>% 
  print(n = 50)

sequestration <- c("Aboveground Biomass", "Belowground Biomass",
                   "Dead Wood", "Litter", "Mineral Soil")


state_projections <- state_projections %>% 
  mutate(subsector_mc = case_when(
    #agriculture
    source %in% livestock ~ "livestock",
    source %in% cropland ~ "cropland",
    #transportation
    source %in% passenger_vehicles ~"passenger_vehicles",
    source == "Heavy-duty trucks" ~ "trucks",
    source == "Bus" ~ "buses",
    source == "Aviation" ~ "aviation",
    sector == "Commercial" & source = "Natural gas" ~ "Commercial natural gas",
    sector == "Industrial" & source = "Natural gas" ~ "Industrial natural gas",
    sector == "Residential" & source = "Natural gas" ~ "Residential natural gas",
    source %in% waste ~ "Solid Waste",
    source == "Wastewater" ~ "wastewater",
    sector == "Electricity" ~ "electricity",
    source %in% c("Sequestration") ~ "natural_systems",
    source == "Urban Trees" ~ "urban_trees"
    TRUE ~ "Not inventoried"
  ))
