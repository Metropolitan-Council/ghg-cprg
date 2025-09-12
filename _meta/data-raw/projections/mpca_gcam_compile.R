source("R/_load_pkgs.R")

demographer_state_population <- readRDS("_meta/data/state_population_demographer.RDS")

# for comparison
county_emissions <- readRDS("_meta/data/cprg_county_emissions.RDS")

gcam_projections <- read_xlsx("_meta/data-raw/gcam_all_scenarios.xlsx",
                               sheet = "Emissions by Inventory Category") %>%
  pivot_longer(
    cols = 6:27,
    names_to = "emissions_year",
    values_to = "value_emissions"
  ) %>%
  clean_names() %>%
  # sum different GHG gases
  group_by(scenario, region, sector, source, units, emissions_year) %>%
  summarize(value_emissions = sum(value_emissions)) %>%
  ungroup()

ag <- read_xlsx("_meta/data-raw/MN_ag_lulucf.xlsx") %>%
  pivot_longer(
    cols = 8:68,
    names_to = "emissions_year",
    values_to = "value_emissions"
  ) %>%
  clean_names() %>% 
  filter(sector == "Agriculture")

state_projections <- bind_rows(gcam_projections %>% 
                                 filter(sector != "Agriculture"),
                               ag %>% 
                                 select(sector,
                                        scenario,
                                        source,
                                        units = ghg,
                                        emissions_year,
                                        value_emissions))

scenarios_annual <- state_projections %>%
  filter(sector != "Emission Reductions Needed") %>%
  mutate(source_sink = if_else(value_emissions < 0, "Sequestration", "Emission")) %>%
  group_by(emissions_year, scenario, source_sink, units) %>%
  summarize(value_emissions = sum(value_emissions)) %>%
  #### add proportion relative to 2005
  group_by(scenario, source_sink, units) %>%
  mutate(value_2005 = value_emissions[emissions_year == 2005]) %>%
  # Calculate the proportion
  mutate(proportion_of_2005 = value_emissions / value_2005) %>%
  ungroup() %>%
  select(-value_2005) %>%
  rename(units_emission = units)

scenarios_annual_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "emissions_year", class(scenarios_annual$emissions_year), "Year of emissions - inventoried or projected",
    "scenario", class(scenarios_annual$scenario), "Modeled scenario - MN GCAM",
    "source_sink", class(scenarios_annual$source_sink), "Emissions or sequestration activities",
    "units_emission", class(scenarios_annual$units_emission), "Units of emissions or sequestration",
    "value_emissions", class(scenarios_annual$value_emissions), "Value of emissions or sequestration",
    "proportion_of_2005", class(scenarios_annual$proportion_of_2005), "Proportion of emissions or sequestration relative to 2005 baseline"
  )

saveRDS(scenarios_annual, "_meta/data/gcam/mpca_economy_wide_gcam.RDS")
saveRDS(scenarios_annual_meta, "_meta/data/gcam/mpca_economy_wide_gcam_meta.RDS")


scenarios_sector_annual <- state_projections %>%
  filter(sector != "Emission Reductions Needed") %>%
  mutate(source_sink = if_else(value_emissions < 0, "Sequestration", "Emission")) %>%
  group_by(emissions_year, scenario, source_sink, sector, units) %>%
  summarize(value_emissions = sum(value_emissions)) %>%
  #### add proportion relative to 2005
  group_by(scenario, source_sink, sector, units) %>%
  mutate(value_2005 = value_emissions[emissions_year == 2005]) %>%
  # Calculate the proportion
  mutate(proportion_of_2005 = value_emissions / value_2005) %>%
  ungroup() %>%
  select(-value_2005) %>%
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

# #agriculture is modeled separately now.
# state_projections %>%
#   filter(sector == "Agriculture") %>%
#   distinct(source) %>%
#   print(n = 50)
# 
# 
# # only labeling categories where we have current emissions
# livestock <- c(
#   "American Bison", "Beef Cattle", "Dairy Cattle", "Dairy Heifers",
#   "Goats", "Horses", "Swine", "Poultry", "Sheep", "Mules and Asses"
# )
# 
# cropland <- c("Biofuels", "Cereals", "Cropland") # omitting liming, mineral soils, grasslands (emitters?), rice, as these are not accounted for in our inv


state_projections %>%
  filter(sector == "Transportation") %>%
  distinct(source) %>%
  print(n = 50)

county_emissions %>%
  filter(sector == "Transportation") %>%
  distinct(category) %>%
  print(n = 50)

passenger_vehicles <- c("Light-duty trucks", "Motorcycle", "Passenger cars")


state_projections %>%
  filter(sector == "Waste") %>%
  distinct(source) %>%
  print(n = 50)

county_emissions %>%
  filter(sector == "Waste") %>%
  distinct(source) %>%
  print(n = 50)

state_projections %>%
  filter(sector == "Residential") %>%
  distinct(source) %>%
  print(n = 50)

county_emissions %>%
  filter(sector == "Residential") %>%
  distinct(source) %>%
  print(n = 50)

state_projections %>%
  filter(sector == "Commercial") %>%
  distinct(source) %>%
  print(n = 50)

county_emissions %>%
  filter(sector == "Commercial") %>%
  distinct(source) %>%
  print(n = 50)

state_projections %>%
  filter(sector == "Industrial") %>%
  distinct(source) %>%
  print(n = 50)

county_emissions %>%
  filter(sector == "Industrial") %>%
  distinct(source) %>%
  print(n = 50)

state_projections %>%
  filter(sector == "Agriculture") %>%
  distinct(source) %>%
  print(n = 50)

county_emissions %>%
  filter(sector == "Agriculture") %>%
  distinct(source) %>%
  print(n = 50)

state_projections <- state_projections %>%
  mutate(subsector_mc = case_when(
    # # agriculture
    # source %in% livestock ~ "Livestock",
    # source %in% cropland ~ "Cropland",
    # transportation
    source %in% passenger_vehicles ~ "Passenger vehicles",
    source == "Heavy-duty trucks" ~ "Trucks",
    source == "Bus" ~ "Buses",
    source == "Aviation" ~ "Aviation",
    # residential
    sector == "Residential" & source == "Natural gas" ~ "Residential natural gas",
    # commercial
    sector == "Commercial" & source == "Natural gas" ~ "Commercial natural gas",
    sector == "Commercial" & source %in% c(
      "Biofuel",
      "Coal",
      "Refined Liquids"
    ) ~ "Commercial fuel combustion",
    # industrial - recoding as processes are too finicky
    sector == "Industrial" & source == "Natural gas" ~ "Industrial natural gas",
    sector == "Industrial" & source %in% c("Oil refining") ~ "Refinery processes",
    sector == "Industrial" & !source %in% c("Natural gas", "Oil refining") ~ "Industrial processes",
    
    #agriculture
    sector == "Agriculture" & source == "Enteric Fermentation" ~ "Enteric fermentation",
    sector == "Agriculture" & source == "Manure Management" ~ "Manure management",
    sector == "Agriculture" & source== "N2O from Ag Soil Management" ~ "Cropland soil",
    # waste
    source %in% c("Landfills","RDF", "Incineration") ~ "Solid waste",
    source == "Wastewater treatment" ~ "Wastewater",
    # electricity
    sector == "Electricity" ~ "Electricity",
    # # natural systems
    # source %in% sequestration & sector != "Agriculture" ~ "Sequestration",
    # source == "Urban Trees" ~ "Urban tree",
    TRUE ~ "not_inventoried"
  )) %>%
  mutate(subsector_mc = case_when(
    subsector_mc == "not_inventoried" & value_emissions < 0 ~ "sequestration_not_inventoried",
    subsector_mc == "not_inventoried" & value_emissions >= 0 ~ "emissions_not_inventoried",
    TRUE ~ subsector_mc
  ))

county_emissions %>%
  distinct(sector, category, source) %>%
  print(n = 50)

scenarios_sources_annual <- state_projections %>%
  filter(sector != "Emission Reductions Needed") %>%
  mutate(source_sink = if_else(value_emissions < 0, "Sequestration", "Emission")) %>%
  group_by(emissions_year, scenario, source_sink, sector, subsector_mc, units) %>%
  summarize(value_emissions = sum(value_emissions)) %>%
  #### add proportion relative to 2005
  group_by(scenario, subsector_mc, source_sink, sector, units) %>%
  mutate(
    value_2005 = value_emissions[emissions_year == 2005],
    value_2020 = value_emissions[emissions_year == 2020]
  ) %>%
  # Calculate the proportion
  mutate(
    proportion_of_2005 = value_emissions / value_2005,
    proportion_of_2020 = value_emissions / value_2020
  ) %>%
  ungroup() %>%
  select(-value_2005) %>%
  rename(units_emission = units)

scenarios_sources_annual_meta <-
  bind_rows(
    scenarios_annual_meta,
    tibble::tribble(
      ~"Column", ~"Class", ~"Description",
      "subsector_mc", class(scenarios_sources_annual$subsector_mc), "Subsector match for Met Council labels",
    )
  )

saveRDS(scenarios_sources_annual, "_meta/data/gcam/mpca_subsector_gcam.RDS")
saveRDS(scenarios_sources_annual_meta, "_meta/data/gcam/mpca_subsector_gcam_meta.RDS")
