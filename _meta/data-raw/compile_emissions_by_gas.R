# compile emissions by gas type from all sectors into a single data table
source("R/_load_pkgs.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
ef_hub <- readRDS("_meta/data/epa_ghg_factor_hub.RDS")

transportation_gas <- readRDS("_transportation/data/epa_onroad_emissions_compile.rds") %>% 
  filter(emissions_year == 2022,
         pollutant_code %in% c("CO2",
                               "CH4",
                               "N2O")) %>% 
  mutate(value_emissions = as.numeric(emissions * units::as_units("gram") %>%
      units::set_units("metric_ton")
  ),
  units_emissions = paste("Metric tons",pollutant_code)) %>% 
  filter(!is.na(value_emissions)) %>% 
  group_by(emissions_year, county_name, units_emissions) %>% 
  summarize(value_emissions = sum(value_emissions)) %>% 
  ungroup() %>% 
  mutate(sector = "Transportation")

## already by gas
agriculture_gas <- readRDS("_agriculture/data/agricultural_emissions_county.rds") %>% 
  filter(inventory_year == 2022) %>% 
  group_by(geoid, inventory_year, units_emissions) %>% 
  summarize(value_emissions = sum(value_emissions)) %>% 
  ungroup() %>% 
  left_join(cprg_county %>% 
              st_drop_geometry() %>% 
              select(geoid, county_name)) %>% 
  select(-geoid) %>% 
  rename(emissions_year = inventory_year) %>% 
  mutate(sector = "Agriculture")

#freshwater methane emissions
ns_gas <- readRDS("_nature/data/nhd_ctu_waterways_emissions_allyrs.RDS") %>% 
  filter(inventory_year == 2022) %>% 
  group_by(county_name, inventory_year, units_emissions) %>% 
  summarize(value_emissions = sum(value_emissions)) %>% 
  ungroup() %>% 
  rename(emissions_year = inventory_year)  %>% 
  mutate(sector = "Natural Systems")


waste_gas <- bind_rows(readRDS("_waste/data/solid_waste_MN_by_gas.RDS"),
                       readRDS("_waste/data/solid_waste_gas_WI_allyrs.RDS"))%>% 
  filter(inventory_year == 2022) %>% 
  group_by(geoid, inventory_year, units_emissions) %>% 
  summarize(value_emissions = sum(value_emissions)) %>% 
  ungroup() %>% 
  left_join(cprg_county %>% 
              st_drop_geometry() %>% 
              select(geoid, county_name)) %>% 
  select(-geoid) %>% 
  rename(emissions_year = inventory_year) %>% 
  mutate(sector = "Waste")

wastewater_gas <- readRDS("_waste/data/final_wastewater_allyrs.RDS")  %>% 
  filter(inventory_year == 2022) %>% 
  group_by(county_name, inventory_year, units_emissions) %>% 
  summarize(value_emissions = sum(value_emissions)) %>% 
  ungroup() %>% 
  rename(emissions_year = inventory_year) %>% 
  mutate(sector = "Waste")

ng_emissions <- ef_hub$stationary_combustion %>% 
  filter(fuel_category == "Natural Gas",
         per_unit == "scf") %>% 
  mutate(gas_type = str_extract(emission, "(CO2|CH4|N2O)$"),
         mt_gas = case_when(
           gas_type != "CO2" ~
           as.numeric(value * units::as_units("gram") %>%
                               units::set_units("metric_ton")),
           gas_type == "CO2" ~
             as.numeric(value * units::as_units("kilogram") %>%
                          units::set_units("metric_ton"))
         ),
         units_emissions = paste("Metric tons",gas_type)
  ) %>% 
  filter(!is.na(gas_type)) %>% 
  select(fuel_category, per_unit, mt_gas, units_emissions)
  

building_gas <- readRDS("_energy/data/county_natgas_activity.RDS") %>% 
  filter(year == 2022) %>% 
  cross_join(ng_emissions) %>% 
  mutate(value_emissions = activity * 1000 * mt_gas) %>%  #converts mcf to scf which is per unit
  select(emissions_year = year, county_name, sector, value_emissions, units_emissions)

elec_emissions <- ef_hub$egridTimeSeries %>% 
  filter(Year == 2022)%>% 
  mutate(gas_type = str_extract(emission, "(CO2|CH4|N2O)$"),
         mt_gas = 
           as.numeric(value * units::as_units("pound") %>%
                        units::set_units("metric_ton")),
         units_emissions = paste("Metric tons",gas_type)
  ) %>% 
  select(per_unit, mt_gas, units_emissions)

building_elec <- readRDS("_energy/data/county_elec_activity.RDS") %>% 
  filter(year == 2022) %>% 
  cross_join(elec_emissions) %>% 
  mutate(value_emissions = activity * mt_gas) %>%
  select(emissions_year = year, county_name, sector, value_emissions, unit_emissions = units_emissions)
  
industrial_fuel_gas_epa <- readRDS("_industrial/data/fuel_combustion_emissions_by_gas.RDS") %>% 
    group_by(reporting_year, county_name, city_name, units_emissions) %>% 
    summarize(value_emissions = sum(values_emissions)) %>% 
    ungroup() %>% 
  filter(reporting_year == 2022,
         units_emissions != "avg_activity") %>% 
  rename(emissions_year = reporting_year)

industrial_fuel_gas_mpca <- readRDS("_industrial/data/mpca_fuel_emissions_by_gas.RDS") %>% 
  group_by(inventory_year, county_name, ctu_name, unit_emissions) %>% 
  summarize(value_emissions = sum(value_emissions)) %>% 
  ungroup() %>% 
  filter(!ctu_name %in% industrial_fuel_gas_epa$city_name)

industrial_process <- readRDS("_industrial/data/fluorinated_gas_emissions.RDS")

