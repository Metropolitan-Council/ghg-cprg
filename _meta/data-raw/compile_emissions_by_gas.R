# compile emissions by gas type from all sectors into a single data table
source("R/_load_pkgs.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
ef_hub <- readRDS("_meta/data/epa_ghg_factor_hub.RDS")

transportation_gas <- readRDS("_transportation/data/epa_onroad_emissions_compile.rds") %>% 
  filter(emissions_year == 2022,
         pollutant_code %in% c("CO2",
                               "CH4",
                               "N2O")) %>% 
  mutate(mt_gas = as.numeric(emissions * units::as_units("gram") %>%
      units::set_units("metric_ton")
  )) %>% 
  select(emissions_year, county_name, fuel_type, vehicle_type, pollutant_code, mt_gas)

## already by gas
agriculture_gas <- readRDS("_agriculture/data/agricultural_emissions_county.rds")

#freshwater methane emissions
ns_gas <- readRDS("_nature/data/nhd_ctu_waterways_emissions_allyrs.RDS")


waste_gas <- bind_rows(readRDS("_waste/data/solid_waste_MN_by_gas.RDS"),
                       readRDS("_waste/data/solid_waste_gas_WI_allyrs.RDS"))

wastewater_gas <- readRDS("_waste/data/final_wastewater_allyrs.RDS")


building_gas <- 

building_elec <-
  
industrial_gas <- bind_rows(
  readRDS("_industrial/data/fuel_combustion_emissions_by_gas.RDS"),
  readRDS("_industrial/data/mpca_fuel_emissions_by_gas.RDS"),
  readRDS("_industrial/data/fluorinated_gas_emissions.RDS")
)
