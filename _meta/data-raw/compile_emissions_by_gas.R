# compile emissions by gas type from all sectors into a single data table
source("R/_load_pkgs.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")


transportation_gas <- 

## already by gas
agriculture_gas <- readRDS("_agriculture/data/agricultural_emissions_county.rds")

#freshwater methane emissions
ns_gas <- readRDS("_nature/data/nhd_ctu_waterways_emissions_allyrs.RDS")


waste_gas <- bind_rows(readRDS("_waste/data/solid_waste_MN_by_gas.RDS"),
                       readRDS("_waste/data/solid_waste_gas_WI_allyrs.RDS"))

wastewater_gas <- readRDS("_waste/data/final_wastewater_allyrs.RDS")


building_gas

industrial_gas <- bind_rows(
  readRDS("_industrial/data/fuel_combustion_emissions_by_gas.RDS"),
  readRDS("_industrial/data/mpca_fuel_emissions_by_gas.RDS"),
  readRDS("_industrial/data/fluorinated_gas_emissions.RDS")
)
