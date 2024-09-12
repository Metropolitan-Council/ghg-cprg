# This script will reorganize our 2021 final inventory into the Global Covenant 
# of Mayors Common Reporting Framework, including activity data and emissions 
# factors as required. The GCOM inventory will cover the year 2021 (only) and the 
# 7-county region.
source("R/_load_pkgs.R")

county_emissions <- readRDS(file.path(here::here(), "_meta/data/cprg_county_emissions.RDS"))

met_counties <- c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington")

emissions_council_region <- county_emissions %>% 
  filter(geog_name %in% met_counties,
         year == 2021) %>% 
  group_by(sector, category, source) %>% 
  mutate(
    emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e)
  ) %>% 
  select(
    sector,
    category,
    source,
    data_source,
    factor_source,
    emissions_metric_tons_co2e
  ) %>% 
  distinct() %>% 
  ungroup()
 

# modify categories eurgh
crf_emissions <- emissions_council_region %>% 
  unite(category_source, c("category", "source")) %>% 
  select(
    category_source,
    emissions_metric_tons_co2e
  ) %>% 
  pivot_wider(
    names_from = category_source,
    values_from = emissions_metric_tons_co2e
  ) %>% 
  mutate(
    "Stationary energy > Residential buildings^^" = 
      `Residential energy_Electricity` + `Residential energy_Natural gas`,
    "Stationary energy > Commercial buildings & facilities^^" = 
      `Commercial energy_Electricity` + `Commercial energy_Natural gas`,
    "Stationary energy > Institutional buildings & facilities^^" = "IE",
    "Stationary energy > Industrial buildings & facilities^^" =
      `Industrial energy_Electricity` + `Industrial energy_Natural gas`,
    "Stationary energy > Agriculture" = "IE",
    "Stationary energy > Fugitive emissions^^" = "NO",
    "Total Stationary Energy^" = `Total energy_Electricity` + `Total energy_Natural gas`,
    "Transportation > On-road^^" = `Passenger vehicles_Light-duty vehicles` +
      `Commercial vehicles_Medium-duty vehicles` + `Commercial vehicles_Heavy-duty vehicles`,
    "Transportation > Rail^^" = "",
    "Transportation > Waterborne navigation^^" = "",
    "Transprotation > Aviation^^" = "",
    "Transportation > Off-road^^" = "NO",
    "Total Transport^" = `Passenger vehicles_Light-duty vehicles` +
      `Commercial vehicles_Medium-duty vehicles` + `Commercial vehicles_Heavy-duty vehicles`,
    "Waste > Solid waste disposal^^" = `Solid waste_Landfill`,
    "Waste > Biological treatment^^" = `Solid waste_Organics`,
    "Waste > Incineration and open burning^^" = `Solid waste_Waste to energy`,
    "Waste > Wastewater^^" = `Wastewater_Wastewater`,
    "Total Waste" = `Solid waste_Landfill` + `Solid waste_Organics` +
      `Solid waste_Waste to energy` + `Wastewater_Wastewater`,
    "IPPU > Industrial process" = "NE",
    "IPPU > Product use" = "NE",
    "Total IPPU" = "NE"
  ) %>% 
  select(
    !contains("_")
  )
# sum to total region
