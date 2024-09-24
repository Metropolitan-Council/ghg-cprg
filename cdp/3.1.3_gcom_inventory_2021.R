# This script will reorganize our 2021 final inventory into the Global Covenant 
# of Mayors Common Reporting Framework, including activity data and emissions 
# factors as required. The GCOM inventory will cover the year 2021 (only) and the 
# 7-county region.
source("R/_load_pkgs.R")
library("writexl")

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

# require writexl
write_xlsx(emissions_council_region, "cdp/inventory_council_format.xlsx")
 
# Reassign to CRF categories ----

indirect_justification = "Indirect emissions were not estimated in this version of the inventory due to data and capacity limitations."
ne_justification = "Direct emissions from this sector were not estimated due to data limitations. Emissions from this sector may be included in an upcoming inventory update. "

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
    "Stationary energy > Institutional buildings & facilities^^" = 0, #IE
    "Stationary energy > Industrial buildings & facilities^^" =
      `Industrial energy_Electricity` + `Industrial energy_Natural gas`,
    "Stationary energy > Agriculture" = 0, #IE
    "Stationary energy > Fugitive emissions^^" = 0, #NO
    "Total Stationary Energy^" = `Total energy_Electricity` + `Total energy_Natural gas`,
    "Transportation > On-road^^" = `Passenger vehicles_Light-duty vehicles` +
      `Commercial vehicles_Medium-duty vehicles` + `Commercial vehicles_Heavy-duty vehicles`,
    "Transportation > Rail^^" = 0, # NE
    "Transportation > Waterborne navigation^^" = 0, #NO
    "Transprotation > Aviation^^" = 0, # NE
    "Transportation > Off-road^^" = 0, # NO
    "Total Transport^" = `Passenger vehicles_Light-duty vehicles` +
      `Commercial vehicles_Medium-duty vehicles` + `Commercial vehicles_Heavy-duty vehicles`,
    "Waste > Solid waste disposal^^" = `Solid waste_Landfill`,
    "Waste > Biological treatment^^" = `Solid waste_Organics`,
    "Waste > Incineration and open burning^^" = `Solid waste_Waste to energy`,
    "Waste > Wastewater^^" = `Wastewater_Wastewater`,
    "Total Waste" = `Solid waste_Landfill` + `Solid waste_Organics` +
      `Solid waste_Waste to energy` + `Wastewater_Wastewater`,
    "IPPU > Industrial process" = 0, #NE
    "IPPU > Product use" = 0, #NE
    "Total IPPU" = 0, #NE
    "AFOLU > Livestock" = 0, #NE
    "AFOLU > Land use" = `Sequestration_Grassland` + `Sequestration_Tree` +
      `Sequestration_Urban grassland` + `Sequestration_Urban tree` +
      `Sequestration_Wetland`,
    "AFOLU > Other AFOLU" = 0,
    "Total AFOLU" = `Sequestration_Grassland` + `Sequestration_Tree` +
      `Sequestration_Urban grassland` + `Sequestration_Urban tree` +
      `Sequestration_Wetland`,
    "Generation of grid-supplied energy > Electricity-only generation^^" = 
      `Total energy_Electricity`,
    "Generation of grid-supplied energy > CHP generation^^" = 0, #NE
    "Generation of grid-supplied energy > Heat/cold generation^^" = 
      `Total energy_Natural gas` + `Liquid stationary fuels_Propane` +
      `Liquid stationary fuels_Kerosene`,
    "Generation of grid-supplied energy > Local renewable generation" = 0, #NE
    "Total generation of grid-supplied energy" = `Total energy_Electricity` +
      `Total energy_Natural gas` + `Liquid stationary fuels_Propane` +
      `Liquid stationary fuels_Kerosene`,
    "Total Emissions (excluding generation of grid-supplied energy)" =
      `Total Stationary Energy^` + `Total Transport^` + `Total Waste` + `Total IPPU`
      + `Total AFOLU`
  ) %>% 
  select(
    !contains("_")
  ) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "Sectors and Subsectors",
    values_to = "Direct emissions (metric tonnes CO2e)^"
  ) %>% 
  # notation keys for no emissions
  mutate(
    "If you have no direct emissions to report, please select a notation key to
    explain why^" = case_when(
      `Direct emissions (metric tonnes CO2e)^` == 0  ~ case_when(
        `Sectors and Subsectors` %in% c("Stationary energy > Fugitive emissions^^", 
                                        "Transportation > Waterborne navigation^^",
                                        "Transportation > Off-road^^") 
        ~ "NO",
        `Sectors and Subsectors` %in% 
          c("Stationary energy > Institutional buildings & facilities^^",
            "Stationary energy > Agriculture") 
        ~ "IE",
        TRUE ~ "NE"
          )
    ),
    "Direct emissions (metric tonnes CO2e)^" = case_match(
      `Direct emissions (metric tonnes CO2e)^`,
      0 ~ NA,
      .default = `Direct emissions (metric tonnes CO2e)^`
    ),
    "Indirect emissions from the use of grid-supplied electricity, heat, steam and/or
    cooling (metric tonnes CO2e)^" = NA,
    "If you have no inderect emissions to report, please select a notation key 
    to explain why^" = "NE",
    "Emissions occurring outside the jurisdiction boundary as a result of in-jurisdiction
    activities (metric tonnes CO2e)" = NA,
    "If you have no emissions to report that are occurring outside the jurisdiction 
    boundary as a result of in-jurisdiction activities, please select a notation 
    key to explain why" = "NE",
    "Please explain any excluded sources, identify any emissions covered under 
    an ETS and provide any other comments^" = case_when(
      `If you have no direct emissions to report, please select a notation key to
    explain why^` == "NE" ~ paste0(ne_justification, indirect_justification),
    `If you have no direct emissions to report, please select a notation key to
    explain why^` == "IE" ~ case_when(
      `Sectors and Subsectors` == "Stationary energy > Institutional buildings & facilities^^"
      ~ "Because of the manner in which emissions were calculated, this category is included in `Commercial buildings and facilities`. See documentation attached in 3.1.1 for further information.",
      `Sectors and Subsectors` == "Stationary energy > Agriculture"
      ~ "Because of the manner in which emissions were calculated, this category is included in `Industrial buildings and facilities`. See documentation attached in 3.1.1 for further information."
    ),
      TRUE ~ indirect_justification
    )
  )

# export it as an xcel spreadsheet
write_xlsx(crf_emissions, "cdp/3.1.3_crf.xlsx")

# 2005 emissions ----

# pull in from other branches if at all possible. currently not usable.
emissions_2005 <- county_emissions %>% 
  filter(geog_name %in% met_counties,
         year == 2005) %>% 
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
