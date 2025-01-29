rm(list = ls())
source("R/_load_pkgs.R")

overwrite_RDS <- TRUE

# Following the Technical support document to the MPCA Statewide Greenhouse Gas Inventory
# see section 5.3.7 Methane from Inland Waters
#
# all in units of lbs CH4 per day per acre
# lakes: 0.31
# rivers/streams: 1.08
# reservoirs: 0.74

#+ take your emissions estimates in lbs CH4 per day per acre
#+ multiply by 365 days per year
#+ multiply by 247.105 acres per square kilometer
#+ divide by 2.20462 lb per kg
#+ divide by 1000 kg per metric ton
#+
#+ this will yield value emissions in units of metric tons CH4 per km2 per year

# # after conversion, these values are (in units of metric tons CH4 per km2 per year)
# # lakes: 12.68243
# # rivers/streams: 44.18394
# # reservoirs: 30.27418
#
# # EXAMPLE
# # Input
# lbs_CH4_per_day_per_acre = 0.74
#
# # Conversion factors
# days_to_years = 365
# acre_to_km2 = 247.105
# lb_to_kg = 2.20462
# kg_to_mt = 1000
#
#
# # Conversion
# mtCH4_per_year_per_km2 =
#   lbs_CH4_per_day_per_acre * # take your emissions estimates in lbs CH4 per day per acre
#   days_to_years *            # multiply by 365 days per year
#   acre_to_km2 *              # multiply by 247.105 acres per square kilometer
#   (1/lb_to_kg) *             # divide by 2.20462 lb per kg
#   (1/kg_to_mt)               # divide by 1000 kg per metric ton
# mtCH4_per_year_per_km2

# Function to convert lbs_CH4_per_day_per_acre to ton_CO2e_per_year_per_km2
lbsCH4_per_acre_day_TO_mtCH4_per_km2_year <- function(lbs_CH4_per_day_per_acre) {
  days_to_years <- 365
  acre_to_km2 <- 247.105
  lb_to_kg <- 2.20462
  kg_to_mt <- 1000

  mtCH4_per_km2_year <-
    lbs_CH4_per_day_per_acre * # take your emissions estimates in lbs CH4 per day per acre
      days_to_years * # multiply by 365 days per year
      acre_to_km2 * # multiply by 247.105 acres per square kilometer
      (1 / lb_to_kg) * # divide by 2.20462 lb per kg
      (1 / kg_to_mt) # divide by 1000 kg per metric ton
  return(mtCH4_per_km2_year)
}

# lbsCH4_per_acre_day_TO_mtCH4_per_km2_year(0.31)
# lbsCH4_per_acre_day_TO_mtCH4_per_km2_year(1.08)
# lbsCH4_per_acre_day_TO_mtCH4_per_km2_year(0.74)



waterway_types <- c("LakePond", "Reservoir", "SwampMarsh", "StreamRiver", "Lock Chamber", "DamWeir")


waterway_emissions_factors <- data.frame(
  waterway_type = waterway_types,
  factor_source = "MPCA TSD",
  ef_mt_ch4 = NA,
  units_emissions = "Metric tons CH4"
) %>%
  as_tibble() %>%
  mutate(
    ef_mt_ch4 = as.numeric(case_when(
      waterway_type == "LakePond" ~ lbsCH4_per_acre_day_TO_mtCH4_per_km2_year(0.31), # use lakes estimate, 0.31 lbs CH4 per day per acre
      waterway_type == "Reservoir" ~ lbsCH4_per_acre_day_TO_mtCH4_per_km2_year(0.74), # use reservoirs estimate, 0.74 lbs CH4 per day per acre
      waterway_type == "SwampMarsh" ~ lbsCH4_per_acre_day_TO_mtCH4_per_km2_year(0.31), # use lakes estimate, 0.31 lbs CH4 per day per acre
      waterway_type == "StreamRiver" ~ lbsCH4_per_acre_day_TO_mtCH4_per_km2_year(1.08), # use rivers/streams estimate, 1.08 lbs CH4 per day per acre
      waterway_type == "Lock Chamber" ~ lbsCH4_per_acre_day_TO_mtCH4_per_km2_year(0.74), # use reservoirs estimate, 0.74 lbs CH4 per day per acre
      waterway_type == "DamWeir" ~ lbsCH4_per_acre_day_TO_mtCH4_per_km2_year(0.74) # use reservoirs estimate, 0.74 lbs CH4 per day per acre
    ))
  )



waterway_emissions_factors_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "waterway_type", class(waterway_emissions_factors$waterway_type), "Waterway type from National Hydrogaphy Dataset",
    "factor_source", class(waterway_emissions_factors$factor_source), "Emissions factors sourced from MPCA Technical Support Document",
    "ef_mt_ch4", class(waterway_emissions_factors$ef_mt_ch4), "Methane emissions factor for each waterway type in metric tons CH4 per square kilometer per year",
    "units_emissions", class(waterway_emissions_factors$units_emissions), "Units and gas type of emissions"
  )

# User chooses whether to overwrite the rds files
if (overwrite_RDS) {
  saveRDS(waterway_emissions_factors, "./_nature/data/waterways_emissions_factors.rds")
  saveRDS(waterway_emissions_factors_meta, "./_nature/data/waterways_emissions_factors_meta.rds")
}
