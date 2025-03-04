source("R/_load_pkgs.R")
source(file.path(here::here(), "R/global_warming_potential.R"))

solid_waste_wi <- readRDS(file.path(here::here(), 
"_waste/data/solid_waste_WI_allyrs.RDS"))

# incineration factors ----
# see 02_compile_incineration_MN_allyrs.R
fcc <- .4 # fraction of carbon content in MSW, IPCC default
ffc <- .4 # fraction of fossil carbon in MSW, IPCC default
co2_factor <- fcc * ffc * 44 / 12 * .95 # efficiency of combustion for incineration
n2o_factor <- 50 * units::as_units("gram") %>%
  units::set_units("metric_ton") %>%
  as.numeric() # aggregate emissions factor for incineration, g N2O/metric tons waste, GHG Protocol default


co2_disaggregate = 1/(1 + (n2o_factor * gwp$n2o)/co2_factor)
n2o_disaggregate = 1/(co2_factor/n2o_factor + gwp$n2o)

# co2_disaggregate + n2o_disaggregate*gwp$n2o == 1

units_assign <- tibble::tribble(
  ~"source", ~"units_emissions",
  "Landfill", "Metric tons CH4",
  "Waste to energy", "Metric tons CO2",
  "Waste to energy", "Metric tons N2O"
)
# calculate ----
solid_waste_gas_wi <- solid_waste_wi %>%
  select(
    -units_emissions
  ) %>% 
  left_join(
    units_assign, relationship = "many-to-many"
  ) %>% 
  mutate(
    value_emissions = case_when(
      units_emissions == "Metric tons CH4" ~ value_emissions/gwp$ch4,
      units_emissions == "Metric tons CO2" ~ value_emissions*co2_disaggregate,
      units_emissions == "Metric tons N2O" ~ value_emissions*n2o_disaggregate
    )
  )

solid_waste_gas_wi_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "geoid", class(solid_waste_gas_wi$geoid), "5-digit FIPS code",
    "inventory_year", class(solid_waste_gas_wi$inventory_year), "Emissions estimation year",
    "sector", class(solid_waste_gas_wi$sector), "Emissions sector (e.g., Waste)",
    "category", class(solid_waste_gas_wi$category), "Category of emissions within given sector",
    "source", class(solid_waste_gas_wi$source), "Subcategory-specific source (e.g., Landfill)",
    "data_source", class(solid_waste_gas_wi$data_source), "Activity data source",
    "factor_source", class(solid_waste_gas_wi$factor_source), "Emissions factor data source",
    "value_emissions", class(solid_waste_gas_wi$value_emissions), "Emissions value",
    "units_emissions", class(solid_waste_gas_wi$units_emissions), "Emissions units"
  )

saveRDS(solid_waste_gas_wi, paste0("_waste/data/solid_waste_gas_WI_allyrs.RDS"))
saveRDS(solid_waste_gas_wi_meta, paste0("_waste/data/solid_waste_gas_WI_allyrs_meta.RDS"))



# previous attempt ----

# turns out this is only available economy-wide.
# wi_inventory_gas <- tibble::tribble(
#   ~"inventory_year", ~"units_emissions",~"co2e_state",
#   2005, "Metric tons CO2", 131.3,
#   2005, "Metric tons CH4", 12.6,
#   2005, "Metric tons N2O", 8.8,
#   2005, "Metric tons other gas", 2.2,
#   2018, "Metric tons CO2", 118.4,
#   2018, "Metric tons CH4", 14.7,
#   2018, "Metric tons N2O", 9.3,
#   2018, "Metric tons other gas", 3.1,
#   2021, "Metric tons CO2", 118.4,
#   2021, "Metric tons CH4", 14.7,
#   2021, "Metric tons N2O", 9.3,
#   2021, "Metric tons other gas", 3.1
# ) %>%
#   complete(units_emissions, inventory_year = 2005:2021) %>%
#   group_by(units_emissions) %>%
#   mutate(
#     co2e_state = zoo::na.approx(co2e_state, na.rm = FALSE)
#   ) %>% 
#   mutate(
#     co2e_state = co2e_state * 10^6,
#     # mmt to mt
#     value_emissions = case_when(
#       units_emissions == "Metric tons CH4" ~ co2e_state/gwp$ch4,
#       units_emissions == "Metric tons N2O" ~ co2e_state/gwp$n2o,
#       units_emissions == "Metric tons other gas" ~ co2e_state/gwp$`HFC-152a`,
#       units_emissions == "Metric tons CO2" ~ co2e_state/gwp$co2
#     )
#   ) 
# 
# solid_waste_gas_wi <- wi_pop %>%
#   mutate(population_year = as.numeric(population_year)) %>%
#   left_join(
#     wi_inventory_gas,
#     by = join_by(population_year == inventory_year),
#     relationship = "many-to-many"
#   ) %>%
#   mutate(
#     value_emissions = value_emissions * county_proportion_of_state_pop,
#     co2e = co2e_state * county_proportion_of_state_pop,
#     sector = "Waste",
#     category = "Solid waste",
#     data_source = "Wisconsin GHG Inventory",
#     factor_source = "Wisconsin GHG Inventory",
#   ) %>%
#   select(
#     geoid,
#     county_name,
#     inventory_year = population_year,
#     value_emissions,
#     units_emissions,
#     co2e
#   )
# 
