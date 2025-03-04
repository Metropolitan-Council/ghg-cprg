# allocate WI state emissions by county population

source("R/_load_pkgs.R")
source(file.path(here::here(), "R/global_warming_potential.R"))


# from https://widnr.widen.net/view/pdf/o9xmpot5x7/AM610.pdf?t.download=true
# WI GHG Emissions Inventory from the DNR, 2018 data
# wi_total_emissions <- 2.2 * 10^6 # in mtco2e
# 0.1 from waste combustion
# 2.1 from landfills (accounts for flaring and landfill gas to energy)

cprg_county_proportions <- readRDS("_meta/data/cprg_county_proportions.RDS")

names <- c(source = "X", `2005` = "X2005", `2018` = "X2018")
wi_inventory <- read.csv(file.path(here::here(), "_waste/data-raw/solid_waste/tabula-wi_inventory_2005_2018.csv")) %>%
  rename(all_of(names)) %>%
  filter(
    source %in% c("Landfills", "Waste Combustion")
  ) %>%
  pivot_longer(
    cols = !source,
    names_to = "inventory_year",
    values_to = "value_emissions" # still in mmt co2e
  ) %>%
  mutate(
    inventory_year = as.numeric(inventory_year),
    value_emissions = as.numeric(value_emissions) * 10^6
  ) %>%
  # add 2021 with values == 2018
  rbind(
    tibble(
      source = c("Landfills", "Waste Combustion"),
      inventory_year = c(2021, 2021),
      value_emissions = c(2.1, 0.1) * 10^6
    )
  ) %>%
  complete(source, inventory_year = 2005:2021) %>%
  group_by(source) %>%
  mutate(
    value_emissions = zoo::na.approx(value_emissions, na.rm = FALSE)
  )

wi_pop <- cprg_county_proportions %>%
  filter(
    state_name == "Wisconsin",
    population_year %in% 2005:2021
  )
# names will need to be fixed later

solid_waste_wi <- wi_pop %>%
  mutate(population_year = as.numeric(population_year)) %>%
  left_join(
    wi_inventory,
    by = join_by(population_year == inventory_year),
    relationship = "many-to-many"
  ) %>%
  mutate(
    value_emissions = value_emissions * county_proportion_of_state_pop,
    sector = "Waste",
    category = "Solid waste",
    source = case_when(
      source == "Landfills" ~ "Landfill",
      source == "Waste Combustion" ~ "Waste to energy" # this should be incineration.
      # wait for discussion with MPCA to change
    ),
    data_source = "Wisconsin GHG Inventory",
    factor_source = "Wisconsin GHG Inventory",
    units_emissions = "Metric tons CO2e"
  ) %>%
  select(
    geoid,
    inventory_year = population_year,
    sector,
    category,
    source,
    data_source,
    factor_source,
    value_emissions,
    units_emissions
  )

solid_waste_wi_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "geoid", class(solid_waste_wi$geoid), "5-digit FIPS code",
    "inventory_year", class(solid_waste_wi$inventory_year), "Emissions estimation year",
    "sector", class(solid_waste_wi$sector), "Emissions sector (e.g., Waste)",
    "category", class(solid_waste_wi$category), "Category of emissions within given sector",
    "source", class(solid_waste_wi$source), "Subcategory-specific source (e.g., Landfill)",
    "data_source", class(solid_waste_wi$data_source), "Activity data source",
    "factor_source", class(solid_waste_wi$factor_source), "Emissions factor data source",
    "value_emissions", class(solid_waste_wi$value_emissions), "Emissions value",
    "units_emissions", class(solid_waste_wi$units_emissions), "Emissions units"
  )

saveRDS(solid_waste_wi, paste0("_waste/data/solid_waste_WI_allyrs.RDS"))
saveRDS(solid_waste_wi_meta, paste0("_waste/data/solid_waste_WI_allyrs_meta.RDS"))

# gas types ----

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
# solid_waste_gas_wi_meta <-
#   tibble::tribble(
#     ~"Column", ~"Class", ~"Description",
#     "geoid", class(solid_waste_gas_wi$geoid), "5-digit FIPS code",
#     "county_name", class(solid_waste_gas_wi$county_name), "County name",
#     "inventory_year", class(solid_waste_gas_wi$inventory_year), "Emissions estimation year",
#     "value_emissions", class(solid_waste_gas_wi$value_emissions), "Emissions value",
#     "units_emissions", class(solid_waste_gas_wi$units_emissions), "Emissions units",
#     "co2e", class(solid_waste_gas_wi$co2e), "Emissions in CO2-equivalent"
#   )
# 
# saveRDS(solid_waste_gas_wi, paste0("_waste/data/solid_waste_gas_WI_allyrs.RDS"))
# saveRDS(solid_waste_gas_wi_meta, paste0("_waste/data/solid_waste_gas_WI_allyrs_meta.RDS"))
