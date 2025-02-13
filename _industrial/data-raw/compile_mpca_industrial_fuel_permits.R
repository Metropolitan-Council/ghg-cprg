##### read in MPCA data fuel combustion data

source("R/_load_pkgs.R")
source("R/global_warming_potential.R")

cprg_county <- readRDS("_meta/data/cprg_county.rds") %>%
  st_drop_geometry()
cprg_ctu <- readRDS("_meta/data/cprg_ctu.rds") %>%
  st_drop_geometry()
ctu_population <- readRDS("_meta/data/ctu_population.rds")
ghg_factor_hub <- readRDS("_meta/data/epa_ghg_factor_hub.rds")

mpca_fuel <- read_xlsx("_industrial/data-raw/MPCA_industrial_fuel_throughput.xlsx",
  sheet = 1
) %>%
  clean_names()

### create factor hub with matching fuel types to MPCA
fuel_emission_factors <- ghg_factor_hub$industrial_combustion %>%
  mutate(fuel_type = case_when(
    grepl("Distillate Fuel", `Fuel type`) ~ "Distillate Oil",
    `Fuel type` == "Sub-bituminous Coal" ~ "Coal,Subbit",
    `Fuel type` == "Anthracite Coal" ~ "Coal,Anth",
    `Fuel type` == "Natural Gasoline" ~ "Gasoline",
    `Fuel type` == "Wood and Wood Residuals" ~ "Wood",
    `Fuel type` == "Liquefied Petroleum Gases (LPG)" ~ "Lpg",
    grepl("Residual Fuel", `Fuel type`) ~ "Residual Oil",
    # this next one is my best guess - fuel gas is large source in FLIGHT
    `Fuel type` == "Fuel Gas" ~ "Process Gas",
    `Fuel type` == "Coal Coke" ~ "Coke",
    `Fuel type` == "Municipal Solid Waste" ~ "Waste,Solid",
    `Fuel type` == "Kerosene-Type Jet Fuel" ~ "Jet Fuel",
    `Fuel type` == "Used Oil" ~ "Waste Oil",
    # Methanol has no match, and is most chemically similar to ethanol
    `Fuel type` == "Ethanol (100%)" ~ "Methanol",
    # leftovers with no clear analogue being lumped into other
    `Fuel type` %in% "Other Oil (>401 deg F)" ~ "Other Oil",
    TRUE ~ `Fuel type`
  )) %>%
  group_by(fuel_category, fuel_form, emission, per_unit, fuel_type) %>%
  summarize(value = mean(value)) %>%
  arrange(fuel_type) %>%
  bind_rows(
    .,
    ghg_factor_hub$mobile_combustion %>%
      filter(`Fuel Type` == "Diesel Fuel") %>%
      rename(fuel_type = `Fuel Type`, value = `kg CO2 per unit`, per_unit = Unit) %>%
      mutate(
        fuel_category = "Petroleum Products",
        fuel_form = "Liquid",
        emission = "kg CO2"
      )
  )
#               mutate(fuel_type = case_when(
#                 grepl("Distillate Fuel", `Fuel type`) ~ "Distillate Oil",
#                 `Fuel type` == "Sub-bituminous Coal" ~ "Coal,Subbit",
#                 `Fuel type` == "Bituminous Coal" ~ "Coal,Bit",
#                 `Fuel type` == "Anthracite Coal" ~ "Coal,Anth",
#                 `Fuel type` == "Natural Gasoline" ~ "Gasoline",
#                 `Fuel type` == "Wood and Wood Residuals" ~ "Wood",
#                 `Fuel type` == "Liquefied Petroleum Gases (LPG)" ~ "Lpg",
#                 grepl("Residual Fuel", `Fuel type`) ~ "Residual Oil",
#                 # this next one is my best guess - fuel gas is large source in FLIGHT
#                 `Fuel type` == "Fuel Gas" ~ "Process Gas",
#                 `Fuel type` == "Coal Coke" ~ "Coke",
#                 `Fuel type` == "Municipal Solid Waste" ~ "Waste,Solid",
#                 `Fuel type` == "Kerosene-Type Jet Fuel" ~ "Jet Fuel",
#                 `Fuel type` == "Used Oil" ~ "Waste Oil",
#                 # leftovers with no clear analogue being lumped into other
#                 `Fuel type` %in% "Other Oil (>401 deg F)" ~ "Other Oil",
#                 TRUE ~ `Fuel type`
#               )) %>%
#               group_by(fuel_category, fuel_form, emission, per_unit, fuel_type) %>%
#               summarize(value = mean(value)) %>%
#               arrange(fuel_type))

### format data

mpca_fuel_formatted <- mpca_fuel %>%
  ## bring in county and ctu IDs
  mutate(county_name = str_to_sentence(county_name)) %>%
  filter(
    county_name %in% c(cprg_county$county_name),
    !sector %in% c("Waste", "Transportation", "Electric Power"),
    # can't figure out waht this is - only unit reported in barrels
    !standard_material_code == "REFINERY FED"
  ) %>%
  left_join(cprg_county %>% select(county_name, geoid),
    by = "county_name"
  ) %>%
  left_join(
    ctu_population %>%
      # going to assume most facilities are in cities not townships
      filter(ctu_class != "TOWNSHIP") %>%
      distinct(ctu_name, ctuid),
    by = c("geo_city_name" = "ctu_name")
  ) %>%
  ### clean variables
  mutate(
    inventory_year = as.numeric(gsub("EI ", "", inventory_id)),
    value_activity = case_when(
      grepl("E3", activity_unit_code) ~ activity_amt * 10^3,
      grepl("E6", activity_unit_code) ~ activity_amt * 10^6,
      TRUE ~ activity_amt
    ),
    unit_activity = case_when(
      grepl("FT3", activity_unit_code) ~ "scf",
      grepl("GAL", activity_unit_code) ~ "gallon",
      grepl("BBL", activity_unit_code) ~ "barrel",
      grepl("TON", activity_unit_code) ~ "short ton"
    ),
    fuel_type = str_to_title(standard_material_code)
  ) %>%
  mutate(fuel_type = case_when(
    fuel_type %in% fuel_emission_factors$fuel_type ~ fuel_type,
    fuel_type == "Gas" ~ "Natural Gas",
    TRUE ~ "Other Oil"
  )) %>%
  select(county_name,
    county_id = geoid, ctu_name = geo_city_name, ctuid,
    inventory_year, value_activity, unit_activity, fuel_type, source_name,
    sector, naics, naics_description
  )


mpca_fuel_emissions_gas <- mpca_fuel_formatted %>%
  left_join(., fuel_emission_factors,
    by = c("fuel_type",
      "unit_activity" = "per_unit"
    ),
    relationship = "many-to-many"
  ) %>%
  filter(emission != "mmBtu") %>%
  # convert conversion factor to produce metric tons
  mutate(
    value_mt = as.numeric(case_when(
      grepl("CO2", emission) ~ value * units::as_units("kilogram") %>%
        units::set_units("metric_ton"),
      TRUE ~ value * units::as_units("gram") %>%
        units::set_units("metric_ton")
    )),
    # calculate emissions based on activity and MT emission factor
    value_emissions = value_mt * value_activity,
    unit_emissions = paste("Metric tons", sub(".* ", "", emission))
  ) %>%
  group_by(
    county_name, county_id, ctu_name, ctuid,
    inventory_year, fuel_category, fuel_type, unit_emissions,
    source_name, sector, naics, naics_description
  ) %>%
  summarize(value_emissions = sum(value_emissions))

mpca_fuel_emissions_co2e <- mpca_fuel_formatted %>%
  left_join(., fuel_emission_factors,
    by = c("fuel_type",
      "unit_activity" = "per_unit"
    ),
    relationship = "many-to-many"
  ) %>%
  filter(emission != "mmBtu") %>%
  # convert conversion factor to produce metric tons
  mutate(
    value_mt = as.numeric(case_when(
      grepl("CO2", emission) ~ value * units::as_units("kilogram") %>%
        units::set_units("metric_ton"),
      grepl("CH4", emission) ~ value * gwp$ch4 * units::as_units("gram") %>%
        units::set_units("metric_ton"),
      grepl("N2O", emission) ~ value * gwp$n2o * units::as_units("gram") %>%
        units::set_units("metric_ton")
    )),
    # calculate emissions based on activity and MT emission factor
    value_emissions = value_mt * value_activity,
    unit_emissions = "Metric tons CO2e"
  ) %>%
  group_by(
    county_name, county_id, ctu_name, ctuid,
    inventory_year, fuel_category, fuel_type, unit_emissions,
    source_name, sector, naics, naics_description
  ) %>%
  summarize(value_emissions = sum(value_emissions))

mpca_fuel_emissions_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "source_name", class(mpca_fuel_emissions_co2e$source_name), "Facility name",
    "naics", class(mpca_fuel_emissions_co2e$naics), "NAICS code for facility",
    "naics_description", class(mpca_fuel_emissions_co2e$naics_description), "NAICS description of facility type",
    "county_name", class(mpca_fuel_emissions_co2e$county_name), "County name",
    "county_id", class(mpca_fuel_emissions_co2e$county_id), "County geographic ID",
    "ctu_name", class(mpca_fuel_emissions_co2e$ctu_name), "City name",
    "ctuid", class(mpca_fuel_emissions_co2e$ctuid), "CTU geographic ID",
    "sector", class(mpca_fuel_emissions_co2e$sector), "Economic sector of point source: Industrial or Commercial",
    "inventory_year", class(mpca_fuel_emissions_co2e$inventory_year), "Year of activity",
    "fuel_category", class(mpca_fuel_emissions_co2e$fuel_category), "General category of fuel combusted",
    "fuel_type", class(mpca_fuel_emissions_co2e$fuel_type), "Specific type of fuel combusted",
    "value_emissions", class(mpca_fuel_emissions_co2e$value_emissions), "Numerical value of emissions data",
    "unit_emissions", class(mpca_fuel_emissions_co2e$unit_emissions), "Units of emissions data"
  )

saveRDS(mpca_fuel_emissions_gas, "./_industrial/data/mpca_fuel_emissions_by_gas.rds")
saveRDS(mpca_fuel_emissions_co2e, "./_industrial/data/mpca_fuel_emissions.rds")
saveRDS(mpca_fuel_emissions_meta, "./_industrial/data/mpca_fuel_emissions_meta.rds")
saveRDS(mpca_fuel_emissions_meta, "./_industrial/data/mpca_fuel_emissions_by_gas_meta.rds")
