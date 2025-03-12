##### run industrial scripts; compile county and CTU industrial emissions

source("./R/global_warming_potential.R")
ind_fuel_combustion <- readRDS("./_industrial/data/fuel_combustion_emissions_by_gas.rds")
ind_flight_emissions <- readRDS("./_industrial/data/flight_industrial_point_sources_ctu.rds")
ind_nei_emissions <- readRDS("./_industrial/data/nei_county_industrial_emissions.rds")

### first start with county level emission estimates
### The workflow is split out industrial fuel combustion into gas, oil, coal
### subtract away fuel emissions from FLIGHT and call it process
### subtract away flight from NEI and call it 'small industrial'
### If flight > NEI, call 'small industrial' 0

### Research suggest 'fuel gas' is typically natural gas, but occasionally gaseous propane, butane, etc
ind_fuel_combustion <- ind_fuel_combustion %>%
  mutate(fuel_type = case_when(
    specific_fuel_type == "Natural Gas" ~ "Fuel Gas",
    TRUE ~ general_fuel_type
  ))


county_fuel_combustion <- ind_fuel_combustion %>%
  filter(units_emissions != "avg_activity") %>%
  mutate(mt_co2e = case_when(
    units_emissions == "mt_ch4" ~ values_emissions * gwp$ch4,
    units_emissions == "mt_n2o" ~ values_emissions * gwp$n2o,
    TRUE ~ values_emissions
  )) %>%
  group_by(county_name, reporting_year, fuel_type) %>%
  summarize(mt_co2e = sum(mt_co2e)) %>%
  mutate(
    data_source = "EPA FLIGHT Subpart C Analysis",
    factor_source = "EPA Emission Factor Hub",
    sector = "Industrial",
    category = "Fuel combustion"
  ) %>%
  select(county_name,
    inventory_year = reporting_year,
    sector,
    category,
    source = fuel_type,
    data_source,
    factor_source,
    mt_co2e
  )

county_process_emissions <- ind_flight_emissions %>%
  filter(doublecount == "No") %>%
  group_by(county_name, inventory_year) %>%
  summarize(mt_co2e = sum(value_emissions)) %>%
  left_join(
    county_fuel_combustion %>%
      group_by(county_name, inventory_year) %>%
      summarize(mt_co2e = sum(mt_co2e)),
    by = c(
      "county_name",
      "inventory_year"
    ),
    suffix = c("_total", "_fuel_combustion")
  ) %>%
  # 2010 is absent for fuel combustion
  filter(inventory_year != 2010) %>%
  mutate(
    mt_co2e_fuel_combustion = coalesce(mt_co2e_fuel_combustion, 0),
    mt_co2e = mt_co2e_total - mt_co2e_fuel_combustion,
    sector = "Industrial",
    category = "Process",
    source = "Process", # This could be filled in from subparts with some effort
    data_source = "EPA FLIGHT",
    factor_source = "EPA Emission Factor Hub"
  ) %>%
  select(
    inventory_year, county_name, data_source,
    factor_source, mt_co2e, sector,
    category,
    source
  )

# to estimate smaller industrial emissions, we'll subtract all FLIGHT
# emissions away from NEI estimates. Negative numbers will be corrected
# to zero until a better data source is identified
county_small <- ind_nei_emissions %>%
  group_by(inventory_year, county_name) %>%
  summarize(mt_co2e_all = as.numeric(sum(values_emissions))) %>%
  left_join(ind_flight_emissions %>%
    group_by(county_name, inventory_year) %>%
    summarize(mt_co2e_big = sum(value_emissions))) %>%
  mutate(
    mt_co2e_big = coalesce(mt_co2e_big, 0),
    mt_co2e = mt_co2e_all - mt_co2e_big,
    mt_co2e = if_else(mt_co2e < 0, 0, mt_co2e),
    sector = "Industrial",
    category = "Other",
    source = "Small point source",
    data_source = "EPA NEI",
    factor_source = "EPA NEI"
  ) %>%
  select(
    inventory_year, county_name, data_source,
    factor_source, mt_co2e, sector,
    category,
    source
  )

county_industrial_emission <- bind_rows(
  county_fuel_combustion,
  county_process_emissions,
  county_small
) %>%
  mutate(units_emissions = "Metric tons CO2e") %>%
  rename(values_emissions = mt_co2e)

county_industrial_emission_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "county_name", class(county_industrial_emission$county_name), "County name",
    "inventory_year", class(county_industrial_emission$inventory_year), "Year of activity",
    "sector", class(county_industrial_emission$sector), "Sector of emissions",
    "category", class(county_industrial_emission$category), "Category of emissions",
    "source", class(county_industrial_emission$source), "Specific source of emissions",
    "data_source", class(county_industrial_emission$data_source), "Activity data source",
    "factor_source", class(county_industrial_emission$factor_source), "Emissions factor data source",
    "values_emissions", class(county_industrial_emission$values_emissions), "Numerical value of emissions data",
    "units_emissions", class(county_industrial_emission$units_emissions), "Units of emissions data"
  )

saveRDS(county_industrial_emission, "./_industrial/data/county_industrial_emissions.rds")
saveRDS(county_industrial_emission_meta, "./_industrial/data/county_industrial_emissions_meta.rds")


## do the same for cities, excepting the nei source


city_fuel_combustion <- ind_fuel_combustion %>%
  filter(units_emissions != "avg_activity") %>%
  mutate(mt_co2e = case_when(
    units_emissions == "mt_ch4" ~ values_emissions * gwp$ch4,
    units_emissions == "mt_n2o" ~ values_emissions * gwp$n2o,
    TRUE ~ values_emissions
  )) %>%
  group_by(city_name, reporting_year, fuel_type) %>%
  summarize(mt_co2e = sum(mt_co2e)) %>%
  mutate(
    data_source = "EPA FLIGHT Subpart C Analysis",
    factor_source = "EPA Emission Factor Hub",
    sector = "Industrial",
    category = "Fuel combustion"
  ) %>%
  select(city_name,
    inventory_year = reporting_year,
    sector,
    category,
    source = fuel_type,
    data_source,
    factor_source,
    mt_co2e
  )

city_process_emissions <- ind_flight_emissions %>%
  filter(doublecount == "No") %>%
  group_by(city_name, inventory_year) %>%
  summarize(mt_co2e = sum(value_emissions)) %>%
  left_join(
    city_fuel_combustion %>%
      group_by(city_name, inventory_year) %>%
      summarize(mt_co2e = sum(mt_co2e)),
    by = c(
      "city_name",
      "inventory_year"
    ),
    suffix = c("_total", "_fuel_combustion")
  ) %>%
  # 2010 is absent for fuel combustion
  filter(inventory_year != 2010) %>%
  mutate(
    mt_co2e_fuel_combustion = coalesce(mt_co2e_fuel_combustion, 0),
    mt_co2e = mt_co2e_total - mt_co2e_fuel_combustion,
    mt_co2e = if_else(mt_co2e < 0, 0, mt_co2e),
    sector = "Industrial",
    category = "Process",
    source = "Process", # This could be filled in from subparts with some effort
    data_source = "EPA FLIGHT",
    factor_source = "EPA Emission Factor Hub"
  ) %>%
  select(
    inventory_year, city_name, data_source,
    factor_source, mt_co2e, sector,
    category,
    source
  )

city_industrial_emission <- bind_rows(
  city_fuel_combustion,
  city_process_emissions
) %>%
  mutate(units_emissions = "Metric tons CO2e") %>%
  rename(values_emissions = mt_co2e)

city_industrial_emission_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "city_name", class(city_industrial_emission$city_name), "County name",
    "inventory_year", class(city_industrial_emission$inventory_year), "Year of activity",
    "sector", class(city_industrial_emission$sector), "Sector of emissions",
    "category", class(city_industrial_emission$category), "Category of emissions",
    "source", class(city_industrial_emission$source), "Specific source of emissions",
    "data_source", class(city_industrial_emission$data_source), "Activity data source",
    "factor_source", class(city_industrial_emission$factor_source), "Emissions factor data source",
    "values_emissions", class(city_industrial_emission$values_emissions), "Numerical value of emissions data",
    "units_emissions", class(city_industrial_emission$units_emissions), "Units of emissions data"
  )

saveRDS(city_industrial_emission, "./_industrial/data/city_industrial_emissions.rds")
saveRDS(city_industrial_emission_meta, "./_industrial/data/city_industrial_emissions_meta.rds")
