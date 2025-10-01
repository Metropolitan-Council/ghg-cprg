# compile emissions by gas type from all sectors into a single data table
source("R/_load_pkgs.R")
source("R/global_warming_potential.R")

cprg_county <- readRDS("_meta/data/cprg_county.RDS")
ef_hub <- readRDS("_meta/data/epa_ghg_factor_hub.RDS")

transportation_gas <- readRDS("_transportation/data/epa_onroad_emissions_compile.rds") %>%
  filter(
    emissions_year == 2022,
    pollutant_code %in% c(
      "CO2",
      "CH4",
      "N2O"
    )
  ) %>%
  mutate(
    value_emissions = as.numeric(emissions * units::as_units("gram") %>%
      units::set_units("metric_ton")),
    units_emissions = paste("Metric tons", pollutant_code)
  ) %>%
  filter(!is.na(value_emissions)) %>%
  group_by(emissions_year, county_name, units_emissions) %>%
  summarize(value_emissions = sum(value_emissions)) %>%
  ungroup() %>%
  mutate(
    sector = "Transportation",
    metric_tons_co2e = case_when(
      grepl("CH4", units_emissions) ~ value_emissions * gwp$ch4,
      grepl("N2O", units_emissions) ~ value_emissions * gwp$n2o,
      grepl("CO2", units_emissions) ~ value_emissions * gwp$co2
    )
  )

aviation_2022 <- read_rds("_transportation/data/aviation_emissions.rds") %>%
  filter(inventory_year == 2022)
## need to back-calculate to gases for 2022

aviation_ef <- read_rds("_meta/data/epa_ghg_factor_hub.RDS") %>%
  pluck("stationary_combustion") %>%
  filter(`Fuel type` == "Kerosene-Type Jet Fuel" & per_unit == "gallon" & emission != "mmBtu") %>%
  mutate(
    mt_gas = case_when(
      # CO2 emissions factor is reported in kilograms per gallon
      grepl("CO2", emission) ~ value %>%
        units::as_units("kilogram") %>%
        units::set_units("metric_ton") %>%
        as.numeric(),
      # all others are reported in grams per gallon
      grepl("CH4", emission) ~ value %>%
        units::as_units("gram") %>%
        units::set_units("metric_ton") %>%
        as.numeric(),
      grepl("N2O", emission) ~ value %>%
        units::as_units("gram") %>%
        units::set_units("metric_ton") %>%
        as.numeric(),
    ),
    mt_co2e = case_when(
      grepl("CO2", emission) ~ mt_gas,
      grepl("CH4", emission) ~ mt_gas * gwp$ch4,
      grepl("N2O", emission) ~ mt_gas * gwp$n2o,
    ),
    mt_co2e_perc = mt_co2e / sum(mt_co2e),
    gas_type = str_extract(emission, "(CO2|CH4|N2O)$")
  ) %>%
  select(gas_type, mt_co2e_perc, gas_type)

aviation_gas <- aviation_2022 %>%
  ungroup() %>%
  cross_join(aviation_ef) %>%
  mutate(
    metric_tons_co2e = value_emissions * mt_co2e_perc,
    units_emissions = paste("Metric tons", gas_type),
    value_emissions = case_when(
      grepl("CO2", units_emissions) ~ metric_tons_co2e,
      grepl("CH4", units_emissions) ~ metric_tons_co2e / gwp$ch4,
      grepl("N2O", units_emissions) ~ metric_tons_co2e / gwp$n2o,
    )
  ) %>%
  select(sector, emissions_year = inventory_year, units_emissions, metric_tons_co2e, value_emissions, county_name = geog_name)

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
  mutate(
    sector = "Agriculture",
    metric_tons_co2e = case_when(
      grepl("CH4", units_emissions) ~ value_emissions * gwp$ch4,
      grepl("N2O", units_emissions) ~ value_emissions * gwp$n2o,
      grepl("CO2", units_emissions) ~ value_emissions * gwp$co2
    )
  )

# freshwater methane emissions
ns_gas <- readRDS("_nature/data/nhd_ctu_waterways_emissions_allyrs.RDS") %>%
  filter(inventory_year == 2022) %>%
  group_by(county_name, inventory_year, units_emissions) %>%
  summarize(value_emissions = sum(value_emissions)) %>%
  ungroup() %>%
  rename(emissions_year = inventory_year) %>%
  mutate(
    sector = "Natural Systems",
    metric_tons_co2e = case_when(
      grepl("CH4", units_emissions) ~ value_emissions * gwp$ch4,
      grepl("N2O", units_emissions) ~ value_emissions * gwp$n2o,
      grepl("CO2", units_emissions) ~ value_emissions * gwp$co2
    )
  )


waste_gas <- bind_rows(
  readRDS("_waste/data/solid_waste_MN_by_gas.RDS"),
  readRDS("_waste/data/solid_waste_gas_WI_allyrs.RDS")
) %>%
  filter(inventory_year == 2022) %>%
  group_by(geoid, inventory_year, units_emissions) %>%
  summarize(value_emissions = sum(value_emissions)) %>%
  ungroup() %>%
  left_join(cprg_county %>%
    st_drop_geometry() %>%
    select(geoid, county_name)) %>%
  select(-geoid) %>%
  rename(emissions_year = inventory_year) %>%
  mutate(
    sector = "Waste",
    metric_tons_co2e = case_when(
      grepl("CH4", units_emissions) ~ value_emissions * gwp$ch4,
      grepl("N2O", units_emissions) ~ value_emissions * gwp$n2o,
      grepl("CO2", units_emissions) ~ value_emissions * gwp$co2
    )
  )

wastewater_gas <- readRDS("_waste/data/final_wastewater_allyrs.RDS") %>%
  filter(inventory_year == 2022) %>%
  group_by(county_name, inventory_year, units_emissions) %>%
  summarize(value_emissions = sum(value_emissions)) %>%
  ungroup() %>%
  rename(emissions_year = inventory_year) %>%
  mutate(
    sector = "Waste",
    metric_tons_co2e = case_when(
      grepl("CH4", units_emissions) ~ value_emissions * gwp$ch4,
      grepl("N2O", units_emissions) ~ value_emissions * gwp$n2o,
      grepl("CO2", units_emissions) ~ value_emissions * gwp$co2
    )
  )

ng_emissions <- ef_hub$stationary_combustion %>%
  filter(
    fuel_category == "Natural Gas",
    per_unit == "scf"
  ) %>%
  mutate(
    gas_type = str_extract(emission, "(CO2|CH4|N2O)$"),
    mt_gas = case_when(
      gas_type != "CO2" ~
        as.numeric(value * units::as_units("gram") %>%
          units::set_units("metric_ton")),
      gas_type == "CO2" ~
        as.numeric(value * units::as_units("kilogram") %>%
          units::set_units("metric_ton"))
    ),
    units_emissions = paste("Metric tons", gas_type)
  ) %>%
  filter(!is.na(gas_type)) %>%
  select(fuel_category, per_unit, mt_gas, units_emissions)


building_gas <- readRDS("_energy/data/county_natgas_activity.RDS") %>%
  filter(year == 2022) %>%
  cross_join(ng_emissions) %>%
  mutate(
    value_emissions = activity * 1000 * mt_gas,
    metric_tons_co2e = case_when(
      grepl("CH4", units_emissions) ~ value_emissions * gwp$ch4,
      grepl("N2O", units_emissions) ~ value_emissions * gwp$n2o,
      grepl("CO2", units_emissions) ~ value_emissions * gwp$co2
    )
  ) %>% # converts mcf to scf which is per unit
  select(emissions_year = year, county_name, sector, value_emissions, units_emissions, metric_tons_co2e)

elec_emissions <- ef_hub$egridTimeSeries %>%
  filter(Year == 2022) %>%
  mutate(
    gas_type = str_extract(emission, "(CO2|CH4|N2O)$"),
    mt_gas = as.numeric(value * units::as_units("pound") %>%
      units::set_units("metric_ton")),
    units_emissions = paste("Metric tons", gas_type)
  ) %>%
  select(per_unit, mt_gas, units_emissions)

electricity_gas <- readRDS("_energy/data/county_elec_activity.RDS") %>%
  filter(year == 2022) %>%
  cross_join(elec_emissions) %>%
  mutate(
    value_emissions = activity * mt_gas,
    metric_tons_co2e = case_when(
      grepl("CH4", units_emissions) ~ value_emissions * gwp$ch4,
      grepl("N2O", units_emissions) ~ value_emissions * gwp$n2o,
      grepl("CO2", units_emissions) ~ value_emissions * gwp$co2
    )
  ) %>%
  select(emissions_year = year, county_name, sector, value_emissions, units_emissions, metric_tons_co2e)

industrial_fuel_gas_epa <- readRDS("_industrial/data/fuel_combustion_emissions_by_gas.RDS") %>%
  filter(
    doublecount != "Yes",
    specific_fuel_type != "Municipal Solid Waste"
  )

industrial_fuel_gas_epa_out <- industrial_fuel_gas_epa %>%
  group_by(reporting_year, county_name, city_name, units_emissions) %>%
  summarize(value_emissions = sum(values_emissions)) %>%
  ungroup() %>%
  filter(
    reporting_year == 2022,
    units_emissions != "avg_activity"
  ) %>%
  rename(emissions_year = reporting_year) %>%
  mutate(
    units_emissions = paste(
      "Metric tons",
      toupper(substr(units_emissions, 4, 6))
    ),
    metric_tons_co2e = case_when(
      grepl("CH4", units_emissions) ~ value_emissions * gwp$ch4,
      grepl("N2O", units_emissions) ~ value_emissions * gwp$n2o,
      grepl("CO2", units_emissions) ~ value_emissions * gwp$co2
    )
  ) %>%
  group_by(emissions_year, county_name, units_emissions) %>%
  summarize(
    value_emissions = sum(value_emissions),
    metric_tons_co2e = sum(metric_tons_co2e)
  ) %>%
  ungroup() %>%
  mutate(sector = "Industrial")

industrial_fuel_gas_mpca <- readRDS("_industrial/data/mpca_fuel_emissions_by_gas.RDS") %>%
  filter(fuel_type != "Natural Gas") %>%
  group_by(inventory_year, county_name, ctu_name, unit_emissions) %>%
  summarize(value_emissions = sum(value_emissions)) %>%
  ungroup() %>%
  rename(
    units_emissions = unit_emissions,
    emissions_year = inventory_year
  ) %>%
  filter(!ctu_name %in% industrial_fuel_gas_epa$city_name) %>%
  mutate(metric_tons_co2e = case_when(
    grepl("CH4", units_emissions) ~ value_emissions * gwp$ch4,
    grepl("N2O", units_emissions) ~ value_emissions * gwp$n2o,
    grepl("CO2", units_emissions) ~ value_emissions * gwp$co2
  )) %>%
  group_by(emissions_year, county_name, units_emissions) %>%
  summarize(
    value_emissions = sum(value_emissions),
    metric_tons_co2e = sum(metric_tons_co2e)
  ) %>%
  ungroup() %>%
  mutate(sector = "Industrial") %>%
  filter(emissions_year == 2022)

fluorinated_gases <- readRDS("_industrial/data/fluorinated_gas_emissions.RDS") %>%
  mutate(
    mt_gas = case_when(
      gas_type == "sf6" ~ value_emissions / gwp$sf6,
      gas_type == "nf3" ~ value_emissions / gwp$nf3,
      gas_type == "hfc" ~ value_emissions / gwp$hfc,
      gas_type == "pfc" ~ value_emissions / gwp$cf4,
      gas_type == "other_fully_fluorinated_ghg" ~ value_emissions / ((gwp$cf4 + gwp$nf3) / 2) # best guess for what these might be based on NAICS codes
    ),
    units_emissions = paste("Metric tons fluorocarbons"),
    emissions_year = as.numeric(inventory_year)
  ) %>%
  filter(emissions_year == 2022) %>%
  group_by(emissions_year, county_name, units_emissions) %>%
  summarize(
    metric_tons_co2e = sum(value_emissions),
    value_emissions = sum(mt_gas)
  ) %>%
  ungroup() %>%
  mutate(sector = "Industrial")

gas_by_county <- bind_rows(
  transportation_gas,
  aviation_gas,
  building_gas,
  electricity_gas,
  waste_gas,
  wastewater_gas,
  agriculture_gas,
  ns_gas,
  industrial_fuel_gas_epa_out,
  industrial_fuel_gas_mpca,
  fluorinated_gases
)

county_emissions <- read_rds("_meta/data/cprg_county_emissions.RDS")

gas_by_county %>%
  pull(metric_tons_co2e) %>%
  sum() # 52999987
county_emissions %>%
  filter(
    value_emissions > 0,
    emissions_year == 2022
  ) %>%
  pull(value_emissions) %>%
  sum() # 55377863

52999987 - 55377863 # 4373319 more in gas type analysis

# Problem is in industrial

gas_by_county %>%
  filter(sector == "Industrial") %>%
  group_by(county_name) %>%
  summarize(value_emissions = sum(metric_tons_co2e))

county_emissions %>%
  filter(
    value_emissions > 0,
    emissions_year == 2022,
    sector == "Industrial",
    category != "Building Fuel",
    category != "Electricity"
  ) %>%
  group_by(county_name) %>%
  summarize(value_emissions = sum(value_emissions))
#

# The remaining differences are from specific industrial processes (e.g. hydrogen production, lead production, industrial landfill) that are
# provided by GHGRP but don't have gas type break downs. This needs to be reviewed at a later date.

industrial_fuel_gas_epa_out %>%
  filter(sector == "Industrial") %>%
  group_by(county_name) %>%
  summarize(value_emissions = sum(metric_tons_co2e))

industrial_fuel_gas_mpca %>%
  filter(sector == "Industrial") %>%
  group_by(county_name) %>%
  summarize(value_emissions = sum(metric_tons_co2e))

fluorinated_gases %>%
  filter(sector == "Industrial") %>%
  group_by(county_name) %>%
  summarize(value_emissions = sum(metric_tons_co2e))
