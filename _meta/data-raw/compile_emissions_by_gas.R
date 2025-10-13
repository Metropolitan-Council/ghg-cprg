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

industrial_gas <- readRDS("_industrial/data/industrial_emissions_by_gas.RDS") %>% 
  filter(inventory_year == 2022) %>% 
  group_by(inventory_year, county_name, units_emissions) %>% 
  summarize(value_emissions = sum(mt_gas),
            metric_tons_co2e = sum(metric_tons_co2e)) %>% 
  mutate(sector = "Industrial") %>% 
  select(emissions_year = inventory_year, 
         county_name, 
         sector, 
         value_emissions, 
         units_emissions, 
         metric_tons_co2e)



gas_by_county <- bind_rows(
  transportation_gas,
  aviation_gas,
  building_gas,
  electricity_gas,
  waste_gas,
  wastewater_gas,
  agriculture_gas,
  ns_gas,
  industrial_gas
)%>% 
  left_join(cprg_county %>% 
              st_drop_geometry() %>% 
              select(geoid, county_name))

county_emissions <- read_rds("_meta/data/cprg_county_emissions.RDS")

gas_by_county %>%
  pull(metric_tons_co2e) %>%
  sum() # 55374642
county_emissions %>%
  filter(
    value_emissions > 0,
    emissions_year == 2022
  ) %>%
  pull(value_emissions) %>%
  sum() # 55552139

#slight difference due to documented issues in industrial sector

county_emissions_by_gas_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "emissions_year ", class(gas_by_county$emissions_year ), "Year of survey",
    "geoid", class(gas_by_county$geoid), "County GEOID",
    "county_name", class(gas_by_county$county_name), "County name",
    "sector", class(gas_by_county$sector), "Economic sector",
    "value_emissions", class(gas_by_county$value_emissions), "Numerical value of emissions in metric tons",
    "units_emissions", class(gas_by_county$units_emissions), "Units and gas type of emissions",
    "metric_tons_co2e", class(gas_by_county$metric_tons_co2e ), "Metric tons of gas in CO2 equivalency"
  )

saveRDS(gas_by_county, "./_meta/data/county_emissions_by_gas.rds")
saveRDS(county_emissions_by_gas_meta, "./_meta/data/county_emissions_by_gas_meta.rds")
