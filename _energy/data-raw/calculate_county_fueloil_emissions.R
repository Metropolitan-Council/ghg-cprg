#### pull in ctu mmbtu and convert to emissions
source("R/_load_pkgs.R")
source("R/global_warming_potential.R")

county_fuel <- read_rds("_energy/data-raw/county_propane_fueloil_use.RDS")

fuel_ef_mmbtu <- readRDS("_meta/data/epa_ghg_factor_hub.RDS") %>%
  pluck("stationary_combustion") %>%
  filter(`Fuel type` %in% c("Kerosene",
                            "Propane") & 
           per_unit == "mmBtu") %>%
  mutate(
    mt_co2e_mmbtu = case_when(
      emission == "kg CO2" ~ value %>%
        units::as_units("kilogram") %>%
        units::set_units("metric_ton") %>%
        as.numeric(),
      emission == "g CH4"  ~ (value * gwp$ch4) %>%
        units::as_units("gram") %>%
        units::set_units("metric_ton") %>%
        as.numeric(),
      emission == "g N2O"  ~ (value * gwp$n2o) %>%
        units::as_units("gram") %>%
        units::set_units("metric_ton") %>%
        as.numeric(),
      TRUE ~ 0
    )
  )%>%
  group_by(fuel_category, `Fuel type`) %>%
  summarize(mt_co2e_mmbtu = sum(mt_co2e_mmbtu), .groups = "drop")


propane_ef <- fuel_ef_mmbtu %>%
  filter(`Fuel type` == "Propane") %>%
  pull(mt_co2e_mmbtu)

fueloil_ef <- fuel_ef_mmbtu %>%
  filter(`Fuel type` == "Kerosene") %>%
  pull(mt_co2e_mmbtu)

county_liquid_emissions <- county_fuel %>%
  mutate(category = "Building Energy",
         sector = "Residential") %>%
  pivot_longer(
    cols      = c(propane_mmBtu, fueloil_other_mmBtu),
    names_to  = "source",
    values_to = "mmbtu"
  ) %>%
  mutate(
    activity_type = "mmbtu",
    source          = case_when(
      source == "propane_mmBtu"       ~ "Propane",
      source == "fueloil_other_mmBtu" ~ "Fuel Oil & Other"
    ),
    ef              = case_when(
      source == "Propane"          ~ propane_ef,
      source == "Fuel Oil & Other" ~ fueloil_ef
    ),
    value_emissions = round(mmbtu * ef, digits = 2),
    units_emissions = "Metric tons CO2e"
  ) %>%
  select(-ef) %>% 
  rename(emissions_year = acs_year,
         activity = mmbtu)

saveRDS(county_liquid_emissions, "_energy/data/county_propane_fueloil_activity.RDS")
