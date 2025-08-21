#### This script processes partial 2005-2021 data received from the
#### Metropolitan Airport Commission for aviation fuel emissions

source("R/_load_pkgs.R")
source("R/global_warming_potential.R")

### load in EPA ef hub data
aviation_ef <- read_rds("_meta/data/epa_ghg_factor_hub.RDS") %>%
  pluck("stationary_combustion") %>%
  filter(`Fuel type` == "Kerosene-Type Jet Fuel" & per_unit == "gallon" & emission != "mmBtu")


### load in partial data from MAC and
mac_emissions <- read_csv("_transportation/data-raw/aviation/mac_emissions.csv")

### calculate emissions from fuel provisioned at MAC in 2005 and 2021
metc_emissions <- mac_emissions %>%
  filter(!is.na(Fuel_dispensed_gallons)) %>%
  cross_join(., aviation_ef) %>%
  mutate(
    mt_gas = case_when(
      # CO2 emissions factor is reported in kilograms per gallon
      grepl("CO2", emission) ~ Fuel_dispensed_gallons * value %>%
        units::as_units("kilogram") %>%
        units::set_units("metric_ton") %>%
        as.numeric(),
      # all others are reported in grams per gallon
      grepl("CH4", emission) ~ Fuel_dispensed_gallons * value %>%
        units::as_units("gram") %>%
        units::set_units("metric_ton") %>%
        as.numeric(),
      grepl("N2O", emission) ~ Fuel_dispensed_gallons * value %>%
        units::as_units("gram") %>%
        units::set_units("metric_ton") %>%
        as.numeric(),
    ),
    mt_co2e = case_when(
      grepl("CO2", emission) ~ mt_gas,
      grepl("CH4", emission) ~ mt_gas * gwp$ch4,
      grepl("N2O", emission) ~ mt_gas * gwp$n2o,
    )
  )

### create MSP emissions, selecting calc from fuel where possible
msp_emissions <- mac_emissions %>%
  mutate(mac_emissions = LTO_CO2e + Cruise_CO2e) %>%
  left_join(., metc_emissions %>%
    group_by(year) %>%
    summarize(mt_co2e = sum(mt_co2e))) %>%
  mutate(msp_mt_co2e = if_else(is.na(mt_co2e), mac_emissions, mt_co2e)) %>%
  select(inventory_year = year, msp_mt_co2e)

### load in state aviation data
mpca_aviation <- 
  read_rds("_meta/data/mpca_ghg_inv_2022.RDS") %>%
  filter(Source == "Aviation") %>%
  mutate(inventory_year = as.numeric(inventory_year)) %>% 
  group_by(Sector, Source, inventory_year) %>% 
  summarise(state_mt_co2e = sum(co2e))

# explore ways of interpolating NAs in MSP emissions
aviation_emissions <- full_join(mpca_aviation, msp_emissions) %>%
  mutate(msp_proportion = msp_mt_co2e / state_mt_co2e) %>%
  # first method just uses time series imputation between missing msp values
  mutate(
    msp_mt_co2e_impute = na_kalman(msp_mt_co2e),
    msp_proportion_impute = na_kalman(msp_proportion),
    # second methods uses time series imputation between missing msp_proportion values,
    # then multiplies imputed proportion by state value
    msp_mt_co2e_state_prop = if_else(is.na(msp_mt_co2e),
      msp_proportion_impute * state_mt_co2e,
      msp_mt_co2e
    ),
    ### interpolating directly from MSP values leads to higher aviation estimates in
    ### MSP than entire state from 2010-2012, which is obviously problematic
    ### Using the imputing proportion step as a result
    Sector = "Transportation",
    Subsector = "Aviation"
  )

aviation_emissions_data_source_compare <- aviation_emissions

aviation_out <- aviation_emissions %>%
  select(sector = Sector, source = Source, inventory_year, value_emissions = msp_mt_co2e_state_prop) %>%
  mutate(
    category = "Off-road",
    geog_name = "MSP Airport",
    units_emissions = "Metric tons CO2e",
    data_source = case_when(
      inventory_year %in% c(2005, 2021) ~ "Metropolitan Airport Commission: fuel dispersement reporting",
      inventory_year %in% c(2016:2020) ~ "Metropolitan Airport Commission: GHG emission reporting",
      inventory_year %in% c(2006:2015, 2022) ~ "MPCA state aviation emission proportional analysis"
    ),
    factor_source = "EPA GHG Emission Factor Hub"
  )

aviation_emissions_meta <- tibble::tribble(
  ~"Column", ~"Class", ~"Description",
  "sector", class(aviation_out$sector), "Emissions sector",
  "category", class(aviation_out$category), "Emissions subsector category",
  "source", class(aviation_out$source), "Emissions source",
  "inventory_year", class(aviation_out$inventory_year), "Inventory year of emissions",
  "value_emissions", class(aviation_out$value_emissions), "Numeric value of emissions",
  "units_emissions", class(aviation_out$units_emissions), "Units of emissions",
  "geog_name", class(aviation_out$geog_name), "Geographic location",
  "data_source", class(aviation_out$data_source), "Source of activity data used to calculate emissions",
  "factor_source", class(aviation_out$factor_source), "Source of emission factor for translating activity to emissions"
)

saveRDS(aviation_out, "./_transportation/data/aviation_emissions.rds")
saveRDS(aviation_emissions_meta, "./_transportation/data/aviation_emissions_meta.rds")
saveRDS(aviation_emissions_data_source_compare, "_transportation/data/aviation_emissions_data_source_compare.RDS")
