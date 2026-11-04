#### import and compile existing utility data

source("R/_load_pkgs.R")
source("_energy/data-raw/_energy_emissions_factors.R")

### load in from SQL
elec_view <- import_from_emissions("metro_energy.vw_utility_electricity_by_ctu")
ng_view <- import_from_emissions("metro_energy.vw_utility_natural_gas_by_ctu")

### determine if any cities only have "Total" (mostly looks like aggregation i.e. duplicate)

elec_view %>%
  distinct(ctu_name, year) %>%
  nrow() # 690
ng_view %>%
  distinct(ctu_name, year) %>%
  nrow() # 694

elec_view %>%
  filter(customer_class_name != "Total") %>%
  distinct(ctu_name, year) %>%
  nrow() # 676

ng_view %>%
  filter(customer_class_name != "Total") %>%
  distinct(ctu_name, year) %>%
  nrow() # 526

## remove totals where alternatives exist

elec_view_filtered <- elec_view %>%
  group_by(ctu_name, year) %>%
  filter(!(customer_class_name == "Total" & n() > 1)) %>%
  ungroup()

ng_view_filtered <- ng_view %>%
  group_by(ctu_name, year) %>%
  filter(!(customer_class_name == "Total" & n() > 1)) %>%
  ungroup()

### apply emission factors

electricity_emissions <- elec_view_filtered %>%
  left_join(epa_ghg_factor_hub$egridTimeSeries, ## yearly egrid series
    by = c("year" = "Year")
  ) %>%
  mutate(
    value_emissions = mwh_per_year * value %>%
      units::as_units("pound") %>%
      units::set_units("metric_ton") %>%
      as.numeric(),
    units_emissions = str_replace_all(emission, "lb", "Metric tons"),
    mt_co2e = case_when(
      grepl("CH4", units_emissions) ~ value_emissions * GWP_CH4,
      grepl("N2O", units_emissions) ~ value_emissions * GWP_N2O,
      TRUE ~ value_emissions
    )
  ) %>%
  select(ctu_name,
    emissions_year = year, customer_class = customer_class_name,
    data_source = utility_name, mwh_per_year, number_of_customers,
    factor_source = Source, value_emissions, units_emissions, mt_co2e
  )

ng_emissions <- ng_view_filtered %>%
  cross_join(epa_ghg_factor_hub$stationary_combustion %>%
    filter(
      fuel_category == "Natural Gas",
      per_unit == "mmBtu"
    )) %>%
  mutate(
    value_emissions = case_when(
      emission == "kg CO2" ~ therms_per_year * 10^-1 * value %>% # therm is 100,000 btus
        units::as_units("kilogram") %>%
        units::set_units("metric_ton") %>%
        as.numeric(),
      TRUE ~ therms_per_year * 10^-5 * value %>% # therm is 100,000 btus
        units::as_units("gram") %>%
        units::set_units("metric_ton") %>%
        as.numeric()
    ),
    units_emissions = str_replace_all(emission, "^[^ ]+", "Metric tons"),
    mt_co2e = case_when(
      grepl("CH4", units_emissions) ~ value_emissions * GWP_CH4,
      grepl("N2O", units_emissions) ~ value_emissions * GWP_N2O,
      TRUE ~ value_emissions
    )
  ) %>%
  select(ctu_name,
    emissions_year = year, customer_class = customer_class_name,
    data_source = utility_name, therms_per_year, number_of_customers,
    factor_source = Source, value_emissions, units_emissions, mt_co2e
  )

electricity_emissions

electricity_emissions_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "ctu_name", class(electricity_emissions$ctu_name), "City or township name",
    "emissions_year", class(electricity_emissions$emissions_year), "Year",
    "customer_class", class(electricity_emissions$customer_class), "Emissions sector",
    "data_source", class(electricity_emissions$data_source), "Name utility that provided activity data",
    "mwh_per_year", class(electricity_emissions$mwh_per_year), "Activity data: mWh deliverd in that year",
    "number_of_customers", class(electricity_emissions$number_of_customers), "Number of customers receiving deliveries",
    "factor_source", class(electricity_emissions$factor_source), "Emissions factor data source",
    "value_emissions", class(electricity_emissions$value_emissions), "Numerical value of emissions",
    "units_emissions", class(electricity_emissions$units_emissions), "Units and gas type of emissions",
    "mt_co2e", class(electricity_emissions$mt_co2e), "Metric tons of gas in CO2 equivalency"
  )

saveRDS(electricity_emissions, "./_energy/data/ctu_electricity_emissions_2015_2018.rds")
saveRDS(electricity_emissions_meta, "./_energy/data/electricity_emissions_meta.rds")

ng_emissions_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "ctu_name", class(ng_emissions$ctu_name), "City or township name",
    "emissions_year", class(ng_emissions$emissions_year), "Year",
    "customer_class", class(ng_emissions$customer_class), "Emissions sector",
    "data_source", class(ng_emissions$data_source), "Name utility that provided activity data",
    "therms_per_year", class(ng_emissions$therms_per_year), "Activity data: therms delivered in that year",
    "number_of_customers", class(ng_emissions$number_of_customers), "Number of customers receiving deliveries",
    "factor_source", class(ng_emissions$factor_source), "Emissions factor data source",
    "value_emissions", class(ng_emissions$value_emissions), "Numerical value of emissions",
    "units_emissions", class(ng_emissions$units_emissions), "Units and gas type of emissions",
    "mt_co2e", class(ng_emissions$mt_co2e), "Metric tons of gas in CO2 equivalency"
  )

saveRDS(ng_emissions, "./_energy/data/ctu_ng_emissions_2015_2018.rds")
saveRDS(ng_emissions_meta, "./_energy/data/ng_emissions_meta.rds")
