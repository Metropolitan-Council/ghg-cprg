#### pull in ctu mmbtu and convert to emissions
source("R/_load_pkgs.R")
source("R/global_warming_potential.R")


# ── Emissions factor ──────────────────────────────────────────────────────────

natgas_ef_scf <- readRDS("_meta/data/epa_ghg_factor_hub.RDS") %>%
  pluck("stationary_combustion") %>%
  filter(fuel_category == "Natural Gas" & per_unit == "scf") %>%
  mutate(
    mt_co2e_scf = case_when(
      emission == "kg CO2" ~ value %>%
        units::as_units("kilogram") %>%
        units::set_units("metric_ton") %>%
        as.numeric(),
      emission == "g CH4" ~ (value * gwp$ch4) %>%
        units::as_units("gram") %>%
        units::set_units("metric_ton") %>%
        as.numeric(),
      emission == "g N2O" ~ (value * gwp$n2o) %>%
        units::as_units("gram") %>%
        units::set_units("metric_ton") %>%
        as.numeric(),
      TRUE ~ 0
    ),
    mt_co2e_mcf = mt_co2e_scf * 1000
  ) %>%
  group_by(fuel_category, Source) %>%
  summarize(mt_co2e_mcf = sum(mt_co2e_mcf), .groups = "drop")

fuel_ef_mmbtu <- readRDS("_meta/data/epa_ghg_factor_hub.RDS") %>%
  pluck("stationary_combustion") %>%
  filter(`Fuel type` %in% c(
    "Natural Gas",
    "Kerosene",
    "Propane"
  ) &
    per_unit == "mmBtu") %>%
  mutate(
    mt_co2e_mmbtu = case_when(
      emission == "kg CO2" ~ value %>%
        units::as_units("kilogram") %>%
        units::set_units("metric_ton") %>%
        as.numeric(),
      emission == "g CH4" ~ (value * gwp$ch4) %>%
        units::as_units("gram") %>%
        units::set_units("metric_ton") %>%
        as.numeric(),
      emission == "g N2O" ~ (value * gwp$n2o) %>%
        units::as_units("gram") %>%
        units::set_units("metric_ton") %>%
        as.numeric(),
      TRUE ~ 0
    )
  ) %>%
  group_by(fuel_category, `Fuel type`) %>%
  summarize(mt_co2e_mmbtu = sum(mt_co2e_mmbtu), .groups = "drop")

# ── Load data ─────────────────────────────────────────────────────────────────

coctu_fuel <- read_rds("_energy/data-raw/ctu_ng_combined.rds")

coctu_res_ng <- coctu_fuel %>%
  filter(sector == "Residential") %>%
  mutate(
    mcf = ng_mmbtu / 1.037
  ) %>%
  select(coctu_id_gnis, ctu_name, ctu_class, county_name,
    emissions_year = inventory_year, sector, mcf, data_source
  )

coctu_res_liquid <- coctu_fuel %>%
  filter(sector == "Residential") %>%
  select(coctu_id_gnis, ctu_name, ctu_class, county_name,
    emissions_year = inventory_year, data_source,
    propane_mmbtu, fueloil_other_mmbtu
  )

# Combined natural gas data
coctu_ng <- coctu_fuel %>%
  mutate(mcf = ng_mmbtu / 1.037) %>%
  select(-c(propane_mmbtu, fueloil_other_mmbtu, total_res_mmbtu)) %>%
  rename(emissions_year = inventory_year)


# county data

county_mcf <- readRDS(here("_energy", "data", "county_natgas_activity.RDS")) %>%
  select(
    emissions_year,
    county_name,
    mcf_county = mcf_delivered
  ) %>%
  filter(!county_name %in% c("Chisago", "Sherburne", "St. Croix", "Pierce"))

# compare ctu aggregation to county data

county_comparison <- coctu_ng %>%
  filter(emissions_year >= 2010) %>%
  group_by(county_name, emissions_year) %>%
  summarize(mcf_ctu_sum = sum(mcf, na.rm = TRUE), .groups = "drop") %>%
  left_join(county_mcf, by = c("county_name", "emissions_year")) %>%
  mutate(
    scale_factor = mcf_county / mcf_ctu_sum,
    gap_mcf      = mcf_county - mcf_ctu_sum
  )

coctu_2010_2023 <- coctu_ng %>%
  filter(emissions_year %in% 2010:2023)


# stretching back to 2005 based on county data, keeping early RII data

# Each CTU's earliest 3 years of data
ctu_earliest_3 <- coctu_ng %>%
  group_by(coctu_id_gnis, ctu_name, ctu_class, county_name, sector) %>%
  arrange(emissions_year) %>%
  slice_head(n = 3) %>%
  ungroup()

# Sector sums as proportion of county_mcf totals (not CTU sums)
ctu_sector_props <- ctu_earliest_3 %>%
  group_by(coctu_id_gnis, ctu_name, county_name, emissions_year, sector) %>%
  summarize(sector_mcf = sum(mcf, na.rm = TRUE), .groups = "drop") %>%
  left_join(county_mcf, by = c("county_name", "emissions_year")) %>%
  mutate(sector_prop = sector_mcf / mcf_county) %>%
  group_by(coctu_id_gnis, ctu_name, county_name, sector) %>%
  summarize(mean_sector_prop = mean(sector_prop, na.rm = TRUE), .groups = "drop")

# Existing 2005-2009 actuals (RII, utility data)
existing_2005_2009 <- coctu_ng %>%
  filter(emissions_year %in% 2005:2009)

# Grid: all CTUs × 2005-2009, with county_mcf and proportions
coctu_2005_2009_filled <- ctu_sector_props %>%
  crossing(emissions_year = 2005:2009) %>%
  left_join(county_mcf, by = c("county_name", "emissions_year")) %>%
  mutate(
    mcf = mcf_county * mean_sector_prop,
    data_source = "County proportion (sector-anchored)"
  ) %>%
  filter(!is.na(mcf), mcf > 0) %>%
  # Keep only CTU-years without existing actuals
  anti_join(existing_2005_2009,
    by = c("coctu_id_gnis", "county_name", "sector", "emissions_year")
  ) %>%
  select(
    coctu_id_gnis, ctu_name, county_name,
    emissions_year, sector, mcf, data_source
  )

coctu_2005_2009 <- bind_rows(existing_2005_2009, coctu_2005_2009_filled)


ctu_ng_full <- bind_rows(
  coctu_2005_2009,
  coctu_2010_2023
) %>%
  group_by(
    coctu_id_gnis, ctu_name, ctu_class, county_name,
    sector, emissions_year, data_source
  ) %>%
  summarize(mcf = sum(mcf), .groups = "drop") %>%
  mutate(
    category = "Building Energy",
    source   = "Natural Gas"
  ) %>%
  arrange(ctu_name, ctu_class, county_name, sector, emissions_year)

ctu_ng_full %>%
  group_by(county_name, sector, emissions_year) %>%
  summarise(
    mcf = sum(mcf),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = emissions_year, y = mcf / 1e6, color = sector)) +
  geom_line() +
  geom_point(size = 1) +
  facet_wrap(~county_name, scales = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Natural Gas Deliveries by Sector", x = NULL, y = "MCF (millions)", color = "Sector") +
  theme_minimal()

stopifnot(
  "Duplicate CTU-sector-year rows found" =
    ctu_ng_full %>%
      count(coctu_id_gnis, ctu_name, ctu_class, county_name, sector, emissions_year) %>%
      filter(n > 1) %>%
      nrow() == 0
)

## cast backwards for liquid fuels by holding constant

liquid_2005_2009 <- coctu_res_liquid %>%
  filter(emissions_year == 2010) %>%
  select(-emissions_year) %>%
  crossing(emissions_year = 2005:2009) %>%
  mutate(data_source = "Flat carry (2010 anchor)")

coctu_liquid_full <- bind_rows(
  coctu_res_liquid %>% filter(emissions_year %in% 2010:2023),
  liquid_2005_2009
) %>%
  arrange(ctu_name, ctu_class, county_name, emissions_year)


# Convert natural gas to emissions

ctu_ng_emissions <- ctu_ng_full %>%
  cross_join(
    natgas_ef_scf %>% select(factor_source = Source, mt_co2e_mcf)
  ) %>%
  mutate(
    fuel_type       = "Natural Gas",
    value_emissions = mcf * mt_co2e_mcf,
    units_emissions = "Metric tons CO2e"
  )

# Convert liquid fuels to emissions

propane_ef <- fuel_ef_mmbtu %>%
  filter(`Fuel type` == "Propane") %>%
  pull(mt_co2e_mmbtu)

fueloil_ef <- fuel_ef_mmbtu %>%
  filter(`Fuel type` == "Kerosene") %>%
  pull(mt_co2e_mmbtu)

ctu_liquid_emissions <- coctu_liquid_full %>%
  mutate(category = "Building Energy") %>%
  pivot_longer(
    cols      = c(propane_mmbtu, fueloil_other_mmbtu),
    names_to  = "source",
    values_to = "mmbtu"
  ) %>%
  mutate(
    source = case_when(
      source == "propane_mmbtu" ~ "Propane",
      source == "fueloil_other_mmbtu" ~ "Fuel Oil & Other"
    ),
    ef = case_when(
      source == "Propane" ~ propane_ef,
      source == "Fuel Oil & Other" ~ fueloil_ef
    ),
    value_emissions = round(mmbtu * ef, digits = 2),
    units_emissions = "Metric tons CO2e"
  ) %>%
  select(-ef)

# ── Save ──────────────────────────────────────────────────────────────────────

saveRDS(ctu_ng_emissions, "_energy/data/_ctu_natgas_emissions.RDS")
saveRDS(ctu_liquid_emissions, "_energy/data/_ctu_liquid_emissions.RDS")
