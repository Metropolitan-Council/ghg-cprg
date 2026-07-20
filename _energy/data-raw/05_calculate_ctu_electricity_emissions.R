#### Pull in CTU MWh and convert to emissions
source("R/_load_pkgs.R")
source("R/global_warming_potential.R")

# ── Emissions factor (eGRID temporal) ─────────────────────────────────────────

egrid_temporal <- readRDS("_meta/data/epa_ghg_factor_hub.RDS") %>%
  pluck("egridTimeSeries") %>%
  mutate(
    mt_co2e_mwh = case_when(
      emission == "lb CH4" ~ value * gwp$ch4 %>%
        units::as_units("pound") %>%
        units::set_units("metric_ton") %>%
        as.numeric(),
      emission == "lb N2O" ~ value * gwp$n2o %>%
        units::as_units("pound") %>%
        units::set_units("metric_ton") %>%
        as.numeric(),
      emission == "lb CO2" ~ value %>%
        units::as_units("pound") %>%
        units::set_units("metric_ton") %>%
        as.numeric()
    )
  ) %>%
  group_by(Year, Source) %>%
  summarize(mt_co2e_mwh = sum(mt_co2e_mwh), .groups = "drop") %>%
  rename(emissions_year = Year)

# ── Load CTU combined data ────────────────────────────────────────────────────

coctu_elec <- read_rds("_energy/data-raw/ctu_elec_combined.rds") %>%
  rename(emissions_year = inventory_year)

# ── County activity data ──────────────────────────────────────────────────────

county_mwh <- readRDS(here::here("_energy", "data", "county_elec_activity.RDS")) %>%
  select(
    emissions_year,
    county_name,
    mwh_county = value_activity
  ) %>%
  filter(!county_name %in% c("Chisago", "Sherburne", "St. Croix", "Pierce"))

# ── County comparison ─────────────────────────────────────────────────────────

county_comparison <- coctu_elec %>%
  filter(emissions_year >= 2010) %>%
  group_by(county_name, emissions_year) %>%
  summarize(mwh_ctu_sum = sum(mwh, na.rm = TRUE), .groups = "drop") %>%
  left_join(county_mwh, by = c("county_name", "emissions_year")) %>%
  mutate(
    scale_factor = mwh_county / mwh_ctu_sum,
    gap_mwh      = mwh_county - mwh_ctu_sum
  )

coctu_2010_2023 <- coctu_elec %>%
  filter(emissions_year %in% 2010:2023)

# ── Backcast to 2005 using earliest 3 years of CTU data ───────────────────────
# Each CTU's earliest 3 years anchored against county totals to derive a
# stable sector proportion, then applied to county data for 2005-2009.
# Existing RII or utility data for 2005-2009 is preserved.

ctu_earliest_3 <- coctu_elec %>%
  group_by(coctu_id_gnis, ctu_name, ctu_class, county_name, sector) %>%
  arrange(emissions_year) %>%
  slice_head(n = 3) %>%
  ungroup()

ctu_sector_props <- ctu_earliest_3 %>%
  group_by(coctu_id_gnis, ctu_name, county_name, emissions_year, sector) %>%
  summarize(sector_mwh = sum(mwh, na.rm = TRUE), .groups = "drop") %>%
  left_join(county_mwh, by = c("county_name", "emissions_year")) %>%
  mutate(sector_prop = sector_mwh / mwh_county) %>%
  group_by(coctu_id_gnis, ctu_name, county_name, sector) %>%
  summarize(mean_sector_prop = mean(sector_prop, na.rm = TRUE), .groups = "drop")

# Existing 2005-2009 actuals (RII, utility data)
existing_2005_2009 <- coctu_elec %>%
  filter(emissions_year %in% 2005:2009)

# Grid: all CTUs × 2005-2009, with county_mwh and proportions
coctu_2005_2009_filled <- ctu_sector_props %>%
  crossing(emissions_year = 2005:2009) %>%
  left_join(county_mwh, by = c("county_name", "emissions_year")) %>%
  mutate(
    mwh = mwh_county * mean_sector_prop,
    data_source = "County proportion (sector-anchored)"
  ) %>%
  filter(!is.na(mwh), mwh > 0) %>%
  # Keep only CTU-years without existing actuals
  anti_join(existing_2005_2009,
            by = c("coctu_id_gnis", "county_name", "sector", "emissions_year")
  ) %>%
  select(
    coctu_id_gnis, ctu_name, county_name,
    emissions_year, sector, mwh, data_source
  )

coctu_2005_2009 <- bind_rows(existing_2005_2009, coctu_2005_2009_filled)

# ── Combine full series ───────────────────────────────────────────────────────

ctu_elec_full <- bind_rows(
  coctu_2005_2009,
  coctu_2010_2023
) %>%
  group_by(
    coctu_id_gnis, ctu_name, ctu_class, county_name,
    sector, emissions_year, data_source
  ) %>%
  summarize(mwh = sum(mwh), .groups = "drop") %>%
  mutate(
    category = "Building Energy",
    source   = "Electricity"
  ) %>%
  arrange(ctu_name, ctu_class, county_name, sector, emissions_year)

# ── Diagnostic plot ───────────────────────────────────────────────────────────

ctu_elec_full %>%
  group_by(county_name, sector, emissions_year) %>%
  summarise(mwh = sum(mwh), .groups = "drop") %>%
  ggplot(aes(x = emissions_year, y = mwh / 1e6, color = sector)) +
  geom_line() +
  geom_point(size = 1) +
  facet_wrap(~county_name, scales = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Electricity Deliveries by Sector",
    x = NULL, y = "MWh (millions)", color = "Sector"
  ) +
  theme_minimal()

stopifnot(
  "Duplicate CTU-sector-year rows found" =
    ctu_elec_full %>%
    count(coctu_id_gnis, ctu_name, ctu_class, county_name, sector, emissions_year) %>%
    filter(n > 1) %>%
    nrow() == 0
)

# ── Convert to emissions ──────────────────────────────────────────────────────

ctu_elec_emissions <- ctu_elec_full %>%
  cross_join(
    egrid_temporal %>% select(emissions_year, factor_source = Source, mt_co2e_mwh)
  ) %>%
  filter(emissions_year.x == emissions_year.y) %>%
  rename(emissions_year = emissions_year.x) %>%
  select(-emissions_year.y) %>%
  mutate(
    value_emissions = round(mwh * mt_co2e_mwh, digits = 2),
    units_emissions = "Metric tons CO2e"
  )

# ── Save ──────────────────────────────────────────────────────────────────────

saveRDS(ctu_elec_emissions, "_energy/data/_ctu_electricity_emissions.RDS")
