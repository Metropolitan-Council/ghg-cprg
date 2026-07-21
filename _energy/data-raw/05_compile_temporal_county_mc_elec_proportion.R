### Electricity county sector proportioning
### Decomposition:
###   county total (EIA/7610)
###     - residential (CTU model aggregation)
###     = non-residential remainder
###       → split into Commercial / Industrial via NREL SLOPE ratios

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

# ── CTU electricity data ──────────────────────────────────────────────────────

ctu_elec <- readRDS("_energy/data/_ctu_electricity_emissions.RDS")

# ── County activity data ──────────────────────────────────────────────────────

county_mwh <- readRDS(here::here("_energy", "data", "county_elec_activity.RDS")) %>%
  select(
    emissions_year,
    county_name,
    mwh_county = value_activity
  ) %>%
  filter(!county_name %in% c("Chisago", "Sherburne", "St. Croix", "Pierce"))

# ── NREL SLOPE: commercial / industrial ratio ─────────────────────────────────
# Used only for splitting non-residential electricity into C and I.
# Adjust source/sector filter names to match your nrel_emissions.RDS schema.

nrel_ci_ratio <- readRDS("_energy/data-raw/nrel_slope/nrel_emissions_inv_county.RDS") %>%
  filter(
    source == "Electricity",
    sector_raw %in% c("commercial", "industrial")
  ) %>%
  group_by(county_name, sector_raw) %>%
  summarize(mwh = mean(consumption_mwh, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = sector_raw, values_from = mwh) %>%
  mutate(
    commercial_share = commercial / (commercial + industrial),
    industrial_share = industrial / (commercial + industrial)
  ) %>%
  select(county_name, commercial_share, industrial_share)


# ── Aggregate CTU residential to county level ─────────────────────────────────

county_res <- ctu_elec %>%
  filter(sector == "Residential") %>%
  group_by(county_name, emissions_year) %>%
  summarize(Residential = sum(mwh, na.rm = TRUE), .groups = "drop")

# ── Build wide county table ───────────────────────────────────────────────────

county_wide <- county_mwh %>%
  left_join(county_res, by = c("county_name", "emissions_year")) %>%
  mutate(
    Residential    = replace_na(Residential, 0),
    non_residential = mwh_county - Residential,
    # Guard against negative remainder (CTU residential > county total)
    non_residential = pmax(non_residential, 0)
  ) %>%
  left_join(nrel_ci_ratio, by = "county_name") %>%
  mutate(
    Commercial = non_residential * commercial_share,
    Industrial = non_residential * industrial_share
  )

# ── Diagnostics ───────────────────────────────────────────────────────────────

# Residual check: how does CTU residential compare to county total?
county_wide %>%
  mutate(res_share = Residential / mwh_county) %>%
  filter(emissions_year == 2022) %>%
  select(county_name, mwh_county, Residential, res_share) %>%
  arrange(res_share) %>%
  print(n = Inf)

# Stacked bar for 2022
county_wide %>%
  filter(emissions_year == 2022) %>%
  select(county_name, Residential, Commercial, Industrial) %>%
  pivot_longer(Residential:Industrial, names_to = "sector", values_to = "mwh") %>%
  ggplot(aes(x = county_name, y = mwh / 1e6, fill = sector)) +
  geom_col() +
  scale_fill_manual(values = c(
    "Residential" = "#2166AC",
    "Commercial"  = "#B2182B",
    "Industrial"  = "#F4A582"
  )) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "2022 Electricity: Sector Breakdown",
    x = NULL, y = "MWh (millions)", fill = "Sector"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Time series by sector
county_wide %>%
  select(county_name, emissions_year, Residential, Commercial, Industrial) %>%
  pivot_longer(Residential:Industrial, names_to = "sector", values_to = "mwh") %>%
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

# ── Final output ─────────────────────────────────────────────────────────────

county_mwh_activity <- county_wide %>%
  select(county_name, emissions_year, Residential, Commercial, Industrial) %>%
  pivot_longer(Residential:Industrial, names_to = "sector", values_to = "value_activity") %>%
  mutate(
    category      = "Electricity",
    source        = paste(sector, "electricity"),
    unit_activity = "MWh"
  )

# ── Convert to emissions ──────────────────────────────────────────────────────

county_elec_emissions <- county_mwh_activity %>%
  left_join(egrid_temporal, by = "emissions_year") %>%
  mutate(
    value_emissions = round(value_activity * mt_co2e_mwh, digits = 2),
    unit_emissions = "Metric tons CO2e",
    factor_source   = Source,
    data_source = case_when(
      sector == "Residential" ~ "Met Council CTU modeling",
      sector %in% c("Commercial", "Industrial") ~
        "County remainder split by NREL SLOPE ratios"
    )
  ) %>%
  select(-mt_co2e_mwh, -Source)

# ── Save ──────────────────────────────────────────────────────────────────────

write_rds(
  county_mwh_activity,
  here("_energy", "data", "county_elec_activity_by_sector.RDS")
)

write_rds(
  county_elec_emissions,
  here("_energy", "data", "county_elec_emissions_by_sector.RDS")
)
