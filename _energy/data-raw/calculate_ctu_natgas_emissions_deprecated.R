#### pull in ctu mcf and convert to emissions
source("R/_load_pkgs.R")
source("R/global_warming_potential.R")


# ── Emissions factor ──────────────────────────────────────────────────────────

natgas_ef_scf <- readRDS("_meta/data/epa_ghg_factor_hub.RDS") %>%
  pluck("stationary_combustion") %>%
  filter(fuel_category == "Natural Gas" & per_unit == "scf") %>%
  mutate(
    mt_co2e_mcf = 10^3 * case_when(
      emission == "g CH4" ~ value * gwp$ch4 %>%
        units::as_units("gram") %>%
        units::set_units("metric_ton") %>%
        as.numeric(),
      emission == "g N2O" ~ value * gwp$n2o %>%
        units::as_units("gram") %>%
        units::set_units("metric_ton") %>%
        as.numeric(),
      emission == "kg CO2" ~ value %>%
        units::as_units("kilogram") %>%
        units::set_units("metric_ton") %>%
        as.numeric(),
      TRUE ~ 0
    )
  ) %>%
  group_by(fuel_category, Source) %>%
  summarize(mt_co2e_mcf = sum(mt_co2e_mcf), .groups = "drop")

# ── Load data ─────────────────────────────────────────────────────────────────

coctu_busi <- read_rds("_energy/data-raw/predicted_coctu_business_mcf.rds") %>%
  mutate(sector = "Business") %>%
  rename(mcf = business_mcf)

coctu_res <- read_rds("_energy/data-raw/predicted_coctu_residential_mcf.rds") %>%
  mutate(sector = "Residential") %>%
  rename(mcf = residential_mcf)

coctu_mcf <- bind_rows(coctu_busi, coctu_res)

county_mcf <- readRDS(here("_energy", "data", "county_natgas_activity.RDS")) %>%
  select(
    inventory_year = year,
    county_name,
    mcf_county    = activity,
    county_source = data_source
  ) %>%
  filter(!county_name %in% c("Chisago", "Sherburne", "St. Croix", "Pierce"))

# ── Phase 1: 2014-2023 ───────────────────────────────────────────────────────
# Real county totals available -- scale combined CTU predictions to match

ctu_county_sums_p1 <- coctu_mcf %>%
  filter(inventory_year >= 2014) %>%
  group_by(county_name, inventory_year) %>%
  summarize(mcf_ctu_sum = sum(mcf, na.rm = TRUE), .groups = "drop")

county_scale_factors <- ctu_county_sums_p1 %>%
  left_join(county_mcf, by = c("county_name", "inventory_year")) %>%
  mutate(scale_factor = mcf_county / mcf_ctu_sum)

# diagnostic: flag implausible scale factors
county_scale_factors %>%
  filter(scale_factor < 0.5 | scale_factor > 2) %>%
  select(county_name, inventory_year, mcf_ctu_sum, mcf_county, scale_factor) %>%
  arrange(desc(abs(scale_factor - 1))) %>%
  print()

coctu_phase1 <- coctu_mcf %>%
  filter(inventory_year >= 2014) %>%
  left_join(
    county_scale_factors %>% select(county_name, inventory_year, scale_factor),
    by = c("county_name", "inventory_year")
  ) %>%
  mutate(
    mcf         = mcf * scale_factor,
    data_source = if_else(
      data_source == "Utility report",
      "Utility report (county-scaled)",
      paste0(data_source, " (county-scaled)")
    )
  ) %>%
  select(-scale_factor)

# ── Phase 2: 2010-2013 ───────────────────────────────────────────────────────
# CTU-level data exists but county totals are interpolated.
# Use CTU aggregate trajectory as a shape function, anchored at
# 2010 (interpolated) and 2014 (first real county year) endpoints.

ctu_shape_2010_2013 <- coctu_mcf %>%
  filter(inventory_year %in% 2010:2013) %>%
  group_by(county_name, inventory_year) %>%
  summarize(mcf_ctu_sum = sum(mcf, na.rm = TRUE), .groups = "drop")

# county anchor values at window boundaries
county_anchors <- county_mcf %>%
  filter(inventory_year %in% c(2010, 2014)) %>%
  select(county_name, inventory_year, mcf_county) %>%
  pivot_wider(
    names_from  = inventory_year,
    values_from = mcf_county,
    names_prefix = "mcf_"
  ) %>%
  rename(mcf_anchor_start = mcf_2010, mcf_anchor_end = mcf_2014)

# CTU shape values at anchor years for normalization
ctu_shape_anchors <- ctu_shape_2010_2013 %>%
  filter(inventory_year %in% c(2010, 2013)) %>%
  pivot_wider(
    names_from  = inventory_year,
    values_from = mcf_ctu_sum,
    names_prefix = "shape_"
  ) %>%
  rename(shape_start = shape_2010, shape_end = shape_2013)

ctu_shape_anchored <- ctu_shape_2010_2013 %>%
  left_join(county_anchors,     by = "county_name") %>%
  left_join(ctu_shape_anchors,  by = "county_name") %>%
  mutate(
    t = (inventory_year - 2010) / (2014 - 2010),
    # linearly morph scale factor from start to end anchor
    # applied to the CTU shape so trajectory is preserved
    mcf_county_shaped = mcf_ctu_sum * (
      (1 - t) * (mcf_anchor_start / shape_start) +
        t  * (mcf_anchor_end   / shape_end)
    )
  ) %>%
  select(county_name, inventory_year, mcf_county_shaped)

# distribute shaped county total back to CTUs proportionally
coctu_phase2 <- coctu_mcf %>%
  filter(inventory_year %in% 2010:2013) %>%
  left_join(ctu_shape_anchored, by = c("county_name", "inventory_year")) %>%
  group_by(county_name, inventory_year) %>%
  mutate(
    ctu_prop = mcf / sum(mcf, na.rm = TRUE),
    mcf      = mcf_county_shaped * ctu_prop,
    data_source = paste0(data_source, " (county-shaped)")
  ) %>%
  ungroup() %>%
  select(-c(mcf_county_shaped, ctu_prop))

# ── Phase 3: 2005-2009 ───────────────────────────────────────────────────────
# No CTU-level data. Derive each CTU's mean proportion of county total
# from earliest reliable CTU window (2010-2013), then apply to county totals.
# 2005 is a real county utility report; 2006-2009 are interpolated.

ctu_early_props <- coctu_mcf %>%
  filter(inventory_year %in% 2010:2013) %>%
  group_by(county_name, inventory_year) %>%
  mutate(county_total = sum(mcf, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(ctu_prop = mcf / county_total) %>%
  group_by(coctu_id_gnis, ctu_name, ctu_class, county_name, sector) %>%
  summarize(mean_ctu_prop = mean(ctu_prop, na.rm = TRUE), .groups = "drop")

coctu_phase3 <- county_mcf %>%
  filter(inventory_year %in% 2005:2009) %>%
  left_join(ctu_early_props,
            by      = "county_name",
            relationship = "many-to-many"
  ) %>%
  mutate(
    mcf = mcf_county * mean_ctu_prop,
    data_source = case_when(
      county_source == "Utility report" ~
        "County proportion (CTU-informed, utility anchor)",
      TRUE ~
        "County proportion (CTU-informed, interpolated)"
    )
  ) %>%
  filter(!is.na(mcf), mcf > 0) %>%
  select(coctu_id_gnis, ctu_name, ctu_class, county_name,
         inventory_year, sector, mcf, data_source)

# ── Combine all phases ────────────────────────────────────────────────────────

ctu_full <- bind_rows(
  coctu_phase1,
  coctu_phase2,
  coctu_phase3
) %>%
  group_by(coctu_id_gnis, ctu_name, ctu_class, county_name,
           sector, inventory_year, data_source) %>%
  summarize(mcf = sum(mcf), .groups = "drop") %>%
  mutate(
    category = "Building Energy",
    source   = "Natural Gas"
  ) %>%
  arrange(ctu_name, ctu_class, county_name, sector, inventory_year)

# sanity checks
stopifnot(
  # no duplicate ctu-sector-year rows
  ctu_full %>%
    count(coctu_id_gnis, ctu_name, ctu_class, county_name,
          sector, inventory_year) %>%
    filter(n > 1) %>%
    nrow() == 0
)

# check phase 1 county sums match county_mcf within rounding
ctu_full %>%
  filter(inventory_year >= 2014) %>%
  group_by(county_name, inventory_year) %>%
  summarize(mcf_ctu_sum = sum(mcf), .groups = "drop") %>%
  left_join(county_mcf, by = c("county_name", "inventory_year")) %>%
  mutate(diff = abs(mcf_ctu_sum - mcf_county)) %>%
  filter(diff > 1) %>%
  arrange(desc(diff)) %>%
  print()

# ── Convert to emissions ──────────────────────────────────────────────────────

ctu_emissions <- ctu_full %>%
  cross_join(
    natgas_ef_scf %>% select(factor_source = Source, mt_co2e_mcf)
  ) %>%
  mutate(
    value_emissions = round(mcf * mt_co2e_mcf, digits = 2),
    units_emissions = "Metric tons CO2e"
  )

saveRDS(ctu_emissions, "_energy/data/_ctu_natgas_emissions.RDS")
