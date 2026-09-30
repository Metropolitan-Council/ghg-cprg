# 03_compile_wi_natgas_activity.R
# ──────────────────────────────────────────────────────────────────────────────
# Build WI county x year natural gas activity + emissions (Pierce, St. Croix),
# 2005-2025, replacing wisconsin_natGas_estimate_2005_and_2021.R.
#
# Method, per utility (4220 NSP-WI, 3670 Midwest NG, 5230 SCV, 6650 WI Gas):
#   county_therms = gas_sold_therms_annual     x county customer share (G-26)
#                 + transport_in_scope_therms  x county customer share
#   - gas_sold_therms_annual: PSCW anchors exactly; non-anchor years either
#     linear (02 script) or weather-textured with an EIA index (below).
#   - transport_in_scope_therms: SCV only (all SCV customers are in-scope);
#     0 for other utilities, whose transport is excluded by design.
#   - County shares: interpolated PSCW G-26 customer shares (01 script).
#
# Weather texture (use_eia_texture = TRUE and EIA file present):
#   For each utility, ratio r = sales / EIA WI res+com consumption at anchor
#   years; r is interpolated linearly across years, then sales = r x index.
#   Anchor years are reproduced exactly; between anchors the series inherits
#   EIA's year-to-year (mostly weather) variation instead of a straight line.
#   Years with no EIA value fall back to the linear interpolation.
#
# Inputs:
#   wi_utility_natgas_activity_pcsw_interpolated.RDS  (02_compile_pcsw_...)
#   WI_utility_customer_shares_interpolated.RDS       (01_compile_wi_utility_...)
#   WI_eia_natgas_consumption_state.RDS               (02_compile_eia_..._consumption, optional)
#
# Outputs:
#   WI_county_natgas_activity_detail.RDS  utility x county x year, with components
#   WI_county_natgas_activity.RDS         emissions_year, county_name, mcf, data_source
#   WI_county_natgas_emissions.RDS        emissions_year, county_name, state, sector,
#                                         emissions_metric_tons_co2e
# ──────────────────────────────────────────────────────────────────────────────

source("R/_load_pkgs.R")
source("_energy/data-raw/_energy_emissions_factors.R")

use_eia_texture <- TRUE

# EIA: 1 Mcf = 1.038 MMBtu = 10.38 therms (https://www.eia.gov/tools/faqs/faq.php?id=45)
therms_per_mcf <- 10.38

# --- inputs -------------------------------------------------------------------

pcsw <- read_rds(here(
  "_energy", "data", "wi_utility_natgas_activity_pcsw_interpolated.RDS"
))

if (!"transport_in_scope_therms" %in% names(pcsw)) {
  stop("Re-run 02_compile_pcsw_wi_natgas_activity.R: transport_in_scope_therms missing.")
}

gas_shares <- read_rds(here(
  "_energy", "data", "WI_utility_customer_shares_interpolated.RDS"
)) %>%
  filter(fuel == "gas") %>%
  select(utility_id, county_name = county, year, county_share = county_share_interpolated)

# --- optional EIA weather texture --------------------------------------------

eia_path <- here("_energy", "data", "WI_eia_natgas_consumption_state.RDS")
apply_texture <- use_eia_texture && file.exists(eia_path)

if (use_eia_texture && !apply_texture) {
  warning("EIA consumption file not found; using linear interpolation only.")
}

if (apply_texture) {
  eia_index <- read_rds(eia_path) %>%
    select(year = emissions_year, eia_index = res_com_mmcf)

  pcsw <- pcsw %>%
    left_join(eia_index, by = "year") %>%
    group_by(utility_id) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(
      anchor_ratio = if_else(is_anchor_year, gas_sold_therms / eia_index, NA_real_),
      ratio_interp = if (sum(!is.na(anchor_ratio)) >= 2) {
        approx(
          x = year[!is.na(anchor_ratio)],
          y = anchor_ratio[!is.na(anchor_ratio)],
          xout = year, rule = 2
        )$y
      } else {
        NA_real_
      },
      gas_sold_therms_annual = case_when(
        is_anchor_year ~ gas_sold_therms,
        !is.na(ratio_interp) & !is.na(eia_index) ~ ratio_interp * eia_index,
        TRUE ~ gas_sold_therms_interpolated
      ),
      sales_method = case_when(
        is_anchor_year ~ "pscw_anchor",
        !is.na(ratio_interp) & !is.na(eia_index) ~ "eia_weather_textured",
        TRUE ~ "linear_interpolation"
      )
    ) %>%
    ungroup()

  # QA: textured anchors must reproduce PSCW exactly
  anchor_drift <- pcsw %>%
    filter(is_anchor_year) %>%
    filter(abs(gas_sold_therms_annual - gas_sold_therms) > 1)
  if (nrow(anchor_drift) > 0) stop("Weather texture altered anchor-year sales.")

  # QA: flag large departures from the straight line (possible index issue)
  texture_check <- pcsw %>%
    filter(sales_method == "eia_weather_textured") %>%
    mutate(pct_vs_linear = 100 * (gas_sold_therms_annual / gas_sold_therms_interpolated - 1))
  message(sprintf(
    "EIA texture: non-anchor years deviate from linear by %.1f%% to %.1f%%",
    min(texture_check$pct_vs_linear), max(texture_check$pct_vs_linear)
  ))
  if (any(abs(texture_check$pct_vs_linear) > 25)) {
    warning("Some textured years differ from linear by >25%; inspect texture_check.")
  }
} else {
  pcsw <- pcsw %>%
    mutate(
      gas_sold_therms_annual = gas_sold_therms_interpolated,
      sales_method = if_else(is_anchor_year, "pscw_anchor", "linear_interpolation")
    )
}

# --- allocate to counties -----------------------------------------------------

wi_county_natgas_activity_detail <- pcsw %>%
  select(
    utility_id, utility_name, year,
    gas_sold_therms_annual, transport_in_scope_therms, sales_method
  ) %>%
  inner_join(gas_shares, by = c("utility_id", "year")) %>%
  mutate(
    sales_therms = gas_sold_therms_annual * county_share,
    transport_therms = transport_in_scope_therms * county_share,
    therms = sales_therms + transport_therms,
    mcf = therms / therms_per_mcf
  ) %>%
  select(
    emissions_year = year, county_name, utility_id, utility_name,
    county_share, sales_therms, transport_therms, therms, mcf, sales_method
  ) %>%
  arrange(county_name, utility_name, emissions_year)

# QA: every utility-year should have at least one county row
missing_alloc <- pcsw %>%
  distinct(utility_id, year) %>%
  anti_join(gas_shares, by = c("utility_id", "year"))
if (nrow(missing_alloc) > 0) {
  warning("Utility-years with no county share:")
  print(missing_alloc)
}

# --- county summary (schema parallels WI_county_elec_activity.RDS) -----------

wi_county_natgas_activity <- wi_county_natgas_activity_detail %>%
  group_by(emissions_year, county_name) %>%
  summarise(
    mcf = sum(mcf),
    transport_mcf = sum(transport_therms) / therms_per_mcf,
    .groups = "drop"
  ) %>%
  mutate(
    data_source = if_else(
      apply_texture,
      "WI utility report (PSCW G-24 x G-26 customer share; EIA-textured interpolation)",
      "WI utility report (PSCW G-24 x G-26 customer share)"
    )
  ) %>%
  arrange(county_name, emissions_year)

# --- emissions ------------------------------------------------------------------

lb_to_mt <- 1 %>%
  units::as_units("pound") %>%
  units::set_units("metric_ton") %>%
  as.numeric()

wi_county_natgas_emissions <- wi_county_natgas_activity %>%
  mutate(
    co2_mt = mcf * epa_emissionsHub_naturalGas_factor_lbsCO2_perMCF * lb_to_mt,
    ch4_mt = mcf * epa_emissionsHub_naturalGas_factor_lbsCH4_perMCF * lb_to_mt,
    n2o_mt = mcf * epa_emissionsHub_naturalGas_factor_lbsN2O_perMCF * lb_to_mt,
    emissions_metric_tons_co2e = co2_mt + ch4_mt * gwp$ch4 + n2o_mt * gwp$n2o,
    state = "WI",
    sector = "Natural gas"
  ) %>%
  select(emissions_year, county_name, state, sector, emissions_metric_tons_co2e)

if (anyNA(wi_county_natgas_emissions$emissions_metric_tons_co2e)) {
  stop("NA natural gas emissions in WI county output.")
}

wi_county_natgas_activity %>%
  select(emissions_year, county_name, mcf) %>%
  pivot_wider(names_from = county_name, values_from = mcf) %>%
  print(n = Inf)

# --- write outputs -------------------------------------------------------------

write_rds(
  wi_county_natgas_activity_detail,
  here("_energy", "data", "wi_county_natgas_activity_detail.RDS")
)
write_rds(
  wi_county_natgas_activity,
  here("_energy", "data", "wi_county_natgas_activity.RDS")
)
write_rds(
  wi_county_natgas_emissions,
  here("_energy", "data", "wi_county_natgas_emissions.RDS")
)
