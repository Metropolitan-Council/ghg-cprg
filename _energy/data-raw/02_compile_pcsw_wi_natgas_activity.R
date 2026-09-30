# Wisconsin IOU gas delivered by utility × year
# Source: PSCW Annual Reports, Schedule G-24 "Summary of Gas Account & System Load Statistics"
# (Schedule G-23 in the 2005 report format)
# PDFs can be accessed at https://apps.psc.wi.gov/ARS/annualReports/content/listingIOU.aspx
#
# This script stores gas throughput data separated into two categories:
#   - gas_sold_therms: therms sold to customers (Wisconsin column, "Gas sold (including
#     interdepartmental)" line). This is the utility's own gas moved through its system
#     and billed to end customers.
#   - transport_therms: therms delivered on behalf of third-party gas suppliers (Wisconsin
#     column, "Transport gas delivered" line). These are typically large industrial
#     customers who buy gas from a third party and pay the utility only for delivery.
#
# For county-level GHG inventory:
#   - gas_sold_therms should be apportioned using G-26 customer counts, since the customer
#     base is broadly distributed.
#   - transport_therms should NOT be apportioned by residential customer counts, since it
#     concentrates at a small number of large industrial sites. For Pierce and St. Croix
#     counties (predominantly residential/small-commercial), transport gas is typically
#     absent or negligible and can be omitted unless a known large industrial customer
#     exists there.
#   - EXCEPTION: St Croix Valley Natural Gas (5230). All SCV customers are in Pierce or
#     St. Croix (G-26), so its transport gas is necessarily delivered in-scope. SCV
#     transport is zero through 2015 and begins in 2016 (1,149,446 therms), rising
#     thereafter (~12k t CO2 by 2022, below the GHGRP 25k t threshold). It is carried as
#     an annual in-scope series (transport_in_scope_therms). Transport for NSP-WI,
#     Midwest Natural Gas and Wisconsin Gas remains excluded.
#
# Data notes:
#   - Wisconsin column is used throughout (excludes any Michigan operations).
#   - 2005 uses schedule G-23; 2013+ uses G-24. Both have identical column structure.
#   - 2005 and 2021 gas-only utilities (3670, 5230, 6650) are pulled from the previous
#   R script (wisconsin_natGas_estimate_2005_and_2021.R) where PDFs were not re-verified.
#     Transport for 3670 and 6650 in 2021 remains NA (not used downstream).
#
# Annual estimates:
#   gas_sold_therms is linearly interpolated between reported anchor years for
#   each utility. Anchor observations remain separately identifiable in the
#   annual output. Weather texturing of the interpolated years (EIA index) is
#   applied downstream in 03_compile_wi_natgas_activity.R.
#   SCV transport is linearly interpolated from its own anchor set (which adds
#   the 2015 = 0 / 2016 = 1,149,446 onset years) into transport_in_scope_therms;
#   it is 0 for all other utilities by design.
#
# Outputs:
#   wi_utility_natgas_activity_pcsw.RDS
#     Reported PSCW anchor-year values, including transport where available.
#   wi_utility_natgas_activity_pcsw_interpolated.RDS
#     Annual 2005-2025 utility sales estimates, interpolated from anchors,
#     plus transport_in_scope_therms (SCV only; 0 elsewhere).

source("R/_load_pkgs.R")

wi_iou_gas_delivered <- tribble(
  ~utility_id, ~utility_name,                              ~year, ~gas_sold_therms, ~transport_therms, ~source_schedule,
  # ---- 2005 (schedule G-23) ----
  4220, "Northern States Power Company - Wisconsin",  2005,        138281245,        51531175, "G-23 WI col",
  3670, "Midwest Natural Gas Incorporated",           2005,         18816267,               0, "G-23 WI col",
  5230, "St Croix Valley Natural Gas Company",        2005,          9534249,               0,"G-23 WI col",
  6650, "Wisconsin Gas",                              2005,        728522194,       537900653, "G-23 WI col",
  
  # ---- 2013 (schedule G-24) ----
  4220, "Northern States Power Company - Wisconsin",  2013,        171384590,        46661850, "G-24 WI col",
  3670, "Midwest Natural Gas Incorporated",           2013,         22624435,               0, "G-24 WI col",
  5230, "St Croix Valley Natural Gas Company",        2013,         12622380,               0, "G-24 WI col",
  6650, "Wisconsin Gas",                              2013,        792947359,       725182417, "G-24 WI col",
  
  # ---- 2021 (schedule G-24) ----
  4220, "Northern States Power Company - Wisconsin",  2021,        171102649,        51506030, "G-24 WI col",
  3670, "Midwest Natural Gas Incorporated",           2021,         23181392,              NA, "prior R script",
  5230, "St Croix Valley Natural Gas Company",        2021,         11155826,        2111088 , "G-24 WI col",
  6650, "Wisconsin Gas",                              2021,        751394716,              NA, "prior R script",
  
  # ---- 2022 (schedule G-24) ----
  4220, "Northern States Power Company - Wisconsin",  2022,        175928658,        56719049, "G-24 WI col",
  3670, "Midwest Natural Gas Incorporated",           2022,         27126082,          943315, "G-24 WI col",
  5230, "St Croix Valley Natural Gas Company",        2022,         13036369,         2268334, "G-24 WI col",
  6650, "Wisconsin Gas",                              2022,        833401892,      1235123546, "G-24 WI col",
  
  # ---- 2025 (schedule G-24) ----
  4220, "Northern States Power Company - Wisconsin",  2025,        157615948,        51438251, "G-24 WI col",
  3670, "Midwest Natural Gas Incorporated",           2025,         25678302,         1012995, "G-24 WI col",
  5230, "St Croix Valley Natural Gas Company",        2025,         12513484,         2347322, "G-24 WI col",
  6650, "Wisconsin Gas",                              2025,        868312697,      1118404948, "G-24 WI col"
) %>%
  mutate(total_delivered_therms = gas_sold_therms + transport_therms)


# Quick view of reported anchor data
wi_iou_gas_delivered %>%
  arrange(year, utility_id) %>%
  print(n = Inf)

# --- Interpolate non-transport gas sales between PSCW anchors ----------------
# The four utilities have multiple sales anchors across 2005-2025. Interpolate
# utility-wide gas sold (therms) linearly; do not interpolate transport gas or
# combine it with the sales estimate. Keep the observed value, schedule, and
# transport figure only on the original anchor-year rows for auditability.

interp_start <- 2005L
interp_end <- 2025L

if (anyDuplicated(wi_iou_gas_delivered[c("utility_id", "year")]) > 0) {
  stop("PSCW gas anchor table contains duplicate utility-year rows.")
}

anchor_count_check <- wi_iou_gas_delivered %>%
  group_by(utility_id, utility_name) %>%
  summarise(
    n_anchors = sum(!is.na(gas_sold_therms)),
    min_anchor_year = min(year[!is.na(gas_sold_therms)]),
    max_anchor_year = max(year[!is.na(gas_sold_therms)]),
    .groups = "drop"
  )

if (any(anchor_count_check$n_anchors < 2)) {
  stop("Each utility needs at least two gas-sales anchors to interpolate.")
}
if (any(anchor_count_check$min_anchor_year > interp_start) ||
    any(anchor_count_check$max_anchor_year < interp_end)) {
  stop("PSCW anchors do not bracket the requested 2005-2025 interpolation period for every utility.")
}

# --- SCV transport: in-scope annual series ------------------------------------
# SCV (5230) serves only Pierce and St. Croix, so all of its transport gas is
# in-scope. Anchors = the G-24 values in the table above plus the 2015/2016
# onset years (from the PSCW filings; transport begins in 2016). Linear between
# anchors; rule = 2 holds 0 before the 2005 anchor and flat after 2025.

scv_transport_anchors <- wi_iou_gas_delivered %>%
  filter(utility_id == 5230, !is.na(transport_therms)) %>%
  select(utility_id, year, transport_therms) %>%
  bind_rows(tribble(
    ~utility_id, ~year, ~transport_therms,
    5230,        2015,  0,
    5230,        2016,  1149446
  )) %>%
  arrange(year)

if (anyDuplicated(scv_transport_anchors$year) > 0) {
  stop("SCV transport anchors contain duplicate years.")
}

scv_transport_annual <- tibble(
  utility_id = 5230,
  year = interp_start:interp_end
) %>%
  mutate(
    transport_in_scope_therms = approx(
      x = scv_transport_anchors$year,
      y = scv_transport_anchors$transport_therms,
      xout = year,
      method = "linear",
      rule = 2
    )$y
  )

wi_utility_natgas_activity_pcsw_interpolated <- wi_iou_gas_delivered %>%
  select(
    utility_id, utility_name, year, gas_sold_therms,
    transport_therms, source_schedule
  ) %>%
  complete(
    nesting(utility_id, utility_name),
    year = interp_start:interp_end
  ) %>%
  group_by(utility_id, utility_name) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    gas_sold_therms_interpolated = approx(
      x = year[!is.na(gas_sold_therms)],
      y = gas_sold_therms[!is.na(gas_sold_therms)],
      xout = year,
      method = "linear",
      rule = 2
    )$y
  ) %>%
  ungroup() %>%
  left_join(scv_transport_annual, by = c("utility_id", "year")) %>%
  mutate(
    transport_in_scope_therms = coalesce(transport_in_scope_therms, 0),
    state = "WI",
    fuel = "Natural gas",
    unit_activity = "therms",
    is_anchor_year = !is.na(gas_sold_therms),
    data_source = if_else(
      is_anchor_year,
      "PSCW annual report",
      "Interpolated between PSCW annual report anchors"
    )
  ) %>%
  select(
    utility_id, utility_name, state, fuel, year,
    gas_sold_therms, gas_sold_therms_interpolated,
    transport_therms, transport_in_scope_therms,
    source_schedule, is_anchor_year,
    unit_activity, data_source
  ) %>%
  arrange(utility_id, year)

message(sprintf(
  "Interpolated non-transport PSCW gas sales: %d utility-year rows (%d-%d)",
  nrow(wi_utility_natgas_activity_pcsw_interpolated),
  interp_start, interp_end
))

wi_utility_natgas_activity_pcsw_interpolated %>%
  count(utility_name, is_anchor_year) %>%
  pivot_wider(
    names_from = is_anchor_year,
    values_from = n,
    values_fill = 0L
  ) %>%
  print(n = Inf)

write_rds(
  wi_iou_gas_delivered,
  here("_energy", "data", "wi_utility_natgas_activity_pcsw.RDS")
)

write_rds(
  wi_utility_natgas_activity_pcsw_interpolated,
  here("_energy", "data", "wi_utility_natgas_activity_pcsw_interpolated.RDS")
)
