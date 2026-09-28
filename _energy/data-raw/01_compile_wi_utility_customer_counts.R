# Wisconsin IOU utility customer counts by county
# Source: PSCW Annual Reports, Schedules E-40 (Electric) and G-26 (Gas)
# PDF URL pattern: https://apps.psc.wi.gov/PDFfiles/Annual%20Reports/IOU/IOU_{year}_{utility_id}.pdf
#
# This script stores customer counts extracted from PSCW IOU annual report PDFs
# for the four investor-owned utilities serving Pierce and St. Croix counties.
# Customer count shares are used to apportion utility-wide sales to counties.
#
# Utilities in scope (IOU only — co-ops and municipals handled separately):
#   Electric: 4220 (Northern States Power Co - Wisconsin)
#   Gas:      4220 (Northern States Power Co - Wisconsin)
#             3670 (Midwest Natural Gas Incorporated)
#             5230 (St Croix Valley Natural Gas Company)
#             6650 (Wisconsin Gas)

source("R/_load_pkgs.R")

# --- Customer count data extracted from PSCW annual report PDFs ---
# Each row: one utility × county × year × fuel type
# Sources cited by schedule: E-40 = Electric Customers Served, G-26 = Gas Customers Served
# E-03 = Sales of Electricity By Rate Schedule (for utility-wide electric total)

wi_iou_customer_counts <- tribble(
  ~utility_id, ~utility_name,                              ~fuel,      ~year, ~county,     ~county_customers, ~utility_total_customers, ~schedule_source,
  # ---- 2022 ----
  # NSP-WI Electric (E-40 county totals & grand total "Total - Customers Served")
  4220, "Northern States Power Company - Wisconsin",  "electric",  2022, "Pierce",         7527,   267713, "E-40",
  4220, "Northern States Power Company - Wisconsin",  "electric",  2022, "St. Croix",     25080,   267713, "E-40",
  # NSP-WI Gas (G-26 lines 85, 114, 120)
  4220, "Northern States Power Company - Wisconsin",  "gas",       2022, "Pierce",          105,   114273, "G-26",
  4220, "Northern States Power Company - Wisconsin",  "gas",       2022, "St. Croix",     16272,   114273, "G-26",
  # Midwest Natural Gas (G-26 — no Pierce service; lines 35, 67)
  3670, "Midwest Natural Gas Incorporated",           "gas",       2022, "St. Croix",      5710,    19152, "G-26",
  # St Croix Valley Natural Gas (G-26 lines 6, 12, 13)
  5230, "St Croix Valley Natural Gas Company",        "gas",       2022, "Pierce",         5548,     9505, "G-26",
  5230, "St Croix Valley Natural Gas Company",        "gas",       2022, "St. Croix",      3957,     9505, "G-26",
  # Wisconsin Gas (G-26 lines 412, 486, 654)
  6650, "Wisconsin Gas",                              "gas",       2022, "Pierce",         3342,   650532, "G-26",
  6650, "Wisconsin Gas",                              "gas",       2022, "St. Croix",      4334,   650532, "G-26",
  
  
  # ---- 2021 electric (NSP-WI — E-40 county totals & grand total "Total - Customers Served") ----
  4220, "Northern States Power Company - Wisconsin",  "electric",  2021, "Pierce",         7489,   266071, "E-40",
  4220, "Northern States Power Company - Wisconsin",  "electric",  2021, "St. Croix",     24850,   266071, "E-40",
  
  # ---- 2021 (gas — verified against IOU_2021_4220.pdf; other utilities from wisconsin_natGas_estimate_2005_and_2021.R, unverified) ----
  4220, "Northern States Power Company - Wisconsin",  "gas",       2021, "Pierce",          107,   113012, "G-26",
  4220, "Northern States Power Company - Wisconsin",  "gas",       2021, "St. Croix",     15990,   113012, "G-26",
  3670, "Midwest Natural Gas Incorporated",           "gas",       2021, "St. Croix",      5573,    18793, "G-26",
  5230, "St Croix Valley Natural Gas Company",        "gas",       2021, "Pierce",         5410,     9227, "G-26",
  5230, "St Croix Valley Natural Gas Company",        "gas",       2021, "St. Croix",      3817,     9227, "G-26",
  6650, "Wisconsin Gas",                              "gas",       2021, "Pierce",         3320,   645576, "G-26",
  6650, "Wisconsin Gas",                              "gas",       2021, "St. Croix",      4252,   645576, "G-26",
  
  # ---- 2005 (gas — verified against IOU_2005_4220.pdf; other utilities from wisconsin_natGas_estimate_2005_and_2021.R, unverified) ----
  4220, "Northern States Power Company - Wisconsin",  "gas",       2005, "Pierce",            0,    93588, "G-26",
  4220, "Northern States Power Company - Wisconsin",  "gas",       2005, "St. Croix",     11878,    93588, "G-26",
  3670, "Midwest Natural Gas Incorporated",           "gas",       2005, "St. Croix",      3516,    13845, "G-26",
  5230, "St Croix Valley Natural Gas Company",        "gas",       2005, "Pierce",         4573,     6939, "G-26",
  5230, "St Croix Valley Natural Gas Company",        "gas",       2005, "St. Croix",      2366,     6939, "G-26",
  6650, "Wisconsin Gas",                              "gas",       2005, "Pierce",         3039,   583336, "G-26",
  6650, "Wisconsin Gas",                              "gas",       2005, "St. Croix",      3453,   583336, "G-26",
  
  # ---- 2005 electric (NSP-WI — E-40 county totals & grand total "Total Electric Customers") ----
  4220, "Northern States Power Company - Wisconsin",  "electric",  2005, "Pierce",         6740,   232690, "E-40",
  4220, "Northern States Power Company - Wisconsin",  "electric",  2005, "St. Croix",     20357,   232690, "E-40",
  
  # ---- 2013 electric (NSP-WI — E-40 county totals & grand total "Total Company:") ----
  4220, "Northern States Power Company - Wisconsin",  "electric",  2013, "Pierce",         6981,   244628, "E-40",
  4220, "Northern States Power Company - Wisconsin",  "electric",  2013, "St. Croix",     22174,   244628, "E-40",
  
  # ---- 2013 gas ----
  # NSP-WI (G-26 p2: Pierce=82, St Croix=13,652; p3: Total Company=103,045)
  4220, "Northern States Power Company - Wisconsin",  "gas",       2013, "Pierce",           82,   103045, "G-26",
  4220, "Northern States Power Company - Wisconsin",  "gas",       2013, "St. Croix",     13652,   103045, "G-26",
  # Midwest Natural Gas (G-26 p1: St Croix=4,132; p2: Total Company=15,707)
  3670, "Midwest Natural Gas Incorporated",           "gas",       2013, "St. Croix",      4132,    15707, "G-26",
  # St Croix Valley Natural Gas (G-26 p1: Pierce=4,909; St Croix=3,060; Total=7,969)
  5230, "St Croix Valley Natural Gas Company",        "gas",       2013, "Pierce",         4909,     7969, "G-26",
  5230, "St Croix Valley Natural Gas Company",        "gas",       2013, "St. Croix",      3060,     7969, "G-26",
  # Wisconsin Gas (G-26 p7: Pierce=3,143; p8-9: St Croix=3,810; p11: Total=608,529)
  6650, "Wisconsin Gas",                              "gas",       2013, "Pierce",         3143,   608529, "G-26",
  6650, "Wisconsin Gas",                              "gas",       2013, "St. Croix",      3810,   608529, "G-26",
  
  # ---- 2025 electric (NSP-WI — E-40 county totals & grand total "Total - Customers Served") ----
  4220, "Northern States Power Company - Wisconsin",  "electric",  2025, "Pierce",         7671,   274765, "E-40",
  4220, "Northern States Power Company - Wisconsin",  "electric",  2025, "St. Croix",     25829,   274765, "E-40",
  
  # ---- 2025 gas ----
  # NSP-WI (G-26 p3: Pierce=108, StCroix=16,866; p4: Total=117,478)
  4220, "Northern States Power Company - Wisconsin",  "gas",       2025, "Pierce",          108,   117478, "G-26",
  4220, "Northern States Power Company - Wisconsin",  "gas",       2025, "St. Croix",     16866,   117478, "G-26",
  # Midwest Natural Gas (G-26 p1: StCroix=6,136; p2: Total=19,999)
  3670, "Midwest Natural Gas Incorporated",           "gas",       2025, "St. Croix",      6136,    19999, "G-26",
  # St Croix Valley Natural Gas (G-26 p1: Pierce=5,721; StCroix=4,498; Total=10,219)
  5230, "St Croix Valley Natural Gas Company",        "gas",       2025, "Pierce",         5721,    10219, "G-26",
  5230, "St Croix Valley Natural Gas Company",        "gas",       2025, "St. Croix",      4498,    10219, "G-26",
  # Wisconsin Gas (G-26 p10: Pierce=3,405; p12: StCroix=4,466; p17: Grand Total=670,447)
  6650, "Wisconsin Gas",                              "gas",       2025, "Pierce",         3405,   670447, "G-26",
  6650, "Wisconsin Gas",                              "gas",       2025, "St. Croix",      4466,   670447, "G-26"
)


# --- Calculate county share of each utility's total customers ---

wi_iou_county_shares <- wi_iou_customer_counts %>%
  mutate(
    county_share = county_customers / utility_total_customers
  ) %>%
  arrange(year, fuel, utility_id, county)


# Quick summary view
wi_iou_county_shares %>%
  select(year, fuel, utility_name, county, county_customers, utility_total_customers, county_share) %>%
  print(n = Inf)

# ──────────────────────────────────────────────────────────────────────────────
# Interpolate customer counts and county shares across all years 2005–2025
# ──────────────────────────────────────────────────────────────────────────────
# Anchor years (2005, 2013, 2021, 2022, 2025) are hand-extracted from PSCW IOU
# annual report PDFs (schedules E-40, G-26, E-03). Between/around anchor years,
# linearly interpolate the county SHARE (not raw counts) per utility x fuel x
# county, since utility_total_customers is independently available annually
# from EIA-861 and shares change gradually and smoothly. This mirrors the
# approx()-based interpolation convention used elsewhere in the energy
# workflow (see 02_compile_minnesota_utility_handbook_natgas.R).

interp_start <- 2005L
interp_end <- 2025L

wi_iou_county_shares_interpolated <- wi_iou_county_shares %>%
  select(utility_id, utility_name, fuel, county, year, county_share) %>%
  complete(
    nesting(utility_id, utility_name, fuel, county),
    year = interp_start:interp_end
  ) %>%
  group_by(utility_id, utility_name, fuel, county) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    # rule = 2 holds the nearest anchor value flat outside the anchor range
    # (e.g., 2023-2025 use the 2022 or 2025 anchor; 2006-2012 interpolate
    # between the 2005 and 2013 anchors)
    county_share_interpolated = approx(
      x = year[!is.na(county_share)],
      y = county_share[!is.na(county_share)],
      xout = year,
      rule = 2
    )$y
  ) %>%
  ungroup() %>%
  mutate(
    is_anchor_year = !is.na(county_share),
    data_source = if_else(is_anchor_year, "pscw_annual_report", "interpolated_pscw")
  ) %>%
  select(
    utility_id, utility_name, fuel, county, year,
    county_share_interpolated, is_anchor_year, data_source
  ) %>%
  arrange(utility_name, fuel, county, year)

message(sprintf(
  "Interpolated WI IOU customer shares: %d utility x fuel x county x year rows (%d-%d)",
  nrow(wi_iou_county_shares_interpolated), interp_start, interp_end
))

wi_iou_county_shares_interpolated %>%
  count(utility_name, fuel, county, is_anchor_year) %>%
  pivot_wider(names_from = is_anchor_year, values_from = n, values_fill = 0L) %>%
  print(n = Inf)

# --- write outputs -----------------------------------------------------------

write_rds(
  wi_iou_customer_counts,
  here("_energy", "data", "WI_iou_customer_counts_anchor_years.RDS")
)

write_rds(
  wi_iou_county_shares_interpolated,
  here("_energy", "data", "WI_utility_customer_shares_interpolated.RDS")
)


