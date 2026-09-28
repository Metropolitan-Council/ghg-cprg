# Wisconsin IOU gas delivered by utility × year
# Source: PSCW Annual Reports, Schedule G-24 "Summary of Gas Account & System Load Statistics"
# (Schedule G-23 in the 2005 report format)
# PDF URL pattern: https://apps.psc.wi.gov/PDFfiles/Annual%20Reports/IOU/IOU_{year}_{utility_id}.pdf
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
#
# Data notes:
#   - Wisconsin column is used throughout (excludes any Michigan operations).
#   - 2005 uses schedule G-23; 2013+ uses G-24. Both have identical column structure.
#   - 2005 and 2021 gas-only utilities (3670, 5230, 6650) are pulled from the previous
#     R script (wisconsin_natGas_estimate_2005_and_2021.R) where PDFs were not re-verified.
#     The previous script did not track transport gas separately, so those rows show NA
#     for transport_therms — assume ~0 for 3670 and 5230 based on 2013/2022 pattern; 6650
#     transport is substantial (near equal to gas sold in later years) and should be
#     verified from PDF before use.

source("R/_load_pkgs.R")

wi_iou_gas_delivered <- tribble(
  ~utility_id, ~utility_name,                              ~year, ~gas_sold_therms, ~transport_therms, ~source_schedule,
  # ---- 2005 (schedule G-23) ----
  4220, "Northern States Power Company - Wisconsin",  2005,        138281245,        51531175, "G-23 WI col",
  3670, "Midwest Natural Gas Incorporated",           2005,         18816267,              NA, "prior R script",
  5230, "St Croix Valley Natural Gas Company",        2005,          9534249,              NA, "prior R script",
  6650, "Wisconsin Gas",                              2005,        728522194,              NA, "prior R script",
  
  # ---- 2013 (schedule G-24) ----
  4220, "Northern States Power Company - Wisconsin",  2013,        171384590,        46661850, "G-24 WI col",
  3670, "Midwest Natural Gas Incorporated",           2013,         22624435,               0, "G-24 WI col",
  5230, "St Croix Valley Natural Gas Company",        2013,         12622380,               0, "G-24 WI col",
  6650, "Wisconsin Gas",                              2013,        792947359,       725182417, "G-24 WI col",
  
  # ---- 2021 (schedule G-24) ----
  4220, "Northern States Power Company - Wisconsin",  2021,        171102649,        51506030, "G-24 WI col",
  3670, "Midwest Natural Gas Incorporated",           2021,         23181392,              NA, "prior R script",
  5230, "St Croix Valley Natural Gas Company",        2021,         11155826,              NA, "prior R script",
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


# Quick view
wi_iou_gas_delivered %>%
  arrange(year, utility_id) %>%
  print(n = Inf)

write_rds(
  wi_iou_gas_delivered,
  here("_energy", "data", "wi_utility_elec_activity_pcsw.RDS")
)
