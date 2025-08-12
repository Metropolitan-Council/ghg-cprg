# cdp totals
# from the

source("R/_load_pkgs.R")

mndot_vmt_county <- readRDS("_transportation/data-raw/mndot/mndot_vmt_county.RDS") %>%
  filter(!county %in% c(
    "Chisago",
    "Sherburne",
    "Wright"
  ))

mndot_vmt_county %>%
  filter(year == max(year)) %>%
  summarize(
    year = unique(year),
    daily_vmt = sum(daily_vmt),
    annual_vmt = sum(annual_vmt),
    centerline_miles = sum(centerline_miles)
  ) %>%
  kable()
