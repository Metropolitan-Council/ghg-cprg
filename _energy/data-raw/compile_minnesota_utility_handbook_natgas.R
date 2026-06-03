# compile_historical_mn_gas.R
# Compiles historical MN natural gas data from MN utility handbooks (2006, 2012)
# and back-estimates county-level deliveries for pre-2014 years using
# county shares derived from earliest available utility reports.
#
# Data sources: The MN Utility data book
#  https://www.lrl.mn.gov/docs/2009/mandated/090598.pdf
#  https://mn.gov/commerce-stat/pdfs/utility-data-book-2012.pdf
#   - Table 12: MN statewide annual natural gas consumption 1989-2012 (1,000 MCF)
#   - Table 13/14: Company-level deliveries for 2006 and 2012
#   - Existing utility report county-level data (2014+)

# source("R/_load_pkgs.R")
# source("_energy/data-raw/_energy_emissions_factors.R")

library(tidyverse)

# =============================================================================
# 1. STATEWIDE ANNUAL TOTALS (Table 12) — units are 1,000 MCF
# =============================================================================
mn_statewide_gas <- tribble(
  ~year, ~residential, ~commercial, ~industrial, ~electric_gen, ~transport_customers, ~company_use, ~unaccounted_for, ~total,
  2005,  128883, 92825,  34629,  7122,  62576,  168,  2752,  328956,
  2006,  119125, 85914,  29526,  5991,  70756,  181,  5268,  316850,  # note: matches Table 13 total / 1000
  2007,  129208, 80311,  41705,  6463,  77259,  195,  4421,  339562,
  2008,  140152, 90825,  41857,  6427,  78671,  207,  3868,  362008,
  2009,  133172, 91555,  33996,  3114,  74807,  202,  2870,  339714,
  2010,  122787, 78404,  36047,  4131, 100829,  162,  1246,  343647,
  2011,  127351, 76026,  35690,  3341, 106917,  202,    72,  349641,
  2012,  109607, 70810,  30825, 23092, 112014,  331,  1015,  347734
) %>%
  # Convert from 1,000 MCF to MCF for consistency with utility report data
  mutate(across(residential:total, ~ .x * 1000))


# =============================================================================
# 2. COMPANY-LEVEL TOTALS for metro-relevant utilities
#    Only need companies serving study area counties
# =============================================================================

# --- 2006 company data (Table 13) ---
company_detail_2006 <- tribble(
  ~utility_handbook,          ~residential, ~commercial, ~industrial, ~electric_gen, ~transport, ~company_use, ~unaccounted_for, ~total,
  "CenterPoint Energy",        61749831,    48284742,    11436167,     4231559,     18028263,      70584,        2927708,    146729154,
  "Xcel Energy",               31316643,    19102235,    11117889,     1351945,      7633570,      16110,        2320251,     72858643,
  "Centennial Utilities",        207543,      129914,           0,           0,            0,       4721,          -5748,       336430,
  "MN Energy Resources-NMU",   3297586,     3159742,      598224,           0,      7972633,          0,              0,     15028185,
  "MN Energy Resources-PNG",  13633698,     9036350,     1250996,           0,     35176179,          0,              0,     59097222,
  "Greater MN Gas",              220877,       21835,       43301,           0,        54938,          0,              0,       340950
) %>%
  mutate(year = 2006)

# --- 2012 company data (Table 14) ---
company_detail_2012 <- tribble(
  ~utility_handbook,          ~residential, ~commercial, ~industrial, ~electric_gen, ~transport, ~company_use, ~unaccounted_for, ~total,
  "CenterPoint Energy",        57717558,    43828879,     6154958,    21621619,     29482627,      92488,        1011795,    159909924,
  "Xcel Energy",               30038392,    18673629,    10715218,      141288,     26719980,      15922,              0,     86304429,
  "Centennial Utilities",        190705,          NA,           0,           0,       323160,       4378,             NA,       645945,
  "MN Energy Resources-NMU",   2767053,      768480,     2947335,           0,      5080744,       9892,              0,     11573504,
  "MN Energy Resources-PNG",  10945391,      527042,     7384961,           0,     47132316,       7706,              0,     65997416,
  "Greater MN Gas",              276384,      145655,           0,           0,        24928,          0,              0,       446967
) %>%
  mutate(year = 2012)

# --- Combined company detail for both handbook years ---
company_detail_all <- bind_rows(company_detail_2006, company_detail_2012)

# --- Simplified totals view (for back-estimation functions) ---
company_totals_2006 <- company_detail_2006 %>% select(utility_handbook, mcf_total = total)
company_totals_2012 <- company_detail_2012 %>% select(utility_handbook, mcf_total = total)


# =============================================================================
# 3. BUILD COUNTY SHARES FROM EARLIEST UTILITY REPORTS
#    Uses your existing processed utility report data
# =============================================================================

# TODO: Update path to match your project structure
# processed_mn_gasUtil_activityData <- read_rds(here("_energy", "data", "minnesota_gasUtils_ActivityAndEmissions.RDS"))

# --- Mapping from utility folder names to handbook company names ---
# UPDATE these to match the actual folder names in your mn_ng_utility_reporting_state directory
utility_name_crosswalk <- tribble(
  ~utility,              ~utility_handbook,
  # "centerpoint_energy",  "CenterPoint Energy",
  # "xcel_energy",         "Xcel Energy",
  # "centennial_utils",    "Centennial Utilities",
  # ---- fill in actual folder names ----
)

# Calculate county share of each utility's total from earliest utility report year
# This is the key assumption: within-utility county distribution is stable over time
calc_county_shares <- function(utility_data, anchor_year = 2014) {
  utility_data %>%
    filter(year == anchor_year) %>%
    group_by(utility) %>%
    mutate(
      utility_total = sum(mcf_delivered, na.rm = TRUE),
      county_share  = mcf_delivered / utility_total
    ) %>%
    ungroup() %>%
    select(utility, county, county_share)
}

# county_shares_2014 <- calc_county_shares(processed_mn_gasUtil_activityData, anchor_year = 2014)


# =============================================================================
# 4. BACK-ESTIMATE COUNTY-LEVEL DELIVERIES FOR HANDBOOK YEARS (2006, 2012)
# =============================================================================

back_estimate_counties <- function(company_totals, county_shares, crosswalk, year_label) {
  county_shares %>%
    left_join(crosswalk, by = "utility") %>%
    left_join(company_totals, by = "utility_handbook") %>%
    mutate(
      mcf_estimated = mcf_total * county_share,
      year = year_label,
      county_source = paste0("Handbook ", year_label, " + utility shares")
    ) %>%
    group_by(county, year, county_source) %>%
    summarise(
      total_mcf = sum(mcf_estimated, na.rm = TRUE),
      .groups = "drop"
    )
}

# county_est_2006 <- back_estimate_counties(company_totals_2006, county_shares_2014, utility_name_crosswalk, 2006)
# county_est_2012 <- back_estimate_counties(company_totals_2012, county_shares_2014, utility_name_crosswalk, 2012)


# =============================================================================
# 5. INTERPOLATE REMAINING GAPS, SCALED BY STATEWIDE ANNUAL TOTALS
#
#    For years between anchor points (e.g., 2007-2011, 2013), interpolate
#    county-level MCF and then scale by the ratio of that year's statewide
#    total to the interpolation baseline, so year-to-year weather variation
#    (and other macro shifts) is reflected.
# =============================================================================

interpolate_with_scaling <- function(county_anchors, statewide_totals) {
  # county_anchors: df with columns county, year, total_mcf (for anchor years only)
  # statewide_totals: df with columns year, total
  
  counties <- unique(county_anchors$county)
  all_years <- min(county_anchors$year):max(county_anchors$year)
  
  map_dfr(counties, function(cty) {
    anchors <- county_anchors %>% filter(county == cty) %>% arrange(year)
    
    # Linear interpolation of county MCF across all years
    interp <- approx(
      x = anchors$year,
      y = anchors$total_mcf,
      xout = all_years,
      rule = 2  # clamp at boundaries
    )
    
    tibble(county = cty, year = interp$x, total_mcf_interp = interp$y)
  }) %>%
    # Join statewide totals
    left_join(statewide_totals %>% select(year, state_total = total), by = "year") %>%
    # Build a reference statewide curve from the same interpolation approach
    # (interpolating between anchor-year state totals)
    group_by(county) %>%
    mutate(
      # For each county, get the statewide total at its anchor years
      # and interpolate to build expected state total
      # Then scale = actual_state / expected_state
    ) %>%
    ungroup()
  
  # NOTE: Simpler approach — just use the statewide total ratio directly:
  # For each year, scaling_factor = statewide_total[year] / statewide_total[nearest_anchor_year]
  # Then: total_mcf_scaled = total_mcf_interp * scaling_factor
  # This adds weather-driven variation on top of the structural interpolation
}


# =============================================================================
# 6. COMBINE ALL YEARS INTO FINAL COUNTY-LEVEL TIMESERIES
# =============================================================================

# Anchor years with actual/estimated county data:
#   2006 — from handbook company totals × 2014 county shares
#   2012 — from handbook company totals × 2014 county shares
#   2014+ — from actual utility reports (existing data)
#
# Interpolated years:
#   2005 — scale 2006 county estimates by statewide 2005/2006 ratio
#   2007-2011 — interpolate between 2006 and 2012, scaled by statewide totals
#   2013 — interpolate between 2012 and 2014, scaled by statewide totals

# combine_all <- bind_rows(
#   county_est_2006,
#   county_est_2012,
#   # 2014+ actuals from existing pipeline:
#   MNcounty_level_gas_emissions %>% filter(year >= 2014) %>% select(county = county_name, year, total_mcf)
# )

# For 2005: simple ratio scaling from 2006
# county_est_2005 <- county_est_2006 %>%
#   mutate(
#     year = 2005,
#     total_mcf = total_mcf * (mn_statewide_gas %>% filter(year == 2005) %>% pull(total)) /
#                              (mn_statewide_gas %>% filter(year == 2006) %>% pull(total)),
#     county_source = "Scaled from 2006 handbook estimate"
#   )


# =============================================================================
# 7. QA CHECKS
# =============================================================================

# Verify handbook totals match Table 12
# company_totals_2006 %>% summarise(sum(mcf_total))  # should ≈ 316,804,334 (full table)
# company_totals_2012 %>% summarise(sum(mcf_total))  # should ≈ 347,734,296 (full table)
# Note: we only include metro-serving utilities, so these will be < statewide total.
# The county shares approach handles this because we're distributing each company's
# total to its own service-area counties.

# Compare 2012 back-estimate vs 2014 actuals for reasonableness
# county_est_2012 %>%
#   left_join(
#     MNcounty_level_gas_emissions %>% filter(year == 2014) %>%
#       select(county = county_name, mcf_2014 = total_mcf),
#     by = "county"
#   ) %>%
#   mutate(pct_diff = (total_mcf - mcf_2014) / mcf_2014 * 100)

# Compare old pop-downscaled 2005 Anoka estimate vs new approach
# Old: ~22,978,654 MCF (pop-proportional)
# New should be meaningfully lower if Anoka's share of CenterPoint/Xcel
# deliveries is smaller than its share of state population

cat("
=== NEXT STEPS ===
1. Fill in utility_name_crosswalk with actual folder names from your utility reports
2. Uncomment the processing sections and run
3. Verify 2006 and 2012 company totals against the handbook tables
4. Check whether MERC-PNG or MERC-NMU serve any study area counties
   (if so, add them to company_totals and crosswalk)
5. Compare the new Anoka 2005 estimate against the old pop-downscaled value
6. Decide whether to anchor interpolation to 2006+2012+2014 (three points)
   or just 2006+2014 (two points, ignoring 2012 as redundant)
")
