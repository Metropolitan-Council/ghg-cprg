# 03_compile_wi_electricity_activity.R
# ──────────────────────────────────────────────────────────────────────────────
# Combine EIA-861 utility-wide MWh totals (02_compile_eia_wi_elec_activity.R)
# with county allocation weights to build a complete WI county x year
# electricity activity + emissions table, 2005-latest, for the 7 in-scope
# electric utilities serving Pierce and St. Croix counties.
#
# County allocation method by utility (methodology carried forward from
# wisconsin_elec_estimate_2021.R, now made temporal instead of single-year):
#
#   Northern States Power Co-Wisconsin (NSP-WI)
#     -> interpolated PSCW E-40 customer share (01_compile_wi_utility_customer_counts.R)
#        real anchor years: 2005, 2013, 2021, 2022, 2025; linearly interpolated
#        / held flat (rule = 2) between and outside anchor years
#   New Richmond Municipal Electric Utility
#     -> 100% St. Croix (utility's territory is entirely contained in one county)
#   River Falls Municipal Utility
#     -> single measured 2021 PSCW customer-count split (4,901 Pierce / 2,137
#        St. Croix), held constant across all years. No other anchor year is
#        available for this utility, so this is the best available estimate
#        (still preferred over population share, since it reflects measured
#        customers rather than modeled population).
#   Dunn Energy Cooperative, Polk-Burnett Electric Cooperative,
#   St Croix Electric Cooperative, Pierce-Pepin Electric Cooperative Services
#     -> static Census-block population share of the utility's service area
#        (electric co-ops never report county-level customer counts to PSCW
#        or EIA; population share is the only available split and is treated
#        as constant across all years, consistent with the prior 2021-only
#        estimate)
#
# NOTE: the River Falls and co-op allocation shares are NOT independently
# verified across time — flag for review if St. Croix/Pierce grant work turns
# up updated customer or population splits for these 5 utilities.
#
# Inputs:
#   WI_utility_elec_activity_eia861.RDS            (02 script)
#   WI_utility_customer_shares_interpolated.RDS    (01 script)
#   wi_utility_reporting/WIutilities_in_scope_CensusBlockSums.shp
#   wi_utility_reporting/fullElecUtilityServiceAreas_CensusBlockSums.shp
#   _meta/data/epa_ghg_factor_hub.RDS  (egridTimeSeries)
#
# Outputs:
#   WI_county_elec_activity_detail.RDS
#     emissions_year, county_name, utility_name, utility_type, mwh,
#     allocation_method, data_source
#   WI_county_elec_activity.RDS
#     emissions_year, county_name, mwh, data_source
#     (schema matches county_elec_activity.RDS so this can be joined
#      alongside the MN table directly in compile_temporal_county_nrel_proportion.R)
#   WI_county_elec_emissions.RDS
#     emissions_year, county_name, state, sector, emissions_metric_tons_co2e
# ──────────────────────────────────────────────────────────────────────────────

source("R/_load_pkgs.R")
source("R/global_warming_potential.R")

# --- eGRID temporal emissions factor (metric tons CO2e per MWh, by year) -----
# Matches the convention used in 05_compile_temporal_county_mc_elec_proportion.R
# and compile_temporal_county_nrel_proportion.R

egrid_temporal <- read_rds(here("_meta", "data", "epa_ghg_factor_hub.RDS")) %>%
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
  summarise(mt_co2e_mwh = sum(mt_co2e_mwh), .groups = "drop") %>%
  rename(emissions_year = Year) %>%
  select(emissions_year, mt_co2e_mwh)

# --- inputs -------------------------------------------------------------------

wi_eia861 <- read_rds(here("_energy", "data", "WI_utility_elec_activity_eia861.RDS"))

# NSP-WI interpolated customer share (01 script). The PDF-extracted tribble
# uses "Northern States Power Company - Wisconsin" (with spaces around the
# hyphen); the electric service-area shapefile / EIA harmonization lookup
# (02 script) use "Northern States Power Company-Wisconsin" (no spaces).
# Harmonize here so the join below succeeds.
wi_customer_shares <- read_rds(here(
  "_energy", "data", "WI_utility_customer_shares_interpolated.RDS"
)) %>%
  filter(fuel == "electric") %>%
  mutate(
    utility_name = if_else(
      utility_name == "Northern States Power Company - Wisconsin",
      "Northern States Power Company-Wisconsin",
      utility_name
    )
  ) %>%
  select(
    utility_name, county_name = county, emissions_year = year,
    county_share = county_share_interpolated
  )

# --- static Census-block population weights ------------------------------------
# ArcGIS Pro-derived (centroid-in-polygon population sums); see
# wisconsin_elec_estimate_2021.R for original processing notes. Single Census
# vintage applied as a constant proportion across all years.

wi_util_county_pop <- st_read(here(
  "_energy", "data-raw", "wi_utility_reporting",
  "WIutilities_in_scope_CensusBlockSums.shp"
), quiet = TRUE) %>%
  st_drop_geometry() %>%
  rename(
    utility_name = utlty_n,
    countyUtilityPop = sum_value,
    county_name = cnty_nm
  ) %>%
  select(utility_name, county_name, countyUtilityPop)

wi_util_total_pop <- st_read(here(
  "_energy", "data-raw", "wi_utility_reporting",
  "fullElecUtilityServiceAreas_CensusBlockSums.shp"
), quiet = TRUE) %>%
  st_drop_geometry() %>%
  rename(
    utility_name = utlty_nm_x,
    totalUtilityPop = sum_value
  ) %>%
  select(utility_name, totalUtilityPop)

wi_pop_shares <- wi_util_county_pop %>%
  left_join(wi_util_total_pop, by = "utility_name") %>%
  mutate(county_share = countyUtilityPop / totalUtilityPop) %>%
  select(utility_name, county_name, county_share)

# --- one-time measured River Falls Muni customer split (2021 PSCW report) ---
# See wisconsin_elec_estimate_2021.R for the original 4,901 / 2,137 figures.

river_falls_share <- tribble(
  ~utility_name, ~county_name, ~county_share,
  "River Falls Municipal Utility", "Pierce", 4901 / (4901 + 2137),
  "River Falls Municipal Utility", "St. Croix", 2137 / (4901 + 2137)
)

# --- New Richmond Muni: fully contained within St. Croix ---------------------

new_richmond_share <- tribble(
  ~utility_name, ~county_name, ~county_share,
  "New Richmond Municipal Electric Utility", "St. Croix", 1
)

# --- build utility x county x year allocation weights ------------------------

coop_utilities <- c(
  "Dunn Energy Cooperative",
  "Polk-Burnett Electric Cooperative",
  "St Croix Electric Cooperative",
  "Pierce-Pepin Electric Cooperative Services"
)

wi_utility_years <- wi_eia861 %>%
  distinct(utility_name, emissions_year)

allocation_weights <- bind_rows(
  # NSP-WI: interpolated PSCW customer share, varies by year
  wi_customer_shares %>%
    filter(utility_name == "Northern States Power Company-Wisconsin") %>%
    mutate(allocation_method = "pscw_customer_share_interpolated"),

  # River Falls Muni: constant measured 2021 share, applied to every EIA year
  wi_utility_years %>%
    filter(utility_name == "River Falls Municipal Utility") %>%
    left_join(river_falls_share, by = "utility_name") %>%
    mutate(allocation_method = "pscw_customer_share_static_2021"),

  # New Richmond Muni: 100% St. Croix, every year
  wi_utility_years %>%
    filter(utility_name == "New Richmond Municipal Electric Utility") %>%
    left_join(new_richmond_share, by = "utility_name") %>%
    mutate(allocation_method = "fully_contained"),

  # Co-ops: static Census block population share, applied to every EIA year
  wi_utility_years %>%
    filter(utility_name %in% coop_utilities) %>%
    left_join(wi_pop_shares, by = "utility_name") %>%
    mutate(allocation_method = "census_block_population_share")
) %>%
  select(utility_name, county_name, emissions_year, county_share, allocation_method)

# --- QA: flag any utility-year with no allocation weight (name mismatch etc)--

unallocated <- wi_utility_years %>%
  anti_join(allocation_weights, by = c("utility_name", "emissions_year"))

if (nrow(unallocated) > 0) {
  warning("WI utility-years with no county allocation weight found:")
  print(unallocated)
}

# --- QA: check per-utility-year county shares don't exceed 1.0 ---------------

share_check <- allocation_weights %>%
  group_by(utility_name, emissions_year) %>%
  summarise(share_sum = sum(county_share, na.rm = TRUE), .groups = "drop")

over_one <- share_check %>% filter(share_sum > 1.001)
if (nrow(over_one) > 0) {
  warning("Some WI utility-years have county shares summing above 1.0:")
  print(over_one)
}

# --- allocate EIA-861 total MWh to counties -----------------------------------

wi_county_elec_activity_detail <- wi_eia861 %>%
  select(utility_name, utility_type, emissions_year, total) %>%
  inner_join(allocation_weights, by = c("utility_name", "emissions_year")) %>%
  filter(!is.na(county_share)) %>%
  mutate(
    mwh = total * county_share,
    data_source = "eia_861_x_pscw_customer_share"
  ) %>%
  select(
    emissions_year, county_name, utility_name, utility_type,
    mwh, allocation_method, data_source
  ) %>%
  arrange(county_name, utility_name, emissions_year)

message(sprintf(
  "WI county electricity activity (detail): %d utility x county x year rows, %d-%d",
  nrow(wi_county_elec_activity_detail),
  min(wi_county_elec_activity_detail$emissions_year),
  max(wi_county_elec_activity_detail$emissions_year)
))

coverage <- wi_county_elec_activity_detail %>%
  count(utility_name, allocation_method)
message("\nCoverage by utility x allocation method:")
print(coverage)

# --- county-level summary (schema matches county_elec_activity.RDS for MN) ---

wi_county_elec_activity <- wi_county_elec_activity_detail %>%
  group_by(emissions_year, county_name) %>%
  summarise(mwh = sum(mwh, na.rm = TRUE), .groups = "drop") %>%
  mutate(data_source = "WI utility report (EIA-861 x PSCW customer share)") %>%
  arrange(county_name, emissions_year)

# --- emissions (temporal eGRID MROW factor by year) ---------------------------

wi_county_elec_emissions <- wi_county_elec_activity %>%
  left_join(egrid_temporal, by = "emissions_year") %>%
  mutate(value_emissions = mwh * mt_co2e_mwh) %>%
  group_by(emissions_year, county_name) %>%
  summarise(
    emissions_metric_tons_co2e = sum(value_emissions, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(state = "WI", sector = "Electricity") %>%
  arrange(county_name, emissions_year)

# --- write outputs -------------------------------------------------------------

write_rds(
  wi_county_elec_activity_detail,
  here("_energy", "data", "wi_county_elec_activity_detail.RDS")
)
write_rds(
  wi_county_elec_activity,
  here("_energy", "data", "wi_county_elec_activity.RDS")
)
write_rds(
  wi_county_elec_emissions,
  here("_energy", "data", "wi_county_elec_emissions.RDS")
)

message(sprintf(
  "\nWrote WI_county_elec_activity_detail.RDS, WI_county_elec_activity.RDS, WI_county_elec_emissions.RDS (%d-%d)",
  min(wi_county_elec_activity$emissions_year),
  max(wi_county_elec_activity$emissions_year)
))
