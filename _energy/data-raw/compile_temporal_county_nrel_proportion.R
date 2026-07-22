# use nrel proportions to breakout county energy deliveries
# backcast where possible but be clear about interpolation/data weaknesses
# includes power plant natural gas subtraction to avoid double-counting
# with electricity generation analysis (eGRID-based)

source("R/_load_pkgs.R")
source("R/global_warming_potential.R")


nrel_emissions <- readRDS("_energy/data-raw/nrel_slope/nrel_emissions_inv_county.RDS")
egrid_temporal <- readRDS("_meta/data/epa_ghg_factor_hub.RDS") %>%
  pluck("egridTimeSeries") %>%
  mutate( # metric ton co2e per mwh
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
  # get rid of unnecessary columns from eGRID factor tables
  group_by(Year, Source) %>%
  summarize(mt_co2e_mwh = sum(mt_co2e_mwh), .groups = "keep") %>%
  ungroup()

natgas_ef_scf <- readRDS("_meta/data/epa_ghg_factor_hub.RDS") %>%
  pluck("stationary_combustion") %>%
  filter(fuel_category == "Natural Gas" & per_unit == "scf") %>%
  mutate( # metric ton co2e per mwh
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
  # get rid of unnecessary columns from eGRID factor tables
  group_by(fuel_category, Source) %>%
  summarize(mt_co2e_mcf = sum(mt_co2e_mcf), .groups = "keep") %>%
  ungroup()


# ════════════════════════════════════════════════════════════════════
# ELECTRICITY: COUNTY ACTIVITY
# MN counties: complete 2005–2023 from new 7610/EIA 861 pipeline
# WI counties: per-capita estimate anchored to 2021 utility reports
# ════════════════════════════════════════════════════════════════════

# ── MN counties from new pipeline (already gap-filled and backcasted) ────────
electric_mn <- readRDS(here::here("_energy", "data", "county_elec_activity.RDS")) %>%
  transmute(
    emissions_year,
    county_name,
    mwh = value_activity,
    data_source = "Utility report (7610/EIA 861)"
  )

# ── WI counties via per-capita estimate from 2021 ───────────────────────────
county_pop <- readRDS(here::here("_meta", "data", "census_county_population.RDS"))

wi_electric <- readRDS(here::here("_energy", "data", "wisconsin_elecUtils_ActivityAndEmissions.RDS")) %>%
  group_by(county_name) %>%
  summarize(mwh = sum(coalesced_utilityCounty_mWh), .groups = "drop") %>%
  left_join(
    county_pop %>% filter(population_year == 2021),
    by = "county_name"
  ) %>%
  mutate(mwh_per_capita = mwh / population) %>%
  select(county_name, mwh_per_capita) %>%
  left_join(
    county_pop %>% filter(population_year >= 2005),
    by = "county_name"
  ) %>%
  transmute(
    emissions_year = as.numeric(population_year),
    county_name,
    mwh = mwh_per_capita * population,
    data_source = if_else(
      emissions_year == 2021,
      "Utility report",
      "Population based estimate"
    )
  )

# ── Combine and calculate emissions ─────────────────────────────────────────
electric_interpolated <- bind_rows(electric_mn, wi_electric) %>%
  mutate(sector = "Electricity") %>%
  left_join(egrid_temporal, by = c("emissions_year" = "Year")) %>%
  mutate(
    value_emissions = mt_co2e_mwh * mwh,
    unit_emissions = "Metric tons CO2e",
    activity_type = "mWh delivered"
  ) %>%
  select(
    emissions_year,
    county_name,
    sector,
    activity = mwh,
    activity_type,
    data_source,
    factor_source = Source,
    value_emissions,
    unit_emissions
  )

ggplot(electric_interpolated, aes(x = emissions_year, y = activity, col = county_name)) +
  geom_line() +
  labs(title = "County electricity deliveries (MWh)", x = NULL, y = "MWh")




natgas_raw <- readRDS(file.path(here::here("_energy", "data", "county_natgas_activity.RDS"))) %>%
  as_tibble() %>%
  bind_rows(
    readRDS(file.path(here::here(), "_energy/data/wisconsin_county_GasEmissions.RDS")) %>%
      rename(emissions_year = year)
  ) %>%
  mutate(
    mcf_delivered = if_else(is.na(mcf_delivered),
      emissions_metric_tons_co2e / natgas_ef_scf$mt_co2e_mcf,
      mcf_delivered
    ),
    sector = "Natural gas"
  ) # removing because MERC filing in not in Commerce edocket


# ════════════════════════════════════════════════════════════════════
# NATURAL GAS: COUNTY ACTIVITY
# (unchanged from previous version)
# ════════════════════════════════════════════════════════════════════

natgas_raw <- readRDS(file.path(here::here("_energy", "data", "county_natgas_activity.RDS"))) %>%
  as_tibble() %>%
  bind_rows(
    readRDS(file.path(here::here(), "_energy/data/wisconsin_county_GasEmissions.RDS")) %>%
      rename(emissions_year = year)
  ) %>%
  mutate(
    mcf_delivered = if_else(
      is.na(mcf_delivered),
      emissions_metric_tons_co2e / natgas_ef_scf$mt_co2e_mcf,
      mcf_delivered
    ),
    sector = "Natural gas"
  )


natgas_interpolated <- left_join(
  expand.grid(
    emissions_year = 2005:2023,
    county_name = unique(natgas_raw$county_name),
    sector = "Natural gas"
  ),
  natgas_raw,
  by = join_by(emissions_year, county_name, sector)
) %>%
  mutate(
    mcf_modeled = na_interpolation(mcf_delivered, option = "linear"),
    data_source = if_else(is.na(mcf_delivered), "Interpolated", "Utility report")
  ) %>%
  cross_join(natgas_ef_scf) %>%
  mutate(
    value_emissions = mt_co2e_mcf * mcf_modeled,
    unit_emissions = "Metric tons CO2e",
    activity_type = "mcf delivered"
  ) %>%
  select(emissions_year,
    county_name,
    sector,
    activity = mcf_modeled,
    activity_type,
    data_source,
    factor_source = Source,
    value_emissions,
    unit_emissions
  )

ggplot(natgas_interpolated, aes(x = emissions_year, y = value_emissions, col = county_name)) +
  geom_line() +
  labs(title = "County natural gas emissions", x = NULL, y = "MT CO2e")


## write intermediary activity data files
# NOTE: county_elec_activity.RDS is now produced by the 01/02 pipeline scripts
# for MN counties. This version adds WI and is used for NREL proportioning only.

saveRDS(natgas_interpolated, "_energy/data/county_natgas_activity_nrel.RDS")
saveRDS(electric_interpolated, "_energy/data/county_elec_activity_nrel.RDS")




# ════════════════════════════════════════════════════════════════════
# STEP 1: NREL SECTOR PROPORTIONING
# Applied to the unadjusted county utility totals. Power plant gas is
# NOT removed here — doing so before proportioning would incorrectly
# spread the removal across residential and commercial sectors.
# ════════════════════════════════════════════════════════════════════

nrel_proportions <- nrel_emissions %>%
  group_by(county_name, source, year) %>%
  mutate(
    total_co2e = sum(co2e),
    sector_proportion = co2e / total_co2e
  ) %>%
  select(county_name, source, sector_raw, emissions_year = year, sector_proportion)

average_proportions <- nrel_proportions %>%
  group_by(county_name, source, sector_raw) %>%
  summarize(mean_prop = mean(sector_proportion), .groups = "drop")

nrel_proportions_expanded <- nrel_proportions %>%
  bind_rows(average_proportions %>%
    expand(county_name, source, sector_raw, emissions_year = 2005:2016) %>%
    left_join(average_proportions, by = join_by(county_name, source, sector_raw)) %>%
    rename(sector_proportion = mean_prop))

electric_natgas_nrel_proportioned <- bind_rows(
  electric_interpolated,
  natgas_interpolated
) %>%
  select(county_name,
    source = sector, emissions_year, value_emissions, unit_emissions,
    data_source, factor_source
  ) %>%
  mutate(source = str_to_sentence(source)) %>%
  left_join(nrel_proportions_expanded,
    by = c("county_name", "source" = "source", "emissions_year")
  ) %>%
  mutate(
    value_emissions = round(sector_proportion * value_emissions, digits = 2)
  ) %>%
  rename(sector = sector_raw) %>%
  mutate(category = if_else(source == "Electricity", "Electricity", "Building Fuel"))

saveRDS(electric_natgas_nrel_proportioned, "_energy/data/electric_natgas_nrel_proportioned_expanded.RDS")


# ════════════════════════════════════════════════════════════════════
# STEP 2: POWER PLANT NATURAL GAS SUBTRACTION FROM INDUSTRIAL SECTOR
# Power plants (GHGRP power_plant == TRUE) burn natural gas whose
# emissions are already captured in the electricity generation analysis
# via eGRID factors applied to MWh delivered. That gas lands in the
# Industrial sector slice after NREL proportioning, so we subtract it
# there — not before — to avoid incorrectly reducing residential and
# commercial totals.
#
# Minnesota River Station (Carver County, 49 MW peaker, MMPA) is
# excluded: it reported to GHGRP in only 3 years with highly variable
# values (9,709 / 83,981 / 9,709 mcf), dropped below the 25,000 MT
# reporting threshold after 2013, and its volumes are trivially small
# relative to Carver County totals. Including it would introduce a
# poorly-constrained backcast with no methodological benefit.
# ════════════════════════════════════════════════════════════════════
#
# natgas_mcf_per_scf <- 1 / 1000 # 1 mcf = 1000 scf
#
# fuel_combustion_activity <- readRDS("_industrial/data/fuel_combustion_activity.RDS")
#
# # ── 1. Measured power plant gas by county-year (GHGRP 2011–2023) ─────────────
# powerplant_natgas_measured <- fuel_combustion_activity %>%
#   filter(
#     power_plant == TRUE,
#     general_fuel_type == "Natural Gas",
#     units_activity == "scf",
#     facility_name != "Minnesota River Station" # excluded — see note above
#   ) %>%
#   mutate(mcf = value_activity * natgas_mcf_per_scf) %>%
#   group_by(county_name, reporting_year) %>%
#   summarize(
#     mcf_powerplant = sum(mcf, na.rm = TRUE),
#     n_facilities = n_distinct(facility_name),
#     facility_names = paste(sort(unique(facility_name)), collapse = "; "),
#     .groups = "drop"
#   ) %>%
#   rename(year = reporting_year)
#
# # ── 2. Backcast to 2005–2010 via Kalman smoothing ────────────────────────────
# # NAs in 2005–2010 are extrapolated backward from the 2011 anchor.
# # Large baseload plants (Black Dog, Riverside, High Bridge, Allen King,
# # Blue Lake, Elk River) were operating continuously before GHGRP, so a
# # flat backward extrapolation is reasonable.
#
# all_pp_counties <- unique(powerplant_natgas_measured$county_name)
#
# powerplant_natgas_full <- expand.grid(
#   year = 2005:2023,
#   county_name = all_pp_counties,
#   stringsAsFactors = FALSE
# ) %>%
#   left_join(powerplant_natgas_measured, by = c("year", "county_name")) %>%
#   group_by(county_name) %>%
#   arrange(year) %>%
#   mutate(
#     mcf_powerplant_modeled = na_kalman(mcf_powerplant),
#     data_type = case_when(
#       !is.na(mcf_powerplant) ~ "measured",
#       year < 2011 ~ "backcasted",
#       TRUE ~ "interpolated" # within-GHGRP gaps
#     )
#   ) %>%
#   ungroup()
#
# # ── 3. Convert to emissions and save for documentation/validation ─────────────
# powerplant_natgas_county <- powerplant_natgas_full %>%
#   cross_join(natgas_ef_scf %>% select(factor_source = Source, mt_co2e_mcf)) %>%
#   mutate(
#     value_emissions_powerplant = mt_co2e_mcf * mcf_powerplant_modeled,
#     unit_emissions = "Metric tons CO2e"
#   ) %>%
#   select(
#     year,
#     county_name,
#     mcf_powerplant = mcf_powerplant_modeled,
#     value_emissions_powerplant,
#     unit_emissions,
#     factor_source,
#     data_type,
#     n_facilities,
#     facility_names
#   )
#
# # saveRDS(powerplant_natgas_county, "_energy/data/county_powerplant_natgas.RDS")
#
# # ── 4. Subtract from the Industrial sector slice only ────────────────────────
# electric_natgas_pp_adjusted <- electric_natgas_nrel_proportioned %>%
#   left_join(
#     powerplant_natgas_county %>%
#       select(year, county_name,
#         value_emissions_powerplant,
#         data_type_pp = data_type
#       ),
#     by = c("year", "county_name")
#   ) %>%
#   mutate(
#     value_emissions_powerplant = replace_na(value_emissions_powerplant, 0),
#
#     # only touch the Industrial natural gas row
#     value_emissions_adjusted = if_else(
#       sector == "industrial" & source == "Natural gas",
#       value_emissions - value_emissions_powerplant,
#       value_emissions
#     ),
#
#     # sanity checks against the industrial slice, not the county total
#     pct_removed = if_else(
#       sector == "industrial" & source == "Natural gas" & value_emissions > 0,
#       value_emissions_powerplant / value_emissions,
#       NA_real_
#     ),
#     sanity_flag = case_when(
#       sector == "industrial" & source == "Natural gas" &
#         value_emissions_adjusted < 0 ~ "NEGATIVE: powerplant > industrial gas slice",
#       sector == "industrial" & source == "Natural gas" &
#         pct_removed > 0.90 ~ "HIGH: powerplant > 90% of industrial gas",
#       sector == "industrial" & source == "Natural gas" &
#         pct_removed > 0.50 ~ "MODERATE: powerplant 50–90% of industrial gas",
#       TRUE ~ "ok"
#     ),
#
#     # floor at zero
#     value_emissions_final = pmax(value_emissions_adjusted, 0),
#     data_source = if_else(
#       sector == "industrial" & source == "Natural gas" &
#         !is.na(data_type_pp) & data_type_pp != "measured",
#       paste0(data_source, "; powerplant subtraction backcasted"),
#       data_source
#     )
#   )
#
# # ── 5. Print and save sanity flag report ─────────────────────────────────────
# pp_flags <- electric_natgas_pp_adjusted %>%
#   filter(sanity_flag != "ok") %>%
#   select(
#     year, county_name, sector, source, value_emissions,
#     value_emissions_powerplant, value_emissions_adjusted,
#     pct_removed, sanity_flag
#   ) %>%
#   arrange(county_name, year)
#
# if (nrow(pp_flags) > 0) {
#   message(glue::glue(
#     "\n⚠  Power plant subtraction flags: {nrow(pp_flags)} county-years",
#     "\n   Review: _energy/data/powerplant_natgas_subtraction_flags.RDS\n"
#   ))
#   print(pp_flags, n = 50)
# } else {
#   message("✓ No power plant subtraction sanity flags.")
# }
#
# saveRDS(pp_flags, "_energy/data/powerplant_natgas_subtraction_flags.RDS")
#
# # Diagnostic plot: industrial natural gas before and after subtraction
# electric_natgas_pp_adjusted %>%
#   filter(sector == "Industrial", source == "Natural gas") %>%
#   select(year, county_name, before = value_emissions, after = value_emissions_final) %>%
#   pivot_longer(c(before, after), names_to = "series", values_to = "value_emissions") %>%
#   ggplot(aes(x = year, y = value_emissions, col = series)) +
#   geom_line() +
#   facet_wrap(~county_name, scales = "free_y") +
#   labs(
#     title = "Industrial natural gas: before and after power plant subtraction",
#     y = "MT CO2e", x = NULL, col = NULL
#   )
#
# # ── 6. Final tidy output ──────────────────────────────────────────────────────
# electric_natgas_nrel_proportioned_final <- electric_natgas_pp_adjusted %>%
#   select(
#     year, county_name, source, sector, category,
#     value_emissions = value_emissions_final,
#     unit_emissions,
#     data_source,
#     factor_source
#   )
#
# saveRDS(
#   electric_natgas_nrel_proportioned_final,
#   "_energy/data/electric_natgas_nrel_proportioned_expanded.RDS"
# )
