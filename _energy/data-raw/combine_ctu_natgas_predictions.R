### Combine residential and business NG predictions, apply combined guardrails
### Outputs ctu_ng_combined.rds for use by the emissions calculation script

source("R/_load_pkgs.R")

mcf_to_mmbtu <- 1.037

# ── Load predictions ──────────────────────────────────────────────────────────

coctu_busi <- read_rds("_energy/data-raw/predicted_coctu_business_mcf.rds")
coctu_res  <- read_rds("_energy/data-raw/predicted_coctu_residential_mmbtu.rds")

# ── Utility totals ────────────────────────────────────────────────────────────
# Only use totals where we have complete utility coverage:
#   - St. Croix Valley and Wisconsin Gas excluded (geographic artifacts)
#   - MER, Greater MN Gas, Centennial excluded from coverage check: cities they
#     serve cannot be auto-corrected in either direction because their gas is
#     unobserved. Even undershoot scaling would be anchored to an incomplete
#     total. These cities are flagged separately.

known_nonresponders <- c(
  "Minnesota Energy Resources",
  "GREATER MINNESOTA GAS INC.",
  "Centennial Utilities"
)

ctu_utility_mcf <- read_rds("_energy/data/ctu_utility_mcf.RDS") %>%
  filter(!utility %in% c("ST. CROIX VALLEY NATURAL GAS", "WISCONSIN GAS CO")) 

util_totals <- ctu_utility_mcf %>%
  group_by(ctu_name, ctu_class, inventory_year) %>%
  summarize(
    full_total       = sum(total_mcf),   # NA if any utility missing total
    res_sum          = sum(residential_mcf, na.rm = TRUE),
    busi_sum         = sum(business_mcf,    na.rm = TRUE),
    has_nonresponder = any(utility %in% known_nonresponders),
    .groups          = "drop"
  ) %>%
  # Only rows with a reliable complete total from all utilities in our data
  filter(
    !is.na(full_total),
    !has_nonresponder,
    # Exclude years where one sector ≈ total — implies the other wasn't reported
    # and the total doesn't represent true combined delivery
    !(abs(res_sum  - full_total) / full_total < 0.01),
    !(abs(busi_sum - full_total) / full_total < 0.01)
  )

# ── City-level combined NG totals ─────────────────────────────────────────────

city_combined <- bind_rows(
  coctu_res  %>% transmute(ctu_name, ctu_class, inventory_year,
                           ng_mcf = ng_mmbtu / mcf_to_mmbtu),
  coctu_busi %>% transmute(ctu_name, ctu_class, inventory_year,
                           ng_mcf = business_mcf)
) %>%
  group_by(ctu_name, ctu_class, inventory_year) %>%
  summarize(city_ng_mcf = sum(ng_mcf, na.rm = TRUE), .groups = "drop") %>%
  left_join(util_totals, by = c("ctu_name", "ctu_class", "inventory_year")) %>%
  filter(!is.na(full_total)) %>%
  mutate(
    gap_pct      = (full_total - city_ng_mcf) / full_total * 100,
    needs_scale  = abs(gap_pct) > 5,
    scale_factor = if_else(needs_scale, full_total / city_ng_mcf, 1)
  )

cat(sprintf(
  "Scaling: %d city-years up (undershoot), %d city-years down (overshoot)\n",
  sum(city_combined$gap_pct > 5,  na.rm = TRUE),
  sum(city_combined$gap_pct < -5, na.rm = TRUE)
))

scale_lookup <- city_combined %>%
  filter(needs_scale) %>%
  select(ctu_name, ctu_class, inventory_year, scale_factor, gap_pct)

# ── Undershoot: scale both sectors up proportionally ─────────────────────────

# ── Apply scale to both sectors ───────────────────────────────────────────────

coctu_busi <- coctu_busi %>%
  left_join(scale_lookup, by = c("ctu_name", "ctu_class", "inventory_year")) %>%
  mutate(
    business_mcf = if_else(!is.na(scale_factor),
                           business_mcf * scale_factor,
                           business_mcf),
    data_source  = if_else(!is.na(scale_factor),
                           paste0(data_source, if_else(
                             scale_factor > 1,
                             " [scaled up to utility total]",
                             " [scaled down to utility total]"
                           )),
                           data_source)
  ) %>%
  select(-scale_factor, -gap_pct)

coctu_res <- coctu_res %>%
  left_join(scale_lookup, by = c("ctu_name", "ctu_class", "inventory_year")) %>%
  mutate(
    ng_mmbtu        = if_else(!is.na(scale_factor),
                              ng_mmbtu * scale_factor,
                              ng_mmbtu),
    total_res_mmbtu = if_else(!is.na(scale_factor),
                              ng_mmbtu + propane_mmBtu + fueloil_other_mmBtu,
                              total_res_mmbtu),
    data_source     = if_else(!is.na(scale_factor),
                              paste0(data_source, if_else(
                                scale_factor > 1,
                                " [scaled up to utility total]",
                                " [scaled down to utility total]"
                              )),
                              data_source)
  ) %>%
  select(-scale_factor, -gap_pct)

# ── Propagate overshoot corrections to non-scaled model-predicted years ───────
# Cities with overshoot corrections in utility-anchored years but
# model-predicted years outside that window would otherwise spike at the
# boundary. Apply the mean overshoot scale to those uncorrected years.

overshoot_mean_scale <- scale_lookup %>%
  filter(scale_factor < 1) %>%
  group_by(ctu_name, ctu_class) %>%
  summarize(mean_overshoot_scale = mean(scale_factor), .groups = "drop")

already_scaled <- scale_lookup %>%
  select(ctu_name, ctu_class, inventory_year) %>%
  mutate(scaled = TRUE)

coctu_busi <- coctu_busi %>%
  left_join(overshoot_mean_scale, by = c("ctu_name", "ctu_class")) %>%
  left_join(already_scaled, by = c("ctu_name", "ctu_class", "inventory_year")) %>%
  mutate(
    scaled       = replace_na(scaled, FALSE),
    apply_prop   = grepl("Model prediction", data_source) &
      !scaled & !is.na(mean_overshoot_scale),
    business_mcf = if_else(apply_prop,
                           business_mcf * mean_overshoot_scale,
                           business_mcf),
    data_source  = if_else(apply_prop,
                           paste0(data_source, " [overshoot correction propagated]"),
                           data_source)
  ) %>%
  select(-mean_overshoot_scale, -scaled, -apply_prop)

coctu_res <- coctu_res %>%
  left_join(overshoot_mean_scale, by = c("ctu_name", "ctu_class")) %>%
  left_join(already_scaled, by = c("ctu_name", "ctu_class", "inventory_year")) %>%
  mutate(
    scaled          = replace_na(scaled, FALSE),
    apply_prop      = grepl("Model prediction", data_source) &
      !scaled & !is.na(mean_overshoot_scale),
    ng_mmbtu        = if_else(apply_prop,
                              ng_mmbtu * mean_overshoot_scale,
                              ng_mmbtu),
    total_res_mmbtu = if_else(apply_prop,
                              ng_mmbtu + propane_mmBtu + fueloil_other_mmBtu,
                              total_res_mmbtu),
    data_source     = if_else(apply_prop,
                              paste0(data_source, " [overshoot correction propagated]"),
                              data_source)
  ) %>%
  select(-mean_overshoot_scale, -scaled, -apply_prop)

# ── Diagnostics ───────────────────────────────────────────────────────────────

overshoot_review <- city_combined %>%
  filter(gap_pct < -5) %>%
  select(ctu_name, ctu_class, inventory_year,
         city_ng_mcf, full_total, gap_pct) %>%
  arrange(gap_pct)

cat(sprintf("Overshoot corrections applied: %d city-years\n",
            nrow(overshoot_review)))

# ── Bind into combined dataframe ─────────────────────────────────────────────

ctu_ng_combined <- bind_rows(
  coctu_res %>%
    transmute(
      coctu_id_gnis, ctu_name, ctu_class, county_name, inventory_year,
      sector              = "Residential",
      ng_mmbtu            = ng_mmbtu,
      propane_mmbtu       = propane_mmBtu,
      fueloil_other_mmbtu = fueloil_other_mmBtu,
      total_res_mmbtu     = total_res_mmbtu,
      data_source
    ),
  coctu_busi %>%
    transmute(
      coctu_id_gnis, ctu_name, ctu_class, county_name, inventory_year,
      sector              = "Business",
      ng_mmbtu            = business_mcf * mcf_to_mmbtu,
      propane_mmbtu       = NA_real_,
      fueloil_other_mmbtu = NA_real_,
      total_res_mmbtu     = NA_real_,
      data_source
    )
) %>%
  arrange(ctu_name, ctu_class, county_name, sector, inventory_year)

stopifnot(
  ctu_ng_combined %>%
    count(coctu_id_gnis, ctu_name, ctu_class, county_name,
          inventory_year, sector) %>%
    filter(n > 1) %>%
    nrow() == 0
)

# ── Year-over-year spike check, HDD-normalized ────────────────────────────────
# Normalise total NG by heating degree days before computing YoY changes.
# This removes the bulk of year-to-year variation driven by cold/warm winters,
# leaving residual changes that are more likely to reflect data discontinuities
# (model switches, utility reporting changes, scaling corrections, etc.)

noaa_year <- readRDS("_meta/data/noaa_weather_monthly.rds") %>%
  group_by(inventory_year) %>%
  summarize(
    heating_degree_days = sum(heating_degree_days),
    cooling_degree_days = sum(cooling_degree_days),
    temperature         = mean(dry_bulb_temp)
  )

spike_check <- ctu_ng_combined %>%
  group_by(coctu_id_gnis, ctu_name, ctu_class, county_name, inventory_year) %>%
  summarize(total_ng_mmbtu = sum(ng_mmbtu, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    noaa_year %>% select(inventory_year, heating_degree_days),
    by = "inventory_year"
  ) %>%
  mutate(ng_per_hdd = total_ng_mmbtu / heating_degree_days) %>%
  arrange(coctu_id_gnis, inventory_year) %>%
  group_by(coctu_id_gnis, ctu_name, ctu_class, county_name) %>%
  mutate(
    prev_ng_per_hdd = lag(ng_per_hdd),
    yoy_pct_change  = (ng_per_hdd - prev_ng_per_hdd) / prev_ng_per_hdd * 100
  ) %>%
  ungroup() %>%
  # Join data sources for both sectors so spikes can be attributed
  left_join(
    ctu_ng_combined %>%
      select(coctu_id_gnis, ctu_name, ctu_class, county_name,
             inventory_year, sector, data_source) %>%
      pivot_wider(
        names_from  = sector,
        values_from = data_source,
        names_prefix = "source_"
      ) %>%
      clean_names(),
    by = c("coctu_id_gnis", "ctu_name", "ctu_class", "county_name", "inventory_year")
  )

# Distribution to help calibrate threshold
cat("YoY HDD-adjusted change distribution (all city-years):\n")
print(quantile(spike_check$yoy_pct_change,
               probs = c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99),
               na.rm = TRUE))

# Flag large residual spikes
# Default threshold: 30% change in HDD-adjusted usage
# Check the quantile output above and tune if needed
spike_threshold <- 30

spikes <- spike_check %>%
  filter(abs(yoy_pct_change) > spike_threshold, !is.na(yoy_pct_change)) %>%
  arrange(desc(abs(yoy_pct_change)))

cat(sprintf("\nSpikes flagged (>%d%% HDD-adjusted YoY change): %d city-years across %d cities\n",
            spike_threshold,
            nrow(spikes),
            n_distinct(spikes$ctu_name)))

print(spikes %>%
        select(ctu_name, ctu_class, county_name, inventory_year,
               total_ng_mmbtu, ng_per_hdd, yoy_pct_change,
               source_residential, source_business) %>%
        head(30))

spike_cities <- c("Norwood Young America", "Centerville", "Hampton", "Camden")

ctu_ng_combined %>%
  filter(ctu_name %in% spike_cities) %>%
  mutate(
    mmbtu = if_else(sector == "Business", ng_mmbtu, total_res_mmbtu),
    source_type = case_when(
      grepl("Utility report", data_source)      ~ "Utility report",
      grepl("RII",            data_source)      ~ "RII utility data",
      grepl("RF only",        data_source)      ~ "RF only",
      grepl("RF scaled",      data_source)      ~ "RF scaled",
      grepl("propagated",     data_source)      ~ "Correction propagated",
      grepl("scaled up",      data_source)      ~ "Scaled up to total",
      grepl("scaled down",    data_source)      ~ "Scaled down to total",
      TRUE                                      ~ "Other model"
    )
  ) %>%
  ggplot(aes(inventory_year, mmbtu,
             color = source_type,
             linetype = sector)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  scale_color_manual(values = c(
    "Utility report"          = "#2166ac",
    "RII utility data"        = "#4dac26",
    "RF only"                 = "#d7191c",
    "RF scaled"               = "#fdae61",
    "Correction propagated"   = "#f46d43",
    "Scaled up to total"      = "#abdda4",
    "Scaled down to total"    = "#9e0142",
    "Other model"             = "#bebebe"
  )) +
  facet_wrap(~ ctu_name, scales = "free_y", ncol = 2) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Natural gas by sector — spike diagnostic",
    x        = NULL,
    y        = "mmBtu",
    color    = "Data source",
    linetype = "Sector"
  ) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.box      = "vertical")

# ── Save ──────────────────────────────────────────────────────────────────────

saveRDS(ctu_ng_combined,  "_energy/data-raw/ctu_ng_combined.rds")
saveRDS(spike_check,      "_energy/data-raw/ctu_ng_spike_check.rds")
saveRDS(spikes,           "_energy/data-raw/ctu_ng_spikes_flagged.rds")
