### Combine residential and business electricity predictions, apply combined guardrails
### Outputs ctu_elec_combined.rds for use by the emissions calculation script

source("R/_load_pkgs.R")

# ── Load predictions ──────────────────────────────────────────────────────────

coctu_busi <- read_rds("_energy/data-raw/predicted_coctu_business_mwh.rds")
coctu_res <- read_rds("_energy/data-raw/predicted_coctu_residential_mwh.rds")

# ── Utility totals ────────────────────────────────────────────────────────────
# Only use totals where we have complete utility coverage.
# Row-level filtering mirrors the prediction scripts: drop all-NA rows before
# checking completeness, and exclude Champlin (Connexus issues).

ctu_utility_mwh <- read_rds("_energy/data/ctu_utility_mwh.RDS")


util_totals <- ctu_utility_mwh %>%
  filter(ctu_name != "Champlin") %>%
  group_by(ctu_name, ctu_class, inventory_year) %>%
  filter(!any(is.na(total_mwh))) %>%
  summarize(
    full_total = sum(total_mwh),
    res_sum    = sum(residential_mwh, na.rm = TRUE),
    busi_sum   = sum(business_mwh, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  filter(
    full_total > 0,
    !(abs(res_sum - full_total) / full_total < 0.01),
    !(abs(busi_sum - full_total) / full_total < 0.01)
  )

# ── City-level combined electricity totals ────────────────────────────────────

city_combined <- bind_rows(
  coctu_res %>% transmute(ctu_name, ctu_class, inventory_year,
    mwh = residential_mwh
  ),
  coctu_busi %>% transmute(ctu_name, ctu_class, inventory_year,
    mwh = business_mwh
  )
) %>%
  group_by(ctu_name, ctu_class, inventory_year) %>%
  summarize(city_mwh = sum(mwh, na.rm = TRUE), .groups = "drop") %>%
  left_join(util_totals, by = c("ctu_name", "ctu_class", "inventory_year")) %>%
  filter(!is.na(full_total)) %>%
  mutate(
    gap_pct      = (full_total - city_mwh) / full_total * 100,
    needs_scale  = abs(gap_pct) > 5,
    scale_factor = if_else(needs_scale, full_total / city_mwh, 1)
  )

cat(sprintf(
  "Scaling: %d city-years up (undershoot), %d city-years down (overshoot)\n",
  sum(city_combined$gap_pct > 5, na.rm = TRUE),
  sum(city_combined$gap_pct < -5, na.rm = TRUE)
))

scale_lookup <- city_combined %>%
  filter(needs_scale) %>%
  select(ctu_name, ctu_class, inventory_year, scale_factor, gap_pct)

# ── Apply scale to both sectors ───────────────────────────────────────────────

coctu_busi_scale <- coctu_busi %>%
  left_join(scale_lookup, by = c("ctu_name", "ctu_class", "inventory_year")) %>%
  mutate(
    business_mwh = if_else(!is.na(scale_factor),
      business_mwh * scale_factor,
      business_mwh
    ),
    data_source = if_else(!is.na(scale_factor),
      paste0(data_source, if_else(
        scale_factor > 1,
        " [scaled up to utility total]",
        " [scaled down to utility total]"
      )),
      data_source
    )
  ) %>%
  select(-scale_factor, -gap_pct)

coctu_res_scale <- coctu_res %>%
  left_join(scale_lookup, by = c("ctu_name", "ctu_class", "inventory_year")) %>%
  mutate(
    residential_mwh = if_else(!is.na(scale_factor),
      residential_mwh * scale_factor,
      residential_mwh
    ),
    data_source = if_else(!is.na(scale_factor),
      paste0(data_source, if_else(
        scale_factor > 1,
        " [scaled up to utility total]",
        " [scaled down to utility total]"
      )),
      data_source
    )
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

coctu_busi_scale <- coctu_busi_scale %>%
  left_join(overshoot_mean_scale, by = c("ctu_name", "ctu_class")) %>%
  left_join(already_scaled, by = c("ctu_name", "ctu_class", "inventory_year")) %>%
  mutate(
    scaled = replace_na(scaled, FALSE),
    apply_prop = grepl("Model prediction", data_source) &
      !scaled & !is.na(mean_overshoot_scale),
    business_mwh = if_else(apply_prop,
      business_mwh * mean_overshoot_scale,
      business_mwh
    ),
    data_source = if_else(apply_prop,
      paste0(data_source, " [overshoot correction propagated]"),
      data_source
    )
  ) %>%
  select(-mean_overshoot_scale, -scaled, -apply_prop)

coctu_res_scale <- coctu_res_scale %>%
  left_join(overshoot_mean_scale, by = c("ctu_name", "ctu_class")) %>%
  left_join(already_scaled, by = c("ctu_name", "ctu_class", "inventory_year")) %>%
  mutate(
    scaled = replace_na(scaled, FALSE),
    apply_prop = grepl("Model prediction", data_source) &
      !scaled & !is.na(mean_overshoot_scale),
    residential_mwh = if_else(apply_prop,
      residential_mwh * mean_overshoot_scale,
      residential_mwh
    ),
    data_source = if_else(apply_prop,
      paste0(data_source, " [overshoot correction propagated]"),
      data_source
    )
  ) %>%
  select(-mean_overshoot_scale, -scaled, -apply_prop)

# ── Diagnostics ───────────────────────────────────────────────────────────────

overshoot_review <- city_combined %>%
  filter(gap_pct < -5) %>%
  select(
    ctu_name, ctu_class, inventory_year,
    city_mwh, full_total, gap_pct
  ) %>%
  arrange(gap_pct)

cat(sprintf(
  "Overshoot corrections applied: %d city-years\n",
  nrow(overshoot_review)
))

# ── Bind into combined dataframe ─────────────────────────────────────────────

ctu_elec_combined <- bind_rows(
  coctu_res_scale %>%
    transmute(
      coctu_id_gnis, ctu_name, ctu_class, county_name, inventory_year,
      sector = "Residential",
      mwh = residential_mwh,
      data_source
    ),
  coctu_busi_scale %>%
    transmute(
      coctu_id_gnis, ctu_name, ctu_class, county_name, inventory_year,
      sector = "Business",
      mwh = business_mwh,
      data_source
    )
) %>%
  arrange(ctu_name, ctu_class, county_name, sector, inventory_year)

stopifnot(
  ctu_elec_combined %>%
    count(
      coctu_id_gnis, ctu_name, ctu_class, county_name,
      inventory_year, sector
    ) %>%
    filter(n > 1) %>%
    nrow() == 0
)

# ── Year-over-year spike check ────────────────────────────────────────────────
# Raw YoY percent change in total electricity.

spike_check <- ctu_elec_combined %>%
  group_by(coctu_id_gnis, ctu_name, ctu_class, county_name, inventory_year) %>%
  summarize(total_mwh = sum(mwh, na.rm = TRUE), .groups = "drop") %>%
  arrange(coctu_id_gnis, inventory_year) %>%
  group_by(coctu_id_gnis, ctu_name, ctu_class, county_name) %>%
  mutate(
    prev_mwh       = lag(total_mwh),
    yoy_pct_change = (total_mwh - prev_mwh) / prev_mwh * 100
  ) %>%
  ungroup() %>%
  left_join(
    ctu_elec_combined %>%
      select(
        coctu_id_gnis, ctu_name, ctu_class, county_name,
        inventory_year, sector, data_source
      ) %>%
      pivot_wider(
        names_from = sector,
        values_from = data_source,
        names_prefix = "source_"
      ) %>%
      clean_names(),
    by = c("coctu_id_gnis", "ctu_name", "ctu_class", "county_name", "inventory_year")
  )

# Distribution to help calibrate threshold
cat("YoY change distribution (all city-years):\n")
print(quantile(spike_check$yoy_pct_change,
  probs = c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99),
  na.rm = TRUE
))

spike_threshold <- 30

spikes <- spike_check %>%
  filter(abs(yoy_pct_change) > spike_threshold, !is.na(yoy_pct_change)) %>%
  arrange(desc(abs(yoy_pct_change)))

cat(sprintf(
  "\nSpikes flagged (>%d%% YoY change): %d city-years across %d cities\n",
  spike_threshold,
  nrow(spikes),
  n_distinct(spikes$ctu_name)
))

print(spikes %>%
  select(
    ctu_name, ctu_class, county_name, inventory_year,
    total_mwh, yoy_pct_change,
    source_residential, source_business
  ) %>%
  head(30), n = 60)

spike_cities <- c(
  "Bayport",
  "Pine Springs",
  "Hastings",
  "Rogers",
  "Mendota"
)

# spike_cities <- c("Lauderdale", "Falcon Heights", "Cottage Grove",
#                   "Newport", "Maplewood")

ctu_elec_combined %>%
  filter(ctu_name %in% spike_cities) %>%
  mutate(
    source_type = case_when(
      grepl("Utility report", data_source) ~ "Utility report",
      grepl("RII", data_source) ~ "RII utility data",
      grepl("RF only", data_source) ~ "RF only",
      grepl("RF scaled", data_source) ~ "RF scaled",
      grepl("propagated", data_source) ~ "Correction propagated",
      grepl("scaled up", data_source) ~ "Scaled up to total",
      grepl("scaled down", data_source) ~ "Scaled down to total",
      grepl("partial utility floor", data_source) ~ "Partial utility floor",
      TRUE ~ "Other model"
    )
  ) %>%
  ggplot(aes(inventory_year, mwh,
    color = source_type,
    linetype = sector
  )) +
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
    "Partial utility floor"   = "#762a83",
    "Other model"             = "#bebebe"
  )) +
  facet_wrap(~ctu_name, scales = "free_y", ncol = 2) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Electricity by sector — spike diagnostic",
    x        = NULL,
    y        = "MWh",
    color    = "Data source",
    linetype = "Sector"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical"
  )

ctu_utility_mwh %>%
  filter(ctu_name %in% spike_cities) %>%
  print(n = 200)

ctu_utility_mwh %>%
  filter(ctu_name %in% spike_cities) %>%
  print(n = 200)

# ── Save ──────────────────────────────────────────────────────────────────────

saveRDS(ctu_elec_combined, "_energy/data-raw/ctu_elec_combined.rds")
