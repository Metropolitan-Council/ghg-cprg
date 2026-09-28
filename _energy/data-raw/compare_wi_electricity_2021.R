# Compare prior 2021 WI electricity activity with the EIA-861-based estimate.
# This is read-only with respect to both input activity RDS files.

source("R/_load_pkgs.R")

old_path <- here(
  "_energy", "data", "wisconsin_elecUtils_ActivityAndEmissions.RDS"
)
new_path <- here("_energy", "data", "WI_county_elec_activity_detail.RDS")

old_activity <- read_rds(old_path)
new_activity <- read_rds(new_path)

required_old <- c("year", "utility_name", "county_name", "coalesced_utilityCounty_mWh")
required_new <- c("emissions_year", "utility_name", "county_name", "mwh")

missing_old <- setdiff(required_old, names(old_activity))
missing_new <- setdiff(required_new, names(new_activity))

if (length(missing_old) > 0) {
  stop(sprintf(
    "Prior activity RDS is missing required columns: %s",
    paste(missing_old, collapse = ", ")
  ))
}
if (length(missing_new) > 0) {
  stop(sprintf(
    "Current activity RDS is missing required columns: %s",
    paste(missing_new, collapse = ", ")
  ))
}

normalize_key <- function(x) {
  x %>%
    str_squish() %>%
    str_remove_all("\\.") %>%
    str_replace_all("\\s*-\\s*", "-") %>%
    str_to_lower()
}

old_2021 <- old_activity %>%
  filter(year == 2021) %>%
  group_by(utility_name, county_name) %>%
  summarise(
    old_mwh = sum(coalesced_utilityCounty_mWh, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    utility_key = normalize_key(utility_name),
    county_key = normalize_key(county_name)
  )

new_2021 <- new_activity %>%
  filter(emissions_year == 2021) %>%
  group_by(utility_name, county_name) %>%
  summarise(
    new_mwh = sum(mwh, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    utility_key = normalize_key(utility_name),
    county_key = normalize_key(county_name)
  )

comparison <- full_join(
  old_2021,
  new_2021,
  by = c("utility_key", "county_key"),
  suffix = c("_old", "_new")
) %>%
  mutate(
    utility_name = coalesce(utility_name_old, utility_name_new),
    county_name = coalesce(county_name_old, county_name_new),
    difference_mwh = new_mwh - old_mwh,
    percent_change = if_else(
      !is.na(old_mwh) & old_mwh != 0,
      100 * difference_mwh / old_mwh,
      NA_real_
    ),
    comparison_status = case_when(
      is.na(old_mwh) ~ "only in current estimate",
      is.na(new_mwh) ~ "only in prior estimate",
      TRUE ~ "matched"
    )
  ) %>%
  select(
    utility_name, county_name, old_mwh, new_mwh,
    difference_mwh, percent_change, comparison_status
  ) %>%
  arrange(county_name, utility_name)

summary <- comparison %>%
  summarise(
    prior_total_mwh = sum(old_mwh, na.rm = TRUE),
    current_total_mwh = sum(new_mwh, na.rm = TRUE),
    difference_mwh = current_total_mwh - prior_total_mwh,
    percent_change = if_else(
      prior_total_mwh != 0,
      100 * difference_mwh / prior_total_mwh,
      NA_real_
    ),
    matched_utility_county_pairs = sum(comparison_status == "matched"),
    only_prior_pairs = sum(comparison_status == "only in prior estimate"),
    only_current_pairs = sum(comparison_status == "only in current estimate")
  )

message("2021 WI electricity comparison by utility and county (MWh):")
print(comparison, n = Inf)
message("\n2021 WI electricity comparison summary:")
print(summary)

write_rds(
  comparison,
  here("_energy", "data", "WI_electricity_2021_old_vs_new.RDS")
)
