# 02_compile_eia_wi_natgas_activity.R
# ──────────────────────────────────────────────────────────────────────────────
# Query EIA's Natural Gas Query System (NGQS) for Form EIA-176 natural-gas
# deliveries and the Wisconsin company roster.
#
# IMPORTANT: NGQS Report RP1 ("176 Natural Gas Deliveries") currently returns
# "Total of All Companies" by state, not respondent-specific company deliveries.
# This script therefore creates a Wisconsin state-level benchmark and a
# separate Wisconsin respondent roster for investigating possible utility-name
# matches. It does NOT produce utility-level activity suitable for county
# allocation. Confirm that respondent-level values are available before using
# any EIA-176 figures as utility totals.
#
# RP1 default items combine sales and transportation volumes by end-use sector:
#   Residential, Commercial, Industrial, Electric Power, Vehicle Fuel, Other.
# NGQS displays volumes in thousand cubic feet (Mcf).
#
# Inputs:
#   EIA NGQS public JSON endpoints:
#     /naturalgas/ngqs/data/report
#     /naturalgas/ngqs/data/report/RP1/data/{start}/{end}/ACI/Name
#     /naturalgas/ngqs/data/report/RP6/data/{year}/{year}/ACI/Name
#
# Raw cached responses:
#   _energy/data-raw/eia_176/
#
# Outputs:
#   WI_eia176_gas_delivery_state_aggregate.RDS
#     One row per year x end-use sector; Wisconsin state total across all
#     reporting companies. This is for benchmarking, not utility allocation.
#   WI_eia176_gas_delivery_state_aggregate_wide.RDS
#     One row per year, sectors as columns.
#   WI_eia176_wi_company_roster.RDS
#     EIA-176 company IDs, company names, and filing status for Wisconsin in
#     the latest available report year; includes possible in-scope name matches.
# ──────────────────────────────────────────────────────────────────────────────

source("R/_load_pkgs.R")

# --- configuration -----------------------------------------------------------

ngqs_base_url <- "https://www.eia.gov/naturalgas/ngqs"
dir_eia_176 <- here("_energy", "data-raw", "eia_176")
dir.create(dir_eia_176, showWarnings = FALSE, recursive = TRUE)

# --- JSON download/read helper ------------------------------------------------
# Keep the original server responses in the data-raw cache for reproducibility.
# Re-download on each run so revisions to current EIA data are not hidden by a
# stale local cache.

download_ngqs_json <- function(url, destination) {
  status <- tryCatch(
    utils::download.file(
      url,
      destfile = destination,
      mode = "wb",
      quiet = TRUE
    ),
    error = function(e) {
      stop(sprintf("Failed to download NGQS URL %s: %s", url, e$message))
    }
  )

  if (!identical(as.integer(status), 0L)) {
    stop(sprintf("NGQS download returned non-zero status for %s", url))
  }
  if (!file.exists(destination) || file.info(destination)$size == 0) {
    stop(sprintf("NGQS download produced an empty file: %s", destination))
  }

  destination
}

read_ngqs_json <- function(url, destination, simplifyVector = TRUE) {
  path <- download_ngqs_json(url, destination)
  tryCatch(
    jsonlite::fromJSON(path, simplifyVector = simplifyVector),
    error = function(e) {
      stop(sprintf(
        "Could not parse NGQS JSON from %s: %s",
        url, e$message
      ))
    }
  )
}

# --- discover RP1's available annual range -----------------------------------

report_metadata_url <- paste0(ngqs_base_url, "/data/report")
report_metadata <- read_ngqs_json(
  report_metadata_url,
  file.path(dir_eia_176, "report_metadata.json"),
  simplifyVector = FALSE
)

rp1_matches <- Filter(
  function(report) identical(report$code, "RP1"),
  report_metadata
)
if (length(rp1_matches) != 1) {
  stop("Expected exactly one EIA-176 NGQS RP1 report in report metadata.")
}
rp1_metadata <- rp1_matches[[1]]

available_years <- sort(unique(as.integer(vapply(
  rp1_metadata$availableYears,
  function(year_record) year_record$ayear,
  numeric(1)
))))
if (length(available_years) == 0 || anyNA(available_years)) {
  stop("Could not determine the available RP1 EIA-176 report years.")
}

first_year <- min(available_years)
latest_year <- max(available_years)
state_abb <- "WI"

message(sprintf(
  "EIA-176 RP1 data available for %d-%d (%d annual years).",
  first_year, latest_year, length(available_years)
))

# --- retrieve RP1 Wisconsin state totals -------------------------------------
# The endpoint returns rows for all areas. Keep the complete raw JSON response,
# then filter to Wisconsin below. ACI is the NGQS default Area/Company/Item sort;
# Name requests company names rather than company IDs in the displayed field.

rp1_url <- sprintf(
  "%s/data/report/RP1/data/%d/%d/ACI/Name",
  ngqs_base_url, first_year, latest_year
)
rp1_path <- file.path(
  dir_eia_176,
  sprintf("RP1_deliveries_%d_%d.json", first_year, latest_year)
)
rp1_response <- read_ngqs_json(rp1_url, rp1_path, simplifyVector = FALSE)

if (is.null(rp1_response$data) || length(rp1_response$data) == 0) {
  stop("EIA-176 RP1 returned no delivery rows.")
}

rp1_raw <- bind_rows(lapply(rp1_response$data, tibble::as_tibble))
year_columns <- grep("^y[0-9]{4}$", names(rp1_raw), value = TRUE)
required_rp1_columns <- c("a", "b", "c", "line")
missing_rp1_columns <- setdiff(required_rp1_columns, names(rp1_raw))

if (length(missing_rp1_columns) > 0) {
  stop(sprintf(
    "EIA-176 RP1 response is missing expected columns: %s",
    paste(missing_rp1_columns, collapse = ", ")
  ))
}
if (length(year_columns) == 0) {
  stop("EIA-176 RP1 response has no annual value columns.")
}

wi_eia176_gas_delivery_state_aggregate <- rp1_raw %>%
  filter(
    str_trim(as.character(a)) == "Wisconsin",
    str_trim(as.character(b)) == "Total of All Companies"
  ) %>%
  pivot_longer(
    cols = all_of(year_columns),
    names_to = "year_column",
    values_to = "gas_delivered_mcf"
  ) %>%
  mutate(
    emissions_year = as.integer(str_remove(year_column, "^y")),
    state = state_abb,
    sector = str_remove(str_trim(as.character(c)), " Volume$"),
    report_line = as.character(line),
    unit_activity = "Mcf",
    data_source = "EIA-176 NGQS RP1 (state aggregate)",
    data_level = "state_aggregate",
    company_name = str_trim(as.character(b))
  ) %>%
  filter(!is.na(gas_delivered_mcf)) %>%
  select(
    emissions_year, state, company_name, sector, report_line,
    gas_delivered_mcf, unit_activity, data_source, data_level
  ) %>%
  arrange(emissions_year, sector)

if (nrow(wi_eia176_gas_delivery_state_aggregate) == 0) {
  stop(
    "No Wisconsin 'Total of All Companies' rows found in the EIA-176 RP1 response."
  )
}

missing_wi_years <- setdiff(
  available_years,
  unique(wi_eia176_gas_delivery_state_aggregate$emissions_year)
)
if (length(missing_wi_years) > 0) {
  warning(sprintf(
    "EIA-176 RP1 has no parsed Wisconsin delivery rows for years: %s",
    paste(missing_wi_years, collapse = ", ")
  ))
}

message("Wisconsin EIA-176 RP1 rows by year and sector:")
print(wi_eia176_gas_delivery_state_aggregate, n = Inf)

wi_eia176_gas_delivery_state_aggregate_wide <-
  wi_eia176_gas_delivery_state_aggregate %>%
  select(emissions_year, sector, gas_delivered_mcf) %>%
  pivot_wider(names_from = sector, values_from = gas_delivered_mcf) %>%
  arrange(emissions_year)

# --- retrieve RP6 Wisconsin company roster -----------------------------------
# RP6 is a respondent/company list, not annual gas-sales data. Fetch the latest
# available year's roster and retain all WI respondents, including inactive
# status rows, so historical or renamed entities can be identified during
# review.

rp6_url <- sprintf(
  "%s/data/report/RP6/data/%d/%d/ACI/Name",
  ngqs_base_url, latest_year, latest_year
)
rp6_path <- file.path(
  dir_eia_176,
  sprintf("RP6_company_list_%d.json", latest_year)
)
rp6_response <- read_ngqs_json(rp6_url, rp6_path, simplifyVector = FALSE)

if (is.null(rp6_response$data) || length(rp6_response$data) == 0) {
  stop(sprintf("EIA-176 RP6 returned no company rows for %d.", latest_year))
}

rp6_raw <- bind_rows(lapply(rp6_response$data, tibble::as_tibble))
required_rp6_columns <- c("a", "b", "c", "d", "e")
missing_rp6_columns <- setdiff(required_rp6_columns, names(rp6_raw))

if (length(missing_rp6_columns) > 0) {
  stop(sprintf(
    "EIA-176 RP6 response is missing expected columns: %s",
    paste(missing_rp6_columns, collapse = ", ")
  ))
}

wi_eia176_wi_company_roster <- rp6_raw %>%
  filter(str_to_upper(str_trim(as.character(a))) == state_abb) %>%
  transmute(
    state = state_abb,
    company_id = as.character(b),
    company_name = str_trim(as.character(c)),
    filing_status = as.character(d),
    report_year = as.integer(e),
    possible_in_scope_match = str_detect(
      str_to_upper(as.character(c)),
      "NORTHERN STATES POWER|MIDWEST NATURAL|ST CROIX|WISCONSIN GAS"
    )
  ) %>%
  arrange(desc(possible_in_scope_match), company_name)

if (nrow(wi_eia176_wi_company_roster) == 0) {
  warning(sprintf(
    "No Wisconsin company rows found in the EIA-176 RP6 roster for %d.",
    latest_year
  ))
}

message(sprintf(
  "EIA-176 RP6 Wisconsin roster (%d): %d companies; possible in-scope name matches:",
  latest_year, nrow(wi_eia176_wi_company_roster)
))
print(
  wi_eia176_wi_company_roster %>%
    filter(possible_in_scope_match),
  n = Inf
)

# --- write outputs -----------------------------------------------------------

write_rds(
  wi_eia176_gas_delivery_state_aggregate,
  here("_energy", "data", "WI_eia176_gas_delivery_state_aggregate.RDS")
)
write_rds(
  wi_eia176_gas_delivery_state_aggregate_wide,
  here("_energy", "data", "WI_eia176_gas_delivery_state_aggregate_wide.RDS")
)
write_rds(
  wi_eia176_wi_company_roster,
  here("_energy", "data", "WI_eia176_wi_company_roster.RDS")
)

message(sprintf(
  "\nWrote WI EIA-176 state totals (%d-%d) and the %d company roster rows.",
  min(wi_eia176_gas_delivery_state_aggregate$emissions_year),
  max(wi_eia176_gas_delivery_state_aggregate$emissions_year),
  nrow(wi_eia176_wi_company_roster)
))
