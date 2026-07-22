# 02_compile_county_elec_activity.R
# ──────────────────────────────────────────────────────────────────────────────
# Compile a complete county-level electricity activity table by:
#   1. Loading observed 7610 county data from 01 script
#   2. Downloading EIA 861 utility-level MN sales (state totals by utility)
#   3. Gap-filling missing 7610 utility-years using EIA 861 state totals
#      allocated to counties via proportions from adjacent filed years
#   4. Backcasting to 2005 for utilities that only filed 7610s starting later
#
# Inputs:
#   county_elec_7610.rds          — from 01_compile script
#   county_elec_7610_known_gaps.rds — from 01_compile script
#   EIA 861 annual zip files      — downloaded from eia.gov
#
# Outputs:
#   county_elec_activity.rds      — complete county × utility × year activity
#                                   table, 2005–latest, with provenance tags
#
# Columns: emissions_year, county_name, county_code, utility_name,
#          value_activity, unit_activity, data_source
#
# data_source values:
#   "mn_7610"              — observed from 7610 filing
#   "manual_annual_report" — manually entered from utility annual report
#   "eia_861_gapfill"      — gap-filled using EIA 861 + county proportions
#   "eia_861_backcast"     — backcast using EIA 861 + county proportions
# ──────────────────────────────────────────────────────────────────────────────

source("R/_load_pkgs.R")

# --- configuration -----------------------------------------------------------

backcast_start <- 2005L
scope_counties <- c(
  "Anoka", "Carver", "Dakota", "Hennepin", "Ramsey",
  "Scott", "Washington", "Sherburne", "Chisago"
)

# --- load 01 outputs ---------------------------------------------------------

elec_7610 <- read_rds(here("_energy", "data", "county_elec_7610.rds"))
known_gaps <- read_rds(here("_energy", "data", "county_elec_7610_known_gaps.rds"))

latest_year <- max(elec_7610$emissions_year)
full_year_range <- backcast_start:latest_year

message(sprintf(
  "Loaded 7610 data: %d rows, years %d-%d",
  nrow(elec_7610), min(elec_7610$emissions_year), latest_year
))

# ═════════════════════════════════════════════════════════════════════════════
# PART 1: Download and compile EIA 861 utility-level data
# ═════════════════════════════════════════════════════════════════════════════

# EIA 861 "Sales to Ultimate Customers" files contain utility-level MWh by
# state and sector. File naming varies by year but follows a predictable
# pattern inside the zip archives.

dir_eia_861 <- here("_energy", "data-raw", "eia_861")
dir.create(dir_eia_861, showWarnings = FALSE, recursive = TRUE)

# --- name harmonization for EIA 861 names -----------------------------------
# EIA utility names don't always match 7610 names. Map to canonical names.


eia_name_lookup <- c(
  # --- Great River Energy and member co-ops ---
  "Great River Energy" = "Great River Energy",
  "Dakota Electric Assn" = "Great River Energy",
  "Dakota Elec Assn" = "Great River Energy",
  "Dakota Electric Association" = "Great River Energy",
  "Wright-Hennepin Coop Elec Assn" = "Great River Energy",
  "Wright-Hennepin Cooperative" = "Great River Energy",
  "Minnesota Valley Elec Coop" = "Great River Energy",
  "Minnesota Valley Electric Coop" = "Great River Energy",
  "Minnesota Valley Coop L&P Assn" = "Great River Energy",
  "McLeod Cooperative Power Assn" = "Great River Energy",
  "McLeod Coop Power Assn" = "Great River Energy",
  "McLeod Cooperative Power" = "Great River Energy",
  "East Central Energy" = "Great River Energy",
  "East Central Elec Assn" = "Great River Energy",
  "Goodhue County Coop Elec Assn" = "Great River Energy",
  "Goodhue County Coop Electric" = "Great River Energy",
  "Goodhue County Cooperative" = "Great River Energy",

  # --- Xcel Energy / Northern States Power ---
  "Northern States Power Co - Minnesota" = "Xcel Energy",
  "Northern States Power Co" = "Xcel Energy",
  "Northern States Power Company" = "Xcel Energy",
  "Xcel Energy Inc" = "Xcel Energy",
  "Xcel Energy" = "Xcel Energy",

  # --- Connexus Energy (was Anoka Electric Coop pre-2008) ---
  "Connexus Energy" = "Connexus Energy",
  "Anoka Electric Coop" = "Connexus Energy",

  # --- Municipal utilities ---
  "City of Shakopee" = "Shakopee Public Utilities",
  "Shakopee Public Utilities Comm" = "Shakopee Public Utilities",
  "Elk River Muni Utilities" = "Elk River Municipal Utilities",
  "Elk River Municipal Utilities" = "Elk River Municipal Utilities",
  "City of Elk River" = "Elk River Municipal Utilities",
  "City of Chaska" = "City of Chaska",
  "City of Chaska - (MN)" = "City of Chaska",
  "Princeton Public Utils Comm" = "Princeton Public Utilities",
  "Princeton Public Utilities" = "Princeton Public Utilities",
  "City of North Branch" = "City of North Branch",
  "North Branch Water & Light" = "City of North Branch",
  "North Branch Water & Light Comm" = "City of North Branch",
  "City of Anoka" = "City of Anoka"
)



harmonize_eia_names <- function(names) {
  idx <- match(names, names(eia_name_lookup))
  ifelse(is.na(idx), names, eia_name_lookup[idx])
}

# --- download EIA 861 files --------------------------------------------------

download_eia_861 <- function(years, dest_dir) {
  for (yr in years) {
    zip_file <- file.path(dest_dir, sprintf("f861%d.zip", yr))

    if (file.exists(zip_file)) next

    # URL pattern varies by year:
    #   2024+  (latest): .../eia861/zip/f861YYYY.zip
    #   2012-2023:       .../eia861/archive/zip/f861YYYY.zip
    #   2005-2011:       .../eia861/archive/zip/861_YYYY.zip (reformatted)
    # The reformatted files (2005-2011) have standardized column names
    # matching the newer files. Prefer those over the originals.

    if (yr >= 2024) {
      url <- sprintf(
        "https://www.eia.gov/electricity/data/eia861/zip/f861%d.zip", yr
      )
    } else if (yr >= 2012) {
      url <- sprintf(
        "https://www.eia.gov/electricity/data/eia861/archive/zip/f861%d.zip", yr
      )
    } else {
      # 2005-2011: use reformatted files
      url <- sprintf(
        "https://www.eia.gov/electricity/data/eia861/archive/zip/861_%d.zip", yr
      )
    }

    message(sprintf("Downloading EIA 861 for %d...", yr))
    tryCatch(
      download.file(url, zip_file, mode = "wb", quiet = TRUE),
      error = function(e) {
        warning(sprintf("Failed to download %d: %s", yr, e$message))
        # Clean up partial download
        if (file.exists(zip_file)) file.remove(zip_file)
      }
    )
  }
}

# --- parse EIA 861 Sales to Ultimate Customers --------------------------------
# File naming inside zips has varied over the years. We look for the sales
# file by pattern matching.

parse_eia_861_sales <- function(zip_file, year) {
  temp_dir <- tempdir()

  # List files in zip, find the sales file
  zip_contents <- unzip(zip_file, list = TRUE)$Name
  # Pattern varies: "Sales_Ult_Cust_2020.xlsx", "sales_ult_cust_2013.xlsx",
  # older years may be .xls or .csv
  sales_file <- grep("sales_ult_cust|Sales_Ult_Cust|sales_ult", zip_contents,
    value = TRUE, ignore.case = TRUE
  )

  if (length(sales_file) == 0) {
    warning(sprintf(
      "No Sales_Ult_Cust file found in %s. Contents: %s",
      basename(zip_file),
      paste(zip_contents, collapse = ", ")
    ))
    return(tibble())
  }

  # Use the first match (xlsx preferred over csv)
  sales_file <- sales_file[1]
  unzip(zip_file, files = sales_file, exdir = temp_dir, overwrite = TRUE)
  extracted_path <- file.path(temp_dir, sales_file)

  # --- Detect header row ------------------------------------------------------
  # Some years have multi-row headers or metadata rows above the data.
  # Strategy: read without headers, find the row containing "Utility" or
  # "State" as text, use that as the header row.

  if (grepl("\\.xlsx$|\\.xls$", sales_file, ignore.case = TRUE)) {
    # First read a preview to find the header
    preview <- tryCatch(
      read_excel(extracted_path, col_names = FALSE, n_max = 10),
      error = function(e) NULL
    )

    if (is.null(preview)) {
      warning(sprintf("Could not read %s for year %d", sales_file, year))
      return(tibble())
    }

    # Find the row that looks like column headers (contains "Utility" or "State")
    header_row <- NA
    for (r in seq_len(nrow(preview))) {
      row_vals <- toupper(as.character(unlist(preview[r, ])))
      if (any(grepl("UTILITY", row_vals)) && any(grepl("STATE", row_vals))) {
        header_row <- r
        break
      }
    }

    skip_n <- if (!is.na(header_row)) header_row - 1 else 0

    raw <- tryCatch(
      read_excel(extracted_path, skip = skip_n, guess_max = 10000),
      error = function(e) {
        warning(sprintf(
          "Failed to read %s for year %d: %s",
          sales_file, year, e$message
        ))
        return(NULL)
      }
    )
  } else {
    raw <- tryCatch(
      read_csv(extracted_path, show_col_types = FALSE, guess_max = 10000),
      error = function(e) NULL
    )
  }

  if (is.null(raw) || nrow(raw) == 0) {
    return(tibble())
  }

  # Standardize column names: lowercase, strip whitespace/punctuation
  names(raw) <- tolower(trimws(names(raw)))
  # Replace multiple spaces/underscores with single underscore
  names(raw) <- gsub("[\\s._]+", "_", names(raw))
  # Remove trailing underscores
  names(raw) <- gsub("_$", "", names(raw))

  # --- Find key columns by pattern matching -----------------------------------
  # EIA 861 files have character encoding issues where 's' is dropped/replaced
  # with '_' in some column names (e.g., "state" → "_tate", "service" →
  # "_ervice"). Column names also vary across years. Match broadly.
  col_names <- names(raw)

  find_col <- function(patterns) {
    for (p in patterns) {
      matches <- grep(p, col_names, value = TRUE, ignore.case = TRUE)
      if (length(matches) > 0) {
        return(matches[1])
      }
    }
    NA_character_
  }

  name_col <- find_col(c(
    "^utility_name$", "^utility_n", "utility.*name",
    "^utility$"
  ))
  state_col <- find_col(c("^state$", "^_tate$", "tate$", "^st$"))

  # Sector MWh columns: the header is a multi-line cell that collapses into
  # positional names like "megawatthour_10", "megawatthour_13", etc.
  # They always appear in order: residential, commercial, industrial,
  # transportation, total. Find all megawatthour columns and assign by position.
  mwh_cols <- grep("megawatthour", col_names, value = TRUE, ignore.case = TRUE)

  # Also check for cleanly-named columns (some years may have proper names)
  res_col <- find_col(c("^residential$", "res.*sales", "res.*megawatt", "res.*mwh"))
  com_col <- find_col(c(
    "^commercial$", "comm.*sales", "com.*sales",
    "comm.*megawatt", "com.*mwh"
  ))
  ind_col <- find_col(c("^industrial$", "ind.*sales", "ind.*megawatt", "ind.*mwh"))
  tot_col <- find_col(c("^total$", "total.*sales", "total.*megawatt"))

  # If named sector columns weren't found, fall back to positional megawatthour
  if (is.na(res_col) && length(mwh_cols) >= 5) {
    res_col <- mwh_cols[1] # 1st megawatthour = residential
    com_col <- mwh_cols[2] # 2nd = commercial
    ind_col <- mwh_cols[3] # 3rd = industrial
    # mwh_cols[4] = transportation (skip)
    tot_col <- mwh_cols[5] # 5th = total
  } else if (is.na(res_col) && length(mwh_cols) >= 3) {
    # Fewer columns — assign what we can, compute total
    res_col <- mwh_cols[1]
    com_col <- mwh_cols[2]
    ind_col <- mwh_cols[3]
  }

  if (is.na(name_col) || is.na(state_col)) {
    warning(sprintf(
      "Could not find utility_name or state column for year %d. Columns: %s",
      year, paste(col_names, collapse = ", ")
    ))
    return(tibble())
  }

  # --- Filter to MN and build output ------------------------------------------
  # Filter first, then build columns — avoids size mismatch errors
  mn_rows <- which(toupper(as.character(raw[[state_col]])) == "MN")

  if (length(mn_rows) == 0) {
    return(tibble())
  }

  raw_mn <- raw[mn_rows, ]

  result <- tibble(
    utility_name_raw = as.character(raw_mn[[name_col]]),
    state            = "MN",
    emissions_year   = as.integer(year)
  )

  # Safely add sector columns, coercing "." to NA
  safe_numeric <- function(x) suppressWarnings(as.numeric(as.character(x)))

  if (!is.na(res_col)) result$residential <- safe_numeric(raw_mn[[res_col]])
  if (!is.na(com_col)) result$commercial <- safe_numeric(raw_mn[[com_col]])
  if (!is.na(ind_col)) result$industrial <- safe_numeric(raw_mn[[ind_col]])
  if (!is.na(tot_col)) result$total <- safe_numeric(raw_mn[[tot_col]])

  # Compute total if not present but sectors are
  if (!"total" %in% names(result) &&
    all(c("residential", "commercial", "industrial") %in% names(result))) {
    result$total <- rowSums(
      result[, c("residential", "commercial", "industrial")],
      na.rm = TRUE
    )
  }

  # Apply name harmonization and drop zero/NA totals
  result <- result %>%
    mutate(utility_name = harmonize_eia_names(utility_name_raw)) %>%
    filter(!is.na(total), total > 0)

  message(sprintf("  %d: %d MN utilities parsed", year, nrow(result)))
  result
}

# --- run EIA download and parse -----------------------------------------------

message("\n=== Downloading and parsing EIA 861 data ===")
download_eia_861(full_year_range, dir_eia_861)

eia_861_raw <- map_dfr(full_year_range, function(yr) {
  zip_file <- file.path(dir_eia_861, sprintf("f861%d.zip", yr))
  if (!file.exists(zip_file)) {
    return(tibble())
  }
  parse_eia_861_sales(zip_file, yr)
})

# Aggregate: some utilities (e.g. Dakota Electric, Wright-Hennepin, MN Valley)
# get mapped to "Great River Energy" via harmonization. Sum them up.
eia_861 <- eia_861_raw %>%
  group_by(utility_name, emissions_year) %>%
  summarise(
    across(
      any_of(c("residential", "commercial", "industrial", "total")),
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

# Keep only utilities that appear in our 7610 data or known_gaps
scope_utilities <- unique(c(
  elec_7610$utility_name,
  known_gaps$utility_name
))

eia_861 <- eia_861 %>%
  filter(utility_name %in% scope_utilities)

message(sprintf(
  "EIA 861 parsed: %d utility-years across %d utilities, %d-%d",
  nrow(eia_861),
  n_distinct(eia_861$utility_name),
  min(eia_861$emissions_year),
  max(eia_861$emissions_year)
))

# ═════════════════════════════════════════════════════════════════════════════
# PART 2: Compute county allocation proportions from 7610 + EIA 861
# ═════════════════════════════════════════════════════════════════════════════
# For each utility, compute each scope county's share of the utility's
# STATEWIDE MN total. The 7610 data only contains our 9 scope counties,
# so we use the EIA 861 statewide total as the denominator to get true
# statewide proportions (not proportions of the 9-county subtotal).

county_proportions <- elec_7610 %>%
  select(
    utility_name, emissions_year, county_name, county_code,
    value_activity
  ) %>%
  left_join(
    eia_861 %>% select(utility_name, emissions_year, state_total = total),
    by = c("utility_name", "emissions_year")
  ) %>%
  filter(!is.na(state_total), state_total > 0) %>%
  mutate(
    county_proportion = value_activity / state_total
  ) %>%
  select(
    utility_name, emissions_year, county_name, county_code,
    county_proportion
  )

# QA: check that per-utility proportions sum to something reasonable
# (should be well under 1.0 — our 9 counties are a subset of the state)
proportion_check <- county_proportions %>%
  group_by(utility_name, emissions_year) %>%
  summarise(proportion_sum = sum(county_proportion), .groups = "drop")

message("\nScope-county share of statewide total by utility:")
proportion_check %>%
  group_by(utility_name) %>%
  summarise(
    avg_share = mean(proportion_sum),
    min_share = min(proportion_sum),
    max_share = max(proportion_sum),
    .groups = "drop"
  ) %>%
  print()

# Flag if any utility's scope share exceeds 1.0 (would indicate a mismatch)
over_one <- proportion_check %>% filter(proportion_sum > 1.0)
if (nrow(over_one) > 0) {
  warning("Some utility-years have scope proportions > 1.0 — name mismatch likely:")
  print(over_one)
}

# For backcasting, compute a stable average proportion using the earliest
# N years of available data per utility. For gap-filling within the 7610
# range, use the nearest available year's proportions.

earliest_proportions <- county_proportions %>%
  group_by(utility_name) %>%
  filter(emissions_year <= min(emissions_year) + 2) %>%
  group_by(utility_name, county_name, county_code) %>%
  summarise(
    avg_proportion = mean(county_proportion, na.rm = TRUE),
    n_years_used = n(),
    .groups = "drop"
  )


# ═════════════════════════════════════════════════════════════════════════════
# PARTS 3+4: Fill all missing utility-years (gap-fill + backcast combined)
# ═════════════════════════════════════════════════════════════════════════════
# For each utility, build a complete 2005–latest grid and fill every missing
# year using EIA 861 state totals × nearest available county proportions.
# No distinction between "gap-fill" and "backcast" — same method either way.

# --- suspect data override ----------------------------------------------------
# Some 7610 filings have implausible values (e.g., GRE 2022-2023 where member
# co-ops appear to have started filing independently, causing the GRE filing
# to drop 40-95%). Remove these from the observed data so the fill logic
# replaces them with EIA 861 estimates instead.

suspect_filings <- tribble(
  ~utility_name,        ~emissions_year, ~reason,
  "Great River Energy", 2022L,           "GRE 7610 filing appears to exclude some member co-op load (down ~40%)",
  "Great River Energy", 2023L,           "GRE 7610 filing appears to exclude most member co-op load (down ~95%)"
)

n_suspect <- elec_7610 %>%
  semi_join(suspect_filings, by = c("utility_name", "emissions_year")) %>%
  nrow()

if (n_suspect > 0) {
  message(sprintf(
    "\nRemoving %d rows from %d suspect utility-year filings (will replace with EIA 861):",
    n_suspect, nrow(suspect_filings)
  ))
  print(suspect_filings)

  elec_7610 <- elec_7610 %>%
    anti_join(suspect_filings, by = c("utility_name", "emissions_year"))

  # Also recompute county proportions without the suspect years
  county_proportions <- county_proportions %>%
    anti_join(suspect_filings, by = c("utility_name", "emissions_year"))
}

observed_utility_years <- elec_7610 %>%
  distinct(utility_name, emissions_year)

# Build the complete grid: every utility × every year in full_year_range
all_utilities <- unique(elec_7610$utility_name)

complete_grid <- expand_grid(
  utility_name   = all_utilities,
  emissions_year = full_year_range
)

# Find all missing utility-years
missing_utility_years <- complete_grid %>%
  anti_join(observed_utility_years, by = c("utility_name", "emissions_year")) %>%
  # Only fill where we have EIA 861 data
  semi_join(eia_861, by = c("utility_name", "emissions_year"))

message(sprintf(
  "\n%d utility-year combinations to fill across %d utilities",
  nrow(missing_utility_years), n_distinct(missing_utility_years$utility_name)
))

# For each missing utility-year, find the nearest year with county proportions
# and allocate the EIA 861 state total to counties
fill_records <- list()

for (i in seq_len(nrow(missing_utility_years))) {
  gap <- missing_utility_years[i, ]

  # Get EIA 861 state total for this utility-year
  eia_total <- eia_861 %>%
    filter(
      utility_name == gap$utility_name,
      emissions_year == gap$emissions_year
    ) %>%
    pull(total)

  if (length(eia_total) == 0 || is.na(eia_total)) next

  # Find the nearest year with county proportions
  available_years <- county_proportions %>%
    filter(utility_name == gap$utility_name) %>%
    distinct(emissions_year) %>%
    pull()

  if (length(available_years) == 0) next

  nearest_year <- available_years[which.min(abs(available_years - gap$emissions_year))]

  # Get county proportions from the nearest year
  props <- county_proportions %>%
    filter(
      utility_name == gap$utility_name,
      emissions_year == nearest_year
    )

  # Tag provenance: backcast if before first 7610, gap-fill if within range
  first_7610 <- min(
    (observed_utility_years %>%
      filter(utility_name == gap$utility_name))$emissions_year
  )

  source_tag <- if_else(
    gap$emissions_year < first_7610,
    "eia_861_backcast",
    "eia_861_gapfill"
  )

  # Allocate the EIA 861 total to counties
  filled <- props %>%
    mutate(
      emissions_year = gap$emissions_year,
      value_activity = eia_total * county_proportion,
      unit_activity  = "mwh",
      data_source    = source_tag
    ) %>%
    select(
      emissions_year, county_name, county_code, utility_name,
      value_activity, unit_activity, data_source
    ) %>%
    filter(value_activity > 0)

  fill_records[[i]] <- filled
}

filled_data <- bind_rows(fill_records)

message(sprintf(
  "Filled %d county-utility-year rows (%d backcast, %d gap-fill)",
  nrow(filled_data),
  sum(filled_data$data_source == "eia_861_backcast"),
  sum(filled_data$data_source == "eia_861_gapfill")
))

# ═════════════════════════════════════════════════════════════════════════════
# PART 5: Combine everything
# ═════════════════════════════════════════════════════════════════════════════

# Strip entity_id from 7610 data before binding (not present in gap/backcast)
elec_7610_slim <- elec_7610 %>%
  select(
    emissions_year, county_name, county_code, utility_name,
    value_activity, unit_activity, data_source
  )

utility_elec_activity <- bind_rows(
  elec_7610_slim,
  filled_data
) %>%
  arrange(county_name, utility_name, emissions_year)

# --- check for duplicates ----------------------------------------------------

dupes <- utility_elec_activity %>%
  group_by(emissions_year, county_name, utility_name) %>%
  filter(n() > 1)

if (nrow(dupes) > 0) {
  warning(sprintf("%d duplicate county-utility-year rows found!", nrow(dupes)))
  print(dupes)
}

# --- summary ------------------------------------------------------------------

provenance_summary <- utility_elec_activity %>%
  group_by(data_source) %>%
  summarise(
    n_rows = n(),
    total_mwh = sum(value_activity, na.rm = TRUE),
    .groups = "drop"
  )

message("\n=== Provenance Summary ===")
print(provenance_summary)

# ═════════════════════════════════════════════════════════════════════════════
# PART 6: Diagnostic plots
# ═════════════════════════════════════════════════════════════════════════════

# --- Plot 1: County totals over time, colored by provenance ------------------
# Show how much of each county-year comes from observed vs filled vs backcast

county_year_provenance <- utility_elec_activity %>%
  mutate(
    source_label = case_when(
      data_source == "mn_7610" ~ "7610 observed",
      data_source == "manual_annual_report" ~ "7610 observed",
      data_source == "eia_861_gapfill" ~ "EIA 861 gap-fill",
      data_source == "eia_861_backcast" ~ "EIA 861 backcast"
    )
  ) %>%
  group_by(emissions_year, county_name, source_label) %>%
  summarise(total_mwh = sum(value_activity, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    source_label = factor(source_label,
      levels = c("7610 observed", "EIA 861 gap-fill", "EIA 861 backcast")
    ),
    size_tier = if_else(
      county_name %in% c("Hennepin", "Ramsey", "Dakota", "Anoka", "Washington"),
      "Large counties", "Small counties"
    ),
    size_tier = factor(size_tier, levels = c("Large counties", "Small counties"))
  )

p_provenance <- ggplot(
  county_year_provenance,
  aes(x = emissions_year, y = total_mwh / 1e6, fill = source_label)
) +
  geom_col(position = "stack", width = 0.8) +
  facet_grid(size_tier ~ county_name, scales = "free_y") +
  scale_x_continuous(breaks = seq(2005, latest_year, by = 3)) +
  scale_y_continuous(labels = function(x) paste0(x, "M")) +
  scale_fill_manual(values = c(
    "7610 observed"     = "steelblue",
    "EIA 861 gap-fill"  = "coral",
    "EIA 861 backcast"  = "goldenrod"
  )) +
  labs(
    title = "County electricity activity by data provenance",
    subtitle = "Blue = 7610 observed | Coral = EIA 861 gap-fill | Gold = EIA 861 backcast",
    x = NULL, y = "MWh delivered", fill = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
  )

print(p_provenance)

# --- Plot 2: County totals trend — does the backcast look reasonable? --------
# Compare the full time series (with backcast) against a smooth trend.
# Major discontinuities at the 7610/backcast boundary = suspicious.

county_year_total <- utility_elec_activity %>%
  group_by(emissions_year, county_name) %>%
  summarise(total_mwh = sum(value_activity, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    size_tier = if_else(
      county_name %in% c("Hennepin", "Ramsey", "Dakota", "Anoka", "Washington"),
      "Large counties", "Small counties"
    ),
    size_tier = factor(size_tier, levels = c("Large counties", "Small counties"))
  )

# Mark the earliest 7610 year as a reference line
earliest_7610 <- min(elec_7610$emissions_year)

p_trend <- ggplot(county_year_total, aes(x = emissions_year, y = total_mwh / 1e6)) +
  geom_vline(
    xintercept = earliest_7610 - 0.5, linetype = "dashed",
    color = "gray50", linewidth = 0.5
  ) +
  geom_line(aes(color = county_name), linewidth = 0.8) +
  geom_point(aes(color = county_name), size = 1.5) +
  annotate("text",
    x = earliest_7610 - 0.5, y = Inf,
    label = "← backcast | 7610 →", hjust = 0.5, vjust = 2,
    size = 3, color = "gray40"
  ) +
  facet_wrap(~size_tier, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = seq(2005, latest_year, by = 2)) +
  scale_y_continuous(labels = function(x) paste0(x, "M")) +
  labs(
    title = "County electricity totals: full time series with backcast",
    subtitle = "Dashed line = boundary between EIA 861 backcast and 7610 observed",
    x = NULL, y = "MWh delivered", color = "County"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_trend)

# --- Plot 3: EIA 861 vs 7610 comparison for validation ----------------------
# In years where we have both EIA 861 and 7610 data, compare utility-level
# totals. Large discrepancies signal name-matching or scope issues.

eia_vs_7610 <- elec_7610 %>%
  group_by(utility_name, emissions_year) %>%
  summarise(mwh_7610 = sum(value_activity), .groups = "drop") %>%
  inner_join(
    eia_861 %>% select(utility_name, emissions_year, mwh_eia = total),
    by = c("utility_name", "emissions_year")
  ) %>%
  mutate(
    pct_diff = (mwh_7610 - mwh_eia) / mwh_eia * 100,
    label = if_else(abs(pct_diff) > 15,
      sprintf("%s %d", utility_name, emissions_year), NA_character_
    )
  )

p_eia_check <- ggplot(eia_vs_7610, aes(x = mwh_eia / 1e6, y = mwh_7610 / 1e6)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(aes(color = utility_name), alpha = 0.7, size = 2) +
  geom_text(aes(label = label),
    size = 2.5, hjust = -0.1, vjust = 1.5,
    na.rm = TRUE
  ) +
  labs(
    title = "EIA 861 vs 7610: utility-level MWh comparison",
    subtitle = "Points on the dashed line = perfect agreement. Labels = >15% discrepancy.",
    x = "EIA 861 MN total (millions MWh)",
    y = "7610 scope-county total (millions MWh)",
    color = "Utility"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

print(p_eia_check)

# ═════════════════════════════════════════════════════════════════════════════
# PART 7: Save
# ═════════════════════════════════════════════════════════════════════════════

write_rds(utility_elec_activity, here("_energy", "data", "utility_county_elec_activity.RDS"))

county_elec_activity <- utility_elec_activity %>%
  group_by(county_name, emissions_year, unit_activity) %>%
  summarize(value_activity = sum(value_activity), .groups = "drop")

write_rds(
  county_elec_activity,
  here("_energy", "data", "county_elec_activity.rds")
)

message(sprintf(
  "\nSaved county_elec_activity.rds: %d rows, %d utilities, %d counties, years %d-%d",
  nrow(utility_elec_activity),
  n_distinct(utility_elec_activity$utility_name),
  n_distinct(utility_elec_activity$county_name),
  min(utility_elec_activity$emissions_year),
  max(utility_elec_activity$emissions_year)
))

message("\nProvenance breakdown:")
utility_elec_activity %>%
  count(data_source) %>%
  print()
