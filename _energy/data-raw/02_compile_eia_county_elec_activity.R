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
  "Great River Energy"               = "Great River Energy",
  "Northern States Power Co"         = "Xcel Energy",
  "Northern States Power Company"    = "Xcel Energy",
  "Xcel Energy Inc"                  = "Xcel Energy",
  "Xcel Energy"                      = "Xcel Energy",
  "Connexus Energy"                  = "Connexus Energy",
  "Dakota Electric Assn"             = "Great River Energy",
  "Dakota Elec Assn"                 = "Great River Energy",
  "Wright-Hennepin Coop Elec Assn"   = "Great River Energy",
  "Wright-Hennepin Cooperative"      = "Great River Energy",
  "Minnesota Valley Elec Coop"       = "Great River Energy",
  "Minnesota Valley Electric Coop"   = "Great River Energy",
  "McLeod Cooperative Power Assn"    = "Great River Energy",
  "McLeod Coop Power Assn"          = "Great River Energy",
  "McLeod Cooperative Power"         = "Great River Energy",
  "East Central Energy"              = "Great River Energy",
  "East Central Elec Assn"           = "Great River Energy",
  "Goodhue County Coop Elec Assn"    = "Great River Energy",
  "Goodhue County Coop Electric"     = "Great River Energy",
  "Goodhue County Cooperative"       = "Great River Energy",
  "City of Shakopee"                 = "Shakopee Public Utilities",
  "Shakopee Public Utilities Comm"   = "Shakopee Public Utilities",
  "Elk River Muni Utilities"         = "Elk River Municipal Utilities",
  "Elk River Municipal Utilities"    = "Elk River Municipal Utilities",
  "City of Chaska"                   = "City of Chaska",
  "Princeton Public Utils Comm"      = "Princeton Public Utilities",
  "Princeton Public Utilities"       = "Princeton Public Utilities",
  "City of North Branch"             = "City of North Branch",
  "North Branch Water & Light"       = "City of North Branch"
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
                     value = TRUE, ignore.case = TRUE)
  
  if (length(sales_file) == 0) {
    warning(sprintf("No Sales_Ult_Cust file found in %s. Contents: %s",
                    basename(zip_file),
                    paste(zip_contents, collapse = ", ")))
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
        warning(sprintf("Failed to read %s for year %d: %s",
                        sales_file, year, e$message))
        return(NULL)
      }
    )
  } else {
    raw <- tryCatch(
      read_csv(extracted_path, show_col_types = FALSE, guess_max = 10000),
      error = function(e) NULL
    )
  }
  
  if (is.null(raw) || nrow(raw) == 0) return(tibble())
  
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
      if (length(matches) > 0) return(matches[1])
    }
    NA_character_
  }
  
  name_col  <- find_col(c("^utility_name$", "^utility_n", "utility.*name",
                          "^utility$"))
  state_col <- find_col(c("^state$", "^_tate$", "tate$", "^st$"))
  
  # Sector MWh columns: the header is a multi-line cell that collapses into
  # positional names like "megawatthour_10", "megawatthour_13", etc.
  # They always appear in order: residential, commercial, industrial,
  # transportation, total. Find all megawatthour columns and assign by position.
  mwh_cols <- grep("megawatthour", col_names, value = TRUE, ignore.case = TRUE)
  
  # Also check for cleanly-named columns (some years may have proper names)
  res_col <- find_col(c("^residential$", "res.*sales", "res.*megawatt", "res.*mwh"))
  com_col <- find_col(c("^commercial$", "comm.*sales", "com.*sales",
                        "comm.*megawatt", "com.*mwh"))
  ind_col <- find_col(c("^industrial$", "ind.*sales", "ind.*megawatt", "ind.*mwh"))
  tot_col <- find_col(c("^total$", "total.*sales", "total.*megawatt"))
  
  # If named sector columns weren't found, fall back to positional megawatthour
  if (is.na(res_col) && length(mwh_cols) >= 5) {
    res_col <- mwh_cols[1]  # 1st megawatthour = residential
    com_col <- mwh_cols[2]  # 2nd = commercial
    ind_col <- mwh_cols[3]  # 3rd = industrial
    # mwh_cols[4] = transportation (skip)
    tot_col <- mwh_cols[5]  # 5th = total
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
  
  if (length(mn_rows) == 0) return(tibble())
  
  raw_mn <- raw[mn_rows, ]
  
  result <- tibble(
    utility_name_raw = as.character(raw_mn[[name_col]]),
    state            = "MN",
    emissions_year   = as.integer(year)
  )
  
  # Safely add sector columns, coercing "." to NA
  safe_numeric <- function(x) suppressWarnings(as.numeric(as.character(x)))
  
  if (!is.na(res_col)) result$residential <- safe_numeric(raw_mn[[res_col]])
  if (!is.na(com_col)) result$commercial  <- safe_numeric(raw_mn[[com_col]])
  if (!is.na(ind_col)) result$industrial  <- safe_numeric(raw_mn[[ind_col]])
  if (!is.na(tot_col)) result$total       <- safe_numeric(raw_mn[[tot_col]])
  
  # Compute total if not present but sectors are
  if (!"total" %in% names(result) &&
      all(c("residential", "commercial", "industrial") %in% names(result))) {
    result$total <- rowSums(
      result[, c("residential", "commercial", "industrial")], na.rm = TRUE
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
  if (!file.exists(zip_file)) return(tibble())
  parse_eia_861_sales(zip_file, yr)
})

# Aggregate: some utilities (e.g. Dakota Electric, Wright-Hennepin, MN Valley)
# get mapped to "Great River Energy" via harmonization. Sum them up.
eia_861 <- eia_861_raw %>%
  group_by(utility_name, emissions_year) %>%
  summarise(
    across(any_of(c("residential", "commercial", "industrial", "total")),
           ~sum(.x, na.rm = TRUE)),
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
# PART 2: Compute county allocation proportions from 7610 data
# ═════════════════════════════════════════════════════════════════════════════
# For each utility, compute each county's share of the utility's total MN
# deliveries in years where 7610 data exists. These proportions are used to
# allocate EIA 861 state totals down to counties.

county_proportions <- elec_7610 %>%
  group_by(utility_name, emissions_year) %>%
  mutate(
    utility_year_total = sum(value_activity, na.rm = TRUE),
    county_proportion  = if_else(
      utility_year_total > 0,
      value_activity / utility_year_total,
      0
    )
  ) %>%
  ungroup() %>%
  select(utility_name, emissions_year, county_name, county_code,
         county_proportion)

# For backcasting, compute a stable average proportion using the earliest
# N years of available data per utility. For gap-filling within the 7610
# range, use the nearest available year's proportions.

earliest_proportions <- county_proportions %>%
  group_by(utility_name) %>%
  filter(emissions_year <= min(emissions_year) + 2) %>%
  group_by(utility_name, county_name, county_code) %>%
  summarise(
    avg_proportion = mean(county_proportion, na.rm = TRUE),
    n_years_used   = n(),
    .groups = "drop"
  )

# ═════════════════════════════════════════════════════════════════════════════
# PART 3: Gap-fill missing 7610 years within the 7610 filing range
# ═════════════════════════════════════════════════════════════════════════════

# Identify which utility-years need gap-filling
observed_utility_years <- elec_7610 %>%
  distinct(utility_name, emissions_year)

# For each utility, determine the full year range it *should* cover
# (from its first 7610 filing to the latest year)
utility_ranges <- elec_7610 %>%
  group_by(utility_name) %>%
  summarise(
    first_year = min(emissions_year),
    last_year  = max(emissions_year),
    .groups = "drop"
  )

# Build the complete grid of utility-years that should exist
expected_utility_years <- utility_ranges %>%
  rowwise() %>%
  mutate(emissions_year = list(first_year:last_year)) %>%
  unnest(emissions_year) %>%
  select(utility_name, emissions_year)

# Find the gaps
gaps_to_fill <- expected_utility_years %>%
  anti_join(observed_utility_years, by = c("utility_name", "emissions_year")) %>%
  # Only fill gaps for utilities we have EIA 861 data for
  semi_join(eia_861, by = c("utility_name", "emissions_year"))

message(sprintf("\n%d utility-year gaps to fill within 7610 range", nrow(gaps_to_fill)))

# For each gap, find the nearest year's county proportions and apply to
# the EIA 861 state total
gapfill_records <- list()

for (i in seq_len(nrow(gaps_to_fill))) {
  gap <- gaps_to_fill[i, ]
  
  # Get EIA 861 state total for this utility-year
  eia_total <- eia_861 %>%
    filter(utility_name == gap$utility_name,
           emissions_year == gap$emissions_year) %>%
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
    filter(utility_name == gap$utility_name,
           emissions_year == nearest_year)
  
  # Allocate the EIA 861 total to counties
  filled <- props %>%
    mutate(
      emissions_year = gap$emissions_year,
      value_activity = eia_total * county_proportion,
      unit_activity  = "mwh",
      data_source    = "eia_861_gapfill"
    ) %>%
    select(emissions_year, county_name, county_code, utility_name,
           value_activity, unit_activity, data_source) %>%
    filter(value_activity > 0)
  
  gapfill_records[[i]] <- filled
}

gapfilled <- bind_rows(gapfill_records)
message(sprintf("Gap-filled %d county-utility-year rows", nrow(gapfilled)))

# ═════════════════════════════════════════════════════════════════════════════
# PART 4: Backcast to 2005 for all utilities
# ═════════════════════════════════════════════════════════════════════════════
# For years before each utility's first 7610 filing, use EIA 861 state totals
# with the earliest available county proportions (averaged over first 3 years
# of 7610 data).

backcast_years <- backcast_start:(min(elec_7610$emissions_year) - 1L)

# Also backcast for utilities that started filing 7610s after 2013
# (e.g., GRE started 2016, Princeton started 2016, etc.)

backcast_records <- list()
bc_idx <- 0

for (util in unique(earliest_proportions$utility_name)) {
  util_first_7610 <- utility_ranges %>%
    filter(utility_name == util) %>%
    pull(first_year)
  
  # Backcast years: from 2005 to the year before first 7610 filing
  util_backcast_years <- full_year_range[
    full_year_range < util_first_7610
  ]
  
  if (length(util_backcast_years) == 0) next
  
  util_props <- earliest_proportions %>%
    filter(utility_name == util)
  
  for (yr in util_backcast_years) {
    eia_total <- eia_861 %>%
      filter(utility_name == util, emissions_year == yr) %>%
      pull(total)
    
    if (length(eia_total) == 0 || is.na(eia_total)) next
    
    bc_idx <- bc_idx + 1
    backcast_records[[bc_idx]] <- util_props %>%
      mutate(
        emissions_year = yr,
        value_activity = eia_total * avg_proportion,
        unit_activity  = "mwh",
        data_source    = "eia_861_backcast",
        utility_name   = util
      ) %>%
      select(emissions_year, county_name, county_code, utility_name,
             value_activity, unit_activity, data_source) %>%
      filter(value_activity > 0)
  }
}

backcast <- bind_rows(backcast_records)
message(sprintf("Backcast %d county-utility-year rows to %d",
                nrow(backcast), backcast_start))

# ═════════════════════════════════════════════════════════════════════════════
# PART 5: Combine everything
# ═════════════════════════════════════════════════════════════════════════════

# Strip entity_id from 7610 data before binding (not present in gap/backcast)
elec_7610_slim <- elec_7610 %>%
  select(emissions_year, county_name, county_code, utility_name,
         value_activity, unit_activity, data_source)

county_elec_activity <- bind_rows(
  elec_7610_slim,
  gapfilled,
  backcast
) %>%
  arrange(county_name, utility_name, emissions_year)

# --- check for duplicates ----------------------------------------------------

dupes <- county_elec_activity %>%
  group_by(emissions_year, county_name, utility_name) %>%
  filter(n() > 1)

if (nrow(dupes) > 0) {
  warning(sprintf("%d duplicate county-utility-year rows found!", nrow(dupes)))
  print(dupes)
}

# --- summary ------------------------------------------------------------------

provenance_summary <- county_elec_activity %>%
  group_by(data_source) %>%
  summarise(
    n_rows    = n(),
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

county_year_provenance <- county_elec_activity %>%
  mutate(
    source_label = case_when(
      data_source == "mn_7610"              ~ "7610 observed",
      data_source == "manual_annual_report" ~ "7610 observed",
      data_source == "eia_861_gapfill"      ~ "EIA 861 gap-fill",
      data_source == "eia_861_backcast"     ~ "EIA 861 backcast"
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

county_year_total <- county_elec_activity %>%
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
  geom_vline(xintercept = earliest_7610 - 0.5, linetype = "dashed",
             color = "gray50", linewidth = 0.5) +
  geom_line(aes(color = county_name), linewidth = 0.8) +
  geom_point(aes(color = county_name), size = 1.5) +
  annotate("text", x = earliest_7610 - 0.5, y = Inf,
           label = "← backcast | 7610 →", hjust = 0.5, vjust = 2,
           size = 3, color = "gray40") +
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
    label    = if_else(abs(pct_diff) > 15,
                       sprintf("%s %d", utility_name, emissions_year), NA_character_)
  )

p_eia_check <- ggplot(eia_vs_7610, aes(x = mwh_eia / 1e6, y = mwh_7610 / 1e6)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(aes(color = utility_name), alpha = 0.7, size = 2) +
  geom_text(aes(label = label), size = 2.5, hjust = -0.1, vjust = 1.5,
            na.rm = TRUE) +
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

write_rds(
  county_elec_activity,
  here("_energy", "data", "county_elec_activity.rds")
)

message(sprintf(
  "\nSaved county_elec_activity.rds: %d rows, %d utilities, %d counties, years %d-%d",
  nrow(county_elec_activity),
  n_distinct(county_elec_activity$utility_name),
  n_distinct(county_elec_activity$county_name),
  min(county_elec_activity$emissions_year),
  max(county_elec_activity$emissions_year)
))

message("\nProvenance breakdown:")
county_elec_activity %>%
  count(data_source) %>%
  print()
