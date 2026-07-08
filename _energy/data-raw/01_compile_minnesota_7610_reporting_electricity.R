# 01_compile_minnesota_7610_reporting_elec.R
# ──────────────────────────────────────────────────────────────────────────────
# Ingest MN 7610 electric utility annual reports (ElectricityByCounty sheet)
# and produce a clean county-level activity table for in-scope counties.
#
# Inputs:  Excel workbooks downloaded from MN Commerce, organized as:
#          {dir_mn_electricity_state}/{utility_folder}/{year_folder}/*.xlsx
#
# Outputs: county_elec_7610.rds
#          A tibble with columns: emissions_year, county_name, county_code,
#          utility_name, entity_id, value_activity, unit_activity, data_source
#
# Notes:
#   - Utility name and year are read from the Registration sheet (C9, C6),
#     NOT inferred from folder names.
#   - County data is parsed by anchoring on the header row ("COUNTY"/"MWH"),
#     not hardcoded cell ranges.
#   - Filters to 9-county scope: 7 metro + Sherburne + Chisago.
#   - GRE/Connexus Anoka dedup handled explicitly.
#   - Emissions calculations are NOT performed here (see 05_* scripts).
# ──────────────────────────────────────────────────────────────────────────────

source("R/_load_pkgs.R")

scope_counties <- c(
  "Anoka", "Carver", "Dakota", "Hennepin", "Ramsey",
  "Scott", "Washington", "Sherburne", "Chisago"
)

# root directory: {utility}/{year}/*.xlsx
dir_mn_electricity_state <- here(
  "_energy", "data-raw", "mn_elec_utility_reporting_state"
)

# name harmonization 
# Utility names vary across filing years . This lookup maps all observed variants
# to a single canonical name. Add new entries as they surface.

utility_name_lookup <- c(
  # Xcel filed as Northern States Power through 2013
  "Northern States Power Company" = "Xcel Energy",
  "Northern States Power"         = "Xcel Energy",
  
  # Chaska capitalization inconsistency in 2020 filing
  "CITY OF CHASKA"                = "City of Chaska",
  
  # North Branch has appeared under three names
  "North Branch Municipal Water & Light" = "City of North Branch",
  "North Branch Water & Light"           = "City of North Branch"
)

harmonize_utility_names <- function(names) {
  idx <- match(names, names(utility_name_lookup))
  ifelse(is.na(idx), names, utility_name_lookup[idx])
}

# --- known data gaps ----------------------------------------------------------
# Document utility-years we know are missing from the 7610 filings, with the
# reason. This gets joined to the output so downstream scripts can distinguish
# "no data filed" from "zero deliveries" from "not yet downloaded."
#

known_gaps <- tribble(
  ~utility_name,                  ~emissions_year, ~gap_type,     ~notes,
  # GRE did not file 7610s before 2016 or for 2019-2020
  
  "Great River Energy",           2013L,           "not_filed",   "No 7610 filing found on MN Commerce eFiling",
  "Great River Energy",           2014L,           "not_filed",   "No 7610 filing found on MN Commerce eFiling",
  "Great River Energy",           2015L,           "not_filed",   "No 7610 filing found on MN Commerce eFiling",
  "Great River Energy",           2019L,           "not_filed",   "No 7610 filing found on MN Commerce eFiling",
  "Great River Energy",           2020L,           "not_filed",   "No 7610 filing found on MN Commerce eFiling",
  # Elk River Municipal — sporadic 7610 filing history
  "Elk River Municipal Utilities", 2013L,          "not_filed",   "No 7610 filing found on MN Commerce eFiling",
  "Elk River Municipal Utilities", 2014L,          "not_filed",   "No 7610 filing found on MN Commerce eFiling",
  "Elk River Municipal Utilities", 2020L,          "not_filed",   "No 7610 filing found on MN Commerce eFiling",
  "Elk River Municipal Utilities", 2022L,          "not_filed",   "No 7610 filing found on MN Commerce eFiling",
  "Elk River Municipal Utilities", 2023L,          "not_filed",   "No 7610 filing found on MN Commerce eFiling",
  # Shakopee Public Utilities — missing early years
  "Shakopee Public Utilities",     2013L,          "not_filed",   "No 7610 filing found on MN Commerce eFiling",
  "Shakopee Public Utilities",     2014L,          "not_filed",   "No 7610 filing found on MN Commerce eFiling",
  # Princeton Public Utilities — missing early years
  "Princeton Public Utilities",    2013L,          "not_filed",   "No 7610 filing found on MN Commerce eFiling",
  "Princeton Public Utilities",    2014L,          "not_filed",   "No 7610 filing found on MN Commerce eFiling",
  "Princeton Public Utilities",    2015L,          "not_filed",   "No 7610 filing found on MN Commerce eFiling",
  # City of North Branch — only sporadic filings
  "City of North Branch",          2014L,          "not_filed",   "No 7610 filing found on MN Commerce eFiling",
  "City of North Branch",          2015L,          "not_filed",   "No 7610 filing found on MN Commerce eFiling",
  "City of North Branch",          2016L,          "not_filed",   "No 7610 filing found on MN Commerce eFiling",
  "City of North Branch",          2017L,          "not_filed",   "No 7610 filing found on MN Commerce eFiling",
  "City of North Branch",          2018L,          "not_filed",   "No 7610 filing found on MN Commerce eFiling",
  "City of North Branch",          2020L,          "not_filed",   "No 7610 filing found on MN Commerce eFiling"
)


# --- file discovery -----------------------------------------------------------
# Walk the directory tree and collect all xlsx paths with their parent folders.
# We read metadata from the file itself, but the folder structure is used only
# for discovery.

discover_files <- function(root_dir) {
  utility_folders <- list.dirs(root_dir, recursive = FALSE)
  
  file_list <- list()
  
  for (utility_folder in utility_folders) {
    year_folders <- list.dirs(utility_folder, recursive = FALSE)
    
    for (year_folder in year_folders) {
      xlsx_files <- list.files(year_folder, pattern = "\\.xlsx$", full.names = TRUE)
      
      for (f in xlsx_files) {
        file_list <- append(file_list, list(list(
          file_path = f,
          folder_utility = basename(utility_folder),
          folder_year = basename(year_folder)
        )))
      }
    }
  }
  
  file_list
}

# --- metadata extraction -----------------------------------------------------
# Read utility name, report year, and entity ID from the Registration sheet
# rather than trusting folder names.

read_registration <- function(file_path) {
  reg <- tryCatch(
    read_excel(file_path, sheet = "Registration", range = "A5:C9", col_names = FALSE),
    error = function(e) NULL
  )
  
  if (is.null(reg)) {
    warning("No Registration sheet found in: ", file_path)
    return(list(entity_id = NA, emissions_year = NA, utility_name = NA))
  }
  
  # C5 = entity ID, C6 = report year, C9 = utility name
  # The range A5:C9 gives us rows 5-9, so indices are:
  # row 1 (A5) -> entity_id in col 3
  # row 2 (A6) -> report year in col 3
  # row 5 (A9) -> utility name in col 3
  list(
    entity_id   = as.integer(reg[[3]][1]),
    emissions_year = as.integer(reg[[3]][2]),
    utility_name   = as.character(reg[[3]][5])
  )
}

# --- county data parsing -----------------------------------------------------
# Anchor-based: find the header row, parse both panels, validate against
# the grand total embedded in the form.

parse_county_data <- function(file_path) {
  # Read entire sheet as text to find anchors
  raw <- tryCatch(
    read_excel(file_path, sheet = "ElectricityByCounty", col_names = FALSE),
    error = function(e) NULL
  )
  
  if (is.null(raw)) {
    warning("No ElectricityByCounty sheet in: ", file_path)
    return(tibble())
  }
  
  # Find header row: column A contains "COUNTY" and column C contains "MWH"
  header_row <- which(
    toupper(as.character(raw[[1]])) == "COUNTY" &
      grepl("MWH", toupper(as.character(raw[[3]])))
  )
  
  if (length(header_row) == 0) {
    warning("Could not find header anchor in: ", file_path)
    return(tibble())
  }
  
  header_row <- header_row[1]
  # Data starts 2 rows below the header (header = "COUNTY", subheader = "CODE",
  # first data row follows)
  data_start <- header_row + 2
  
  # --- Left panel: columns A (code), B (name), C (MWh) ---
  left_panel <- tibble(
    county_code    = as.integer(raw[[1]][data_start:nrow(raw)]),
    county_name    = as.character(raw[[2]][data_start:nrow(raw)]),
    value_activity = as.numeric(raw[[3]][data_start:nrow(raw)])
  ) %>%
    # Stop at first NA county_code (end of the 1-45 block)
    filter(!is.na(county_code))
  
  # --- Right panel: columns E (code), F (name), G (MWh) ---
  right_panel <- tibble(
    county_code    = as.integer(raw[[5]][data_start:nrow(raw)]),
    county_name    = as.character(raw[[6]][data_start:nrow(raw)]),
    value_activity = as.numeric(raw[[7]][data_start:nrow(raw)])
  ) %>%
    # Stop at first NA county_code -- this filters out WI rows like
    # "Burnett, Douglass, Washburn, WI" (no integer code) and GRAND TOTAL
    filter(!is.na(county_code))
  
  combined <- bind_rows(left_panel, right_panel)
  
  # Treat NA mwh as zero (utility doesn't operate in that county)
  combined <- combined %>%
    mutate(value_activity = replace_na(value_activity, 0))
  
  # --- Grand total validation ---
  # Look for "GRAND TOTAL" in column F to grab the entered total
  grand_total_row <- which(
    grepl("GRAND TOTAL.*Entered", as.character(raw[[6]]), ignore.case = TRUE)
  )
  
  if (length(grand_total_row) > 0) {
    entered_total <- as.numeric(raw[[7]][grand_total_row[1]])
    parsed_total  <- sum(combined$value_activity, na.rm = TRUE)
    
    # Only compare MN counties (the entered total may include WI)
    # So we check against all parsed rows (which already exclude WI via
    # the county_code filter). If the utility has WI operations, there
    # may be a legitimate gap -- flag but don't fail.
    if (!is.na(entered_total) && abs(parsed_total - entered_total) > 1) {
      message(
        sprintf(
          "  QA note: parsed MN total (%.1f) vs entered total (%.1f) differ by %.1f MWh in %s",
          parsed_total, entered_total, entered_total - parsed_total, basename(file_path)
        )
      )
    }
  }
  
  combined
}

# --- main processing loop ----------------------------------------------------

file_list <- discover_files(dir_mn_electricity_state)
message(sprintf("Discovered %d files across utility/year folders", length(file_list)))

all_records <- list()

for (i in seq_along(file_list)) {
  fi <- file_list[[i]]
  
  # Read metadata from the workbook itself
  meta <- read_registration(fi$file_path)
  
  if (is.na(meta$emissions_year) || is.na(meta$utility_name)) {
    warning(
      "Skipping file (missing Registration metadata): ", fi$file_path
    )
    next
  }
  
  # Parse county data
  county_data <- parse_county_data(fi$file_path)
  
  if (nrow(county_data) == 0) next
  
  # Attach metadata and harmonize utility name
  county_data <- county_data %>%
    mutate(
      emissions_year = meta$emissions_year,
      utility_name   = harmonize_utility_names(meta$utility_name),
      entity_id      = meta$entity_id,
      unit_activity  = "mwh",
      data_source    = "mn_7610"
    )
  
  all_records[[i]] <- county_data
}

elec_7610_raw <- bind_rows(all_records)

message(sprintf(
  "Parsed %d rows: %d utilities, years %d-%d",
  nrow(elec_7610_raw),
  n_distinct(elec_7610_raw$utility_name),
  min(elec_7610_raw$emissions_year),
  max(elec_7610_raw$emissions_year)
))

# --- filter to scope counties ------------------------------------------------

elec_7610 <- elec_7610_raw %>%
  filter(county_name %in% scope_counties)

# --- GRE / Connexus dedup in Anoka -------------------------------------------
# GRE and Connexus split in 2022. Prior to that, GRE's Anoka values are a
# double-count of Connexus deliveries. Remove GRE Anoka entirely.

gre_connexus_anoka <- elec_7610 %>%
  filter(
    county_name == "Anoka",
    utility_name %in% c("Connexus Energy", "Great River Energy")
  ) %>%
  select(emissions_year, utility_name, value_activity) %>%
  arrange(utility_name, emissions_year)

if (nrow(gre_connexus_anoka) > 0) {
  message("\nGRE/Connexus Anoka overlap (removing GRE):")
  print(gre_connexus_anoka, n = 40)
}

elec_7610 <- elec_7610 %>%
  filter(!(utility_name == "Great River Energy" & county_name == "Anoka"))

# --- manual gap-fills ---------------------------------------------------------
# Elk River Municipal Utilities (Sherburne County) — not in 7610 system for
# all years. 2021 value sourced from their published annual financial report.
# Source: pg 54, https://www.ermumn.com/application/files/3316/5668/9846/
#         2021_Annual_Financial_Report.pdf

elk_river_fills <- tribble(
  ~county_code, ~county_name, ~value_activity, ~utility_name, ~emissions_year, ~entity_id, ~data_source,
  71L, "Sherburne", 341047.71, "Elk River Municipal Utilities", 2021L, NA_integer_, "manual_annual_report"
)
elk_river_fills$unit_activity <- "mwh"

elec_7610 <- bind_rows(elec_7610, elk_river_fills)

# --- drop zero-activity rows -------------------------------------------------
# Counties where a utility reports 0 MWh are legitimate (no service in that
# county) but add noise. Keep only rows with actual deliveries.

elec_7610 <- elec_7610 %>%
  filter(value_activity > 0)

# --- check for duplicates ----------------------------------------------------

dupes <- elec_7610 %>%
  group_by(emissions_year, county_name, utility_name) %>%
  filter(n() > 1)

if (nrow(dupes) > 0) {
  warning("Duplicate county-utility-year rows detected:")
  print(dupes)
}

# --- coverage diagnostics ----------------------------------------------------

message("\n=== Coverage Summary ===")

# Utility-year coverage
utility_years <- elec_7610 %>%
  group_by(utility_name) %>%
  summarise(
    n_years   = n_distinct(emissions_year),
    min_year  = min(emissions_year),
    max_year  = max(emissions_year),
    n_counties = n_distinct(county_name),
    total_mwh  = sum(value_activity),
    .groups = "drop"
  ) %>%
  arrange(desc(total_mwh))

message("\nUtilities in scope (ordered by total MWh):")
print(utility_years, n = 50)

# County-year completeness
county_year_totals <- elec_7610 %>%
  group_by(emissions_year, county_name) %>%
  summarise(
    total_mwh   = sum(value_activity),
    n_utilities = n_distinct(utility_name),
    .groups = "drop"
  )

message("\nCounty-year totals:")
county_year_wide <- county_year_totals %>%
  select(emissions_year, county_name, total_mwh) %>%
  pivot_wider(names_from = emissions_year, values_from = total_mwh)
print(county_year_wide)

# Year-over-year % change by county (flag swings > 20%)
yoy_check <- county_year_totals %>%
  arrange(county_name, emissions_year) %>%
  group_by(county_name) %>%
  mutate(
    pct_change = (total_mwh / lag(total_mwh) - 1) * 100
  ) %>%
  filter(abs(pct_change) > 20) %>%
  ungroup()

if (nrow(yoy_check) > 0) {
  message("\nYear-over-year swings > 20%:")
  print(yoy_check)
}

# --- known gaps: cross-reference against observed data ------------------------
# Check which known gaps are still unresolved (i.e., the data didn't show up
# from another source or manual fill).

year_range <- min(elec_7610$emissions_year):max(elec_7610$emissions_year)

# Filter known_gaps to the year range we actually cover
active_gaps <- known_gaps %>%
  filter(emissions_year %in% year_range)

# Check if any known gaps were actually resolved (data appeared)
resolved <- active_gaps %>%
  semi_join(elec_7610, by = c("utility_name", "emissions_year"))

unresolved <- active_gaps %>%
  anti_join(elec_7610, by = c("utility_name", "emissions_year"))

if (nrow(resolved) > 0) {
  message("\nPreviously-known gaps now resolved (data found):")
  print(resolved)
}

if (nrow(unresolved) > 0) {
  message(sprintf("\n%d known data gaps remain unresolved:", nrow(unresolved)))
  print(unresolved)
  
  # Estimate magnitude of missing data using adjacent years
  gap_magnitude <- unresolved %>%
    filter(gap_type == "not_filed") %>%
    left_join(
      elec_7610 %>%
        group_by(utility_name, county_name) %>%
        summarise(
          avg_mwh = mean(value_activity),
          .groups = "drop"
        ),
      by = "utility_name",
      relationship = "many-to-many"
    ) %>%
    group_by(utility_name, emissions_year) %>%
    summarise(
      est_missing_mwh = sum(avg_mwh, na.rm = TRUE),
      affected_counties = paste(county_name, collapse = ", "),
      .groups = "drop"
    )
  
  if (nrow(gap_magnitude) > 0) {
    message("\nEstimated missing load from unfiled utilities (based on avg of filed years):")
    print(gap_magnitude)
  }
}

# --- diagnostic plots ---------------------------------------------------------

# GRE gap years to shade on plots
gre_gap_years <- known_gaps %>%
  filter(utility_name == "Great River Energy") %>%
  pull(emissions_year)

# Counties affected by GRE gaps (those where GRE reports in non-gap years)
gre_counties <- elec_7610 %>%
  filter(utility_name == "Great River Energy") %>%
  distinct(county_name) %>%
  pull()

# Build county-year totals split by GRE vs non-GRE
county_year_by_gre <- elec_7610 %>%
  mutate(
    source_group = if_else(utility_name == "Great River Energy", "GRE", "Other utilities")
  ) %>%
  group_by(emissions_year, county_name, source_group) %>%
  summarise(mwh = sum(value_activity), .groups = "drop")

# Also compute full county totals for the line
county_year_total <- elec_7610 %>%
  group_by(emissions_year, county_name) %>%
  summarise(total_mwh = sum(value_activity), .groups = "drop") %>%
  mutate(gre_affected = county_name %in% gre_counties)

# Shade rectangles for GRE gap years
gap_shading <- tibble(
  xmin = gre_gap_years - 0.4,
  xmax = gre_gap_years + 0.4
)

# Plot 1: County totals over time, faceted by size tier
# Large counties on top, small counties on bottom, GRE gap years shaded
county_year_total <- county_year_total %>%
  mutate(
    size_tier = if_else(
      county_name %in% c("Hennepin", "Ramsey", "Dakota", "Anoka", "Washington"),
      "Large counties", "Small counties"
    ),
    size_tier = factor(size_tier, levels = c("Large counties", "Small counties"))
  )

p_totals <- ggplot(county_year_total, aes(x = emissions_year, y = total_mwh / 1e6)) +
  geom_rect(
    data = gap_shading,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "steelblue", alpha = 0.08
  ) +
  geom_line(aes(color = county_name, linetype = gre_affected), linewidth = 0.8) +
  geom_point(aes(color = county_name, shape = gre_affected), size = 1.5) +
  facet_wrap(~size_tier, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = year_range) +
  scale_y_continuous(labels = function(x) paste0(x, "M")) +
  scale_linetype_manual(
    values = c("FALSE" = "solid", "TRUE" = "dashed"),
    labels = c("FALSE" = "No GRE exposure", "TRUE" = "GRE-affected"),
    name = NULL
  ) +
  scale_shape_manual(
    values = c("FALSE" = 16, "TRUE" = 17),
    labels = c("FALSE" = "No GRE exposure", "TRUE" = "GRE-affected"),
    name = NULL
  ) +
  labs(
    title = "County electricity totals (7610 filings)",
    subtitle = "Shaded bands = GRE gap years (not filed). Dashed lines = counties with GRE exposure.",
    x = NULL, y = "MWh delivered",
    color = "County"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_totals)

# Plot 2: Stacked area for GRE-affected counties showing GRE vs other
p_stacked <- ggplot(
  county_year_by_gre %>% filter(county_name %in% gre_counties),
  aes(x = emissions_year, y = mwh / 1e6, fill = source_group)
) +
  geom_area(alpha = 0.7, position = "stack") +
  geom_rect(
    data = gap_shading,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "red", alpha = 0.06
  ) +
  facet_wrap(~county_name, ncol = 2, scales = "free_y") +
  scale_x_continuous(breaks = year_range) +
  scale_y_continuous(labels = function(x) paste0(x, "M")) +
  scale_fill_manual(
    values = c("GRE" = "steelblue", "Other utilities" = "gray70"),
    name = NULL
  ) +
  labs(
    title = "GRE contribution to county electricity totals",
    subtitle = "Shaded bands = years GRE did not file a 7610. GRE load disappears entirely.",
    x = NULL, y = "MWh delivered"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_stacked)



# --- save outputs -------------------------------------------------------------

write_rds(
  elec_7610,
  here("_energy", "data", "county_elec_7610.rds")
)

write_rds(
  unresolved,
  here("_energy", "data", "county_elec_7610_known_gaps.rds")
)

message(sprintf(
  "\nSaved county_elec_7610.rds: %d rows, %d utilities, %d counties, years %d-%d",
  nrow(elec_7610),
  n_distinct(elec_7610$utility_name),
  n_distinct(elec_7610$county_name),
  min(elec_7610$emissions_year),
  max(elec_7610$emissions_year)
))

message(sprintf(
  "Saved county_elec_7610_known_gaps.rds: %d unresolved gaps",
  nrow(unresolved)
))
