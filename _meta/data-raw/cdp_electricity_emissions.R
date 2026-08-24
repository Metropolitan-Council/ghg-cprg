#### CDP Reporting: In-boundary emissions from electricity/energy generation
#### Companion script — reads the same GHGRP source data as
#### compile_ghgrp_emissions.R and compile_fuel_combustion.R but produces
#### a CDP-specific summary. Does NOT modify any existing outputs.
####
#### CDP categories:
####   1. Electricity-only generation (Subpart D, no useful thermal output)
####   2. CHP generation (generates electricity AND delivers useful thermal output)
####   3. Heat/cold generation (thermal-only distribution, no electricity)
####   4. Local renewable generation (in-boundary solar/wind; ~0 direct emissions)
####
#### Key design choice: Subpart D facilities have emissions in the
#### `electricity_generation` column. CHP and heat/cold facilities that
#### report under Subpart C only have emissions in `stationary_combustion`
#### (or `total_reported_direct_emissions`). This script handles both.

source("R/_load_pkgs.R")

cprg_county <- readRDS("_meta/data/cprg_county.RDS")

# ---- 1. Read GHGRP FLIGHT data (same source as compile_ghgrp_emissions.R) ----

ghgrp <- lapply(as.character(2011:2023), function(y) {
  read_excel(
    file.path(
      here::here(),
      paste0("_industrial/data-raw/ghgrp/ghgp_data_", y, ".xlsx")
    ),
    sheet = 1,
    skip = 3
  ) %>%
    clean_names() %>%
    filter(state %in% c("MN", "WI")) %>%
    select(-(last_col(offset = 2):last_col())) %>%
    mutate(county_name = str_remove(str_to_title(county), " County")) %>%
    filter(county_name %in% cprg_county$county_name) %>%
    mutate(emissions_year = as.numeric(y))
}) %>%
  bind_rows()

# ---- 2. Facility classification ---------------------------------------------
#
# CDP category assignment for all generation/distribution facilities in the
# seven-county metro. Matched by facility_name (stable across years).
#
# emissions_col: which FLIGHT column holds the relevant emissions.
#   - "electricity_generation" for Subpart D reporters
#   - "stationary_combustion" for Subpart C-only reporters (CHP, heat/cold)
#   - "total_reported_direct_emissions" if stationary_combustion is unreliable
#
# UPDATE THIS TABLE as new facilities appear or configurations change.

cdp_facility_classification <- tribble(
  ~facility_name_pattern, ~cdp_category, ~emissions_col, ~notes,
  
  # --- Electricity-only: Subpart D, no useful thermal output ----
  "Allen S King",
  "electricity_only", "electricity_generation",
  "Coal-fired 511 MW, Xcel. Supplies hot water to Andersen Windows but classified elec-only per Met Council preference. Closing 2028.",
  
  "Black Dog",
  "electricity_only", "electricity_generation",
  "Natural gas CC + peaking, Xcel. Converted from coal. No thermal sales.",
  
  "Blue Lake Generating Plant",
  "electricity_only", "electricity_generation",
  "Natural gas peaking, Xcel.",
  
  "High Bridge",
  "electricity_only", "electricity_generation",
  "Natural gas CC 570 MW, Xcel. Replaced coal plant 2008 via MERP. No thermal sales.",
  
  "Inver Hills Generating Plant",
  "electricity_only", "electricity_generation",
  "Natural gas peaking, Xcel.",
  
  "Minnesota River Station",
  "electricity_only", "electricity_generation",
  "Natural gas peaking, City of Chaska / MMPA.",
  
  "Riverside \\(1927\\)",
  "electricity_only", "electricity_generation",
  "Natural gas CC ~470 MW, Xcel. Repowered from coal 2009. Post-repowering config does not appear to export steam.",
  
  # --- CHP: generates electricity AND delivers useful thermal output ---
  "Cottage Grove Cogeneration",
  "chp", "electricity_generation",
  "265 MW CCGT cogen, topping-cycle. Sells steam to 3M Cottage Grove. Independently owned (Ultra Capital/Panamint).",
  
  "ST PAUL COGENERATION",
  "chp", "stationary_combustion",
  "District Energy St. Paul wood-fired CHP: 25 MW electricity to Xcel + 65 MW thermal. Reports Subpart C only. Large biogenic CO2.",
  
  "UNIVERSITY OF MN - TWIN CITIES",
  "chp", "stationary_combustion",
  "Southeast Steam Plant. CHP: heats 94 buildings, provides electricity for cooling to 19 buildings, plus steam.",
  
  # --- Heat/cold: thermal-only distribution, no electricity generation ---
  "DISTRICT ENERGY ST PAUL INC-HANS O NYMAN",
  "heat_cold", "stationary_combustion",
  "Hans O. Nyman heating plant. Hot water district heating only, no electricity generation.",
  
  "NRG ENERGY CENTER MINNEAPOLIS",
  "heat_cold", "stationary_combustion",
  "Minneapolis district energy (Vicinity Energy). Thermal distribution.",
  
  "ENERGY CENTER MINNEAPOLIS",
  "heat_cold", "stationary_combustion",
  "Minneapolis district energy — alternate name in later reporting years.",
  
  "METROPOLITAN AIRPORTS COMMISSION",
  "heat_cold", "stationary_combustion",
  "MSP Airport central utility plant. Boilers/chillers for terminal complex."
)

# ---- 3. Match classification to GHGRP facility records ----------------------

# Build a lookup of facility_id → cdp_category by matching facility names.
# Using regex patterns to handle minor name variations across years.
facility_lookup <- ghgrp %>%
  distinct(facility_id, facility_name) %>%
  mutate(cdp_category = NA_character_, emissions_col = NA_character_) %>%
  { # nolint
    df <- .
    for (i in seq_len(nrow(cdp_facility_classification))) {
      pattern <- cdp_facility_classification$facility_name_pattern[i]
      matches <- grepl(pattern, df$facility_name)
      df$cdp_category[matches] <- cdp_facility_classification$cdp_category[i]
      df$emissions_col[matches] <- cdp_facility_classification$emissions_col[i]
    }
    df
  } %>%
  filter(!is.na(cdp_category)) %>%
  # A facility_id may have multiple names across years; keep one classification
  
  distinct(facility_id, .keep_all = TRUE)

message("\n=== Classified facilities ===")
facility_lookup %>%
  select(facility_id, facility_name, cdp_category, emissions_col) %>%
  print(n = Inf)

# ---- 4. Extract emissions for each classified facility ----------------------

# For each facility, pull the correct emissions column based on whether
# it reports under Subpart D (electricity_generation) or Subpart C only.

cdp_generation_detail <- ghgrp %>%
  inner_join(
    facility_lookup %>% select(facility_id, cdp_category, emissions_col),
    by = "facility_id"
  ) %>%
  mutate(
    city_name = str_to_sentence(city),
    # Pull the right column per facility
    value_emissions = case_when(
      emissions_col == "electricity_generation" ~
        as.numeric(electricity_generation),
      emissions_col == "stationary_combustion" ~
        as.numeric(stationary_combustion),
      emissions_col == "total_reported_direct_emissions" ~
        as.numeric(total_reported_direct_emissions)
    )
  ) %>%
  filter(!is.na(value_emissions), value_emissions > 0) %>%
  select(
    emissions_year,
    facility_id,
    facility_name,
    city_name,
    county_name,
    state,
    cdp_category,
    value_emissions
  ) %>%
  mutate(
    unit_emissions = "Metric tons CO2e",
    data_source = "EPA GHGRP"
  )

# ---- 5. Summarize for CDP reporting -----------------------------------------

cdp_generation_summary <- cdp_generation_detail %>%
  group_by(emissions_year, cdp_category) %>%
  summarize(
    n_facilities = n_distinct(facility_id),
    value_emissions = sum(value_emissions, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Add local renewable placeholder row (zero direct emissions)
  bind_rows(
    tibble(
      emissions_year = unique(.$emissions_year),
      cdp_category = "local_renewable",
      n_facilities = NA_integer_,
      value_emissions = 0
    )
  ) %>%
  # CDP display labels
  mutate(
    cdp_label = case_when(
      cdp_category == "electricity_only" ~ "Electricity-only generation",
      cdp_category == "chp" ~ "CHP generation",
      cdp_category == "heat_cold" ~ "Heat/cold generation",
      cdp_category == "local_renewable" ~ "Local renewable generation"
    )
  ) %>%
  arrange(emissions_year, cdp_category)

# Total row per year
cdp_generation_total <- cdp_generation_summary %>%
  group_by(emissions_year) %>%
  summarize(
    cdp_category = "total",
    cdp_label = "Total generation of grid-supplied energy",
    n_facilities = sum(n_facilities, na.rm = TRUE),
    value_emissions = sum(value_emissions, na.rm = TRUE),
    .groups = "drop"
  )

cdp_generation_summary <- bind_rows(cdp_generation_summary, cdp_generation_total) %>%
  mutate(unit_emissions = "Metric tons CO2e") %>%
  arrange(emissions_year, cdp_category)

# ---- 6. Print a quick check ------------------------------------------------

message("\n=== CDP Generation Summary (most recent year) ===\n")
cdp_generation_summary %>%
  filter(emissions_year == 2022) %>%
  mutate(value_emissions = scales::comma(round(value_emissions, 0))) %>%
  select(cdp_label, n_facilities, value_emissions) %>%
  print(n = Inf)

message("\n=== Facility detail (most recent year) ===\n")
cdp_generation_detail %>%
  filter(emissions_year == max(emissions_year)) %>%
  arrange(cdp_category, desc(value_emissions)) %>%
  mutate(value_emissions = scales::comma(round(value_emissions, 0))) %>%
  select(facility_name, city_name, county_name, cdp_category, value_emissions) %>%
  print(n = Inf)

# ---- 7. Save outputs -------------------------------------------------------

saveRDS(cdp_generation_detail, "./_industrial/data/cdp_electricity_generation_detail.rds")
saveRDS(cdp_generation_summary, "./_industrial/data/cdp_electricity_generation_summary.rds")

message("\nSaved: cdp_electricity_generation_detail.rds")
message("Saved: cdp_electricity_generation_summary.rds")
