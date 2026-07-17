### compile all ctu electricity emissions

source("R/_load_pkgs.R")

## ctu and county data
cprg_ctu <- read_rds("_meta/data/cprg_ctu.RDS") %>%
  filter(!county_name %in% c("Chisago", "Sherburne", "St. Croix", "Pierce"))
cprg_county <- read_rds("_meta/data/cprg_county.RDS") %>%
  filter(!county_name %in% c("Chisago", "Sherburne", "St. Croix", "Pierce"))
ctu_population <- read_rds("_meta/data/ctu_population.RDS") %>%
  left_join(cprg_county %>% st_drop_geometry() %>% select(geoid, county_name)) %>%
  filter(!county_name %in% c("Chisago", "Sherburne", "St. Croix", "Pierce"))

# assign cities to counties where majority of population is
ctu_county_unique <- ctu_population %>%
  group_by(ctu_name, ctu_class) %>%
  filter(ctu_population == max(ctu_population)) %>%
  ungroup() %>%
  distinct(geoid, ctuid, ctu_name, ctu_class, county_name)

# county activity data
county_mwh <- readRDS("_energy/data/county_elec_activity.rds")

## create storage frame of unique city and utility combos with all years
ctu_utility_year <- readRDS("_energy/data/ctu_elec_utility_intersect.RDS") %>%
  cross_join(data.frame(inventory_year = c(2007:2023))) %>%
  mutate(
    residential_mwh = NA,
    business_mwh = NA,
    total_mwh = NA
  ) %>%
  rename(utility = utility_name)

## load formatted SQL utility data
sql_elec <- readRDS("_energy/data/ctu_electricity_emissions_2015_2018.rds") %>%
  mutate(
    ctu_class = if_else(grepl("Twp.", ctu_name), "TOWNSHIP", "CITY"),
    ctu_name = str_replace_all(ctu_name, " Twp.", ""),
    ctu_name = str_replace_all(ctu_name, "St. ", "Saint "),
    ctu_class = if_else(ctu_name %in% c("Credit River", "Empire"),
      "CITY",
      ctu_class
    ),
    utility = data_source
  ) %>%
  filter(
    units_emissions == "Metric tons CO2",
    !is.na(mwh_per_year)
  ) %>% # removes duplicates
  mutate(sector = if_else(customer_class == "Residential",
    "Residential",
    "Business"
  )) %>%
  group_by(ctu_name, ctu_class, emissions_year, utility, sector) %>%
  summarise(mwh_per_year = sum(mwh_per_year, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = sector, values_from = mwh_per_year,
    names_glue = "{tolower(sector)}_mwh"
  ) %>%
  mutate(total_mwh = rowSums(across(c(business_mwh, residential_mwh)), na.rm = TRUE),
         total_mwh = if_else(is.na(business_mwh) & is.na(residential_mwh), NA_real_, total_mwh))

# correct Elk River data

sql_elec <- sql_elec %>%
  mutate(across(c(residential_mwh, business_mwh, total_mwh),
                ~ if_else(utility == "Elk River Municipal Utilities", . / 1000, .)
  ))

## load and format connexus data
connexus <- readRDS("_energy/data/connexus_electric_activity.RDS") %>%
  filter(!is.na(mwh_delivered)) %>%
  # mutate(mwh_delivered = mwh_delivered * 10e-4) %>%  # kwh listed instead of mwh
  mutate(sector = case_when(
    sector == "Residential" ~ "Residential",
    sector == "Residential/Commercial/Industrial" ~ "Residential/Commercial/Industrial",
    TRUE ~ "Business"
  )) %>%
  group_by(ctu_name, ctu_class, emissions_year, utility, sector) %>%
  summarise(mwh_per_year = sum(mwh_delivered, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = sector, values_from = mwh_per_year,
    names_glue = "{tolower(sector)}_mwh"
  ) %>%
  mutate(
    # preserve combined-category total if no sector breakdown exists
    total_mwh = case_when(
      !is.na(business_mwh) | !is.na(residential_mwh) ~
        rowSums(across(c(business_mwh, residential_mwh)), na.rm = TRUE),
      !is.na(`residential/commercial/industrial_mwh`) ~
        `residential/commercial/industrial_mwh`,
      TRUE ~ NA_real_
    )
  ) %>%
  select(-any_of("residential/commercial/industrial_mwh"))


### load and format Dakota electric data
dakota <- readRDS("_energy/data/dakota_electric_activity.RDS") %>%
  filter(!is.na(mwh_delivered)) %>%
  mutate(sector = case_when(
    sector == "Residential" ~ "Residential",
    TRUE ~ "Business" # includes "Commercial/Industrial", "Commercial", "Dakota Electric Operations", and "Irrigation Services"
  )) %>%
  group_by(ctu_name, ctu_class, emissions_year, utility, sector) %>%
  summarise(mwh_per_year = sum(mwh_delivered, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = sector, values_from = mwh_per_year,
    names_glue = "{tolower(sector)}_mwh"
  ) %>%
  mutate(total_mwh = rowSums(across(c(business_mwh, residential_mwh)), na.rm = TRUE),
         total_mwh = if_else(is.na(business_mwh) & is.na(residential_mwh), NA_real_, total_mwh))

### load and format xcel data
xcel <- readRDS("_energy/data/Xcel_elecNG_activityData_2015_2023.rds") %>%
  filter(
    !is.na(mwh_delivered),
    source == "Electricity"
  ) %>%
  rename(emissions_year = year) %>%
  mutate(sector = case_when(
    sector_mapped == "residential" ~ "Residential",
    TRUE ~ "Business"
  )) %>%
  group_by(ctu_name, ctu_class, emissions_year, utility, sector) %>%
  summarise(mwh_per_year = sum(mwh_delivered, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = sector, values_from = mwh_per_year,
    names_glue = "{tolower(sector)}_mwh"
  ) %>%
  mutate(total_mwh = rowSums(across(c(business_mwh, residential_mwh)), na.rm = TRUE),
         total_mwh = if_else(is.na(business_mwh) & is.na(residential_mwh), NA_real_, total_mwh))


# load in municipal utility data
munis <- readRDS("_energy/data/MNelecMunis_activityData_2014_2023.rds") %>%
  filter(!is.na(mwh_delivered)) %>%
  rename(emissions_year = year) %>%
  mutate(sector = case_when(
    sector_mapped == "residential" ~ "Residential",
    TRUE ~ "Business"
  )) %>%
  group_by(ctu_name, ctu_class, emissions_year, utility, sector) %>%
  summarise(mwh_per_year = sum(mwh_delivered, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = sector, values_from = mwh_per_year,
    names_glue = "{tolower(sector)}_mwh"
  ) %>%
  mutate(total_mwh = rowSums(across(c(business_mwh, residential_mwh)), na.rm = TRUE),
         total_mwh = if_else(is.na(business_mwh) & is.na(residential_mwh), NA_real_, total_mwh))

# function: sequentially load data while keeping NAs
merge_electricity_data <- function(base_df, new_data) {
  base_df %>%
    left_join(new_data %>% rename(inventory_year = emissions_year),
      by = c("ctu_name", "ctu_class", "inventory_year", "utility")
    ) %>%
    mutate(
      residential_mwh = if_else(!is.na(residential_mwh.y), residential_mwh.y, residential_mwh.x),
      business_mwh = if_else(!is.na(business_mwh.y), business_mwh.y, business_mwh.x),
      total_mwh = if_else(!is.na(total_mwh.y), total_mwh.y, total_mwh.x)
    ) %>%
    select(-ends_with(".x"), -ends_with(".y")) # Remove duplicate columns
}

## load each dataset sequentially (deliberately override previous sql data with our data requests)

# make sure names conform
anti_join(sql_elec, ctu_utility_year, by = "utility") %>%
  distinct(utility) %>%
  arrange(utility)
sort(unique(ctu_utility_year$utility))
sql_elec <- sql_elec %>%
  mutate(utility = case_when(
    utility == "City of Chaska" ~ "City of Chaska Electric Department",
    utility == "Wright-Hennepin Coop Electric Assn" ~ "Wright Hennepin Electric Cooperative",
    TRUE ~ utility
  ))

ctu_utility_year <- merge_electricity_data(ctu_utility_year, sql_elec)
ctu_utility_year <- merge_electricity_data(ctu_utility_year, connexus)
ctu_utility_year <- merge_electricity_data(ctu_utility_year, dakota)
ctu_utility_year <- merge_electricity_data(ctu_utility_year, xcel)

anti_join(munis, ctu_utility_year, by = "utility") %>%
  distinct(utility) %>%
  arrange(utility)
sort(unique(ctu_utility_year$utility))

munis <- munis %>%
  mutate(utility = case_when(
    utility == "City of Chaska" ~ "City of Chaska Electric Department",
    utility == "City of Anoka" ~ "Anoka Municipal Utility",
    utility == "City of North St Paul" ~ "City of North Saint Paul Electric Utilities",
    TRUE ~ utility
  ))

ctu_utility_year <- merge_electricity_data(ctu_utility_year, munis)

# ── Dakota Electric SQL-era data quality check ────────────────────────────────
# The SQL server data (2015-2017) for some Dakota Electric cities is drastically
# understated compared to the direct data request (2019+). This appears to be a
# like a problem with SQL server DEA for these cities, not real growth.
# Identify affected cities via YoY check and null out pre-break years so they
# fall through to RF modeling instead of anchoring to bad data.

dea_yoy <- ctu_utility_year %>%
  filter(utility == "Dakota Electric Association", !is.na(total_mwh)) %>%
  arrange(ctu_name, ctu_class, inventory_year) %>%
  group_by(ctu_name, ctu_class) %>%
  mutate(
    prev_mwh   = lag(total_mwh),
    yoy_pct    = (total_mwh - prev_mwh) / prev_mwh * 100,
    break_year = min(inventory_year[yoy_pct > 100], na.rm = TRUE),
    has_break  = any(yoy_pct > 100, na.rm = TRUE)
  ) %>%
  ungroup()

dea_bad_years <- dea_yoy %>%
  filter(has_break, inventory_year < break_year) %>%
  select(ctu_name, ctu_class, utility, inventory_year)

cat("=== Dakota Electric pre-break years nulled out ===\n")
dea_bad_years %>%
  group_by(ctu_name, ctu_class) %>%
  summarize(
    years_dropped = paste(inventory_year, collapse = ", "),
    .groups = "drop"
  ) %>%
  print(n = Inf)

ctu_utility_year <- ctu_utility_year %>%
  left_join(
    dea_bad_years %>% mutate(drop = TRUE),
    by = c("ctu_name", "ctu_class", "utility", "inventory_year")
  ) %>%
  mutate(across(
    c(residential_mwh, business_mwh, total_mwh),
    ~ if_else(!is.na(drop), NA_real_, .)
  )) %>%
  select(-drop)

# ── Interpolate Dakota Electric 2018 gap ──────────────────────────────────────
# Dakota Electric's 2018 filing is faulty (starts March). Interpolate from
# 2017 and 2019 anchors — but only where BOTH anchors survived the quality
# check above. Cities where 2017 was nulled out get no interpolation; their
# 2018 stays NA and falls through to RF modeling.

dea_2018_anchors <- ctu_utility_year %>%
  filter(
    utility == "Dakota Electric Association",
    inventory_year %in% c(2017, 2019),
    !is.na(total_mwh)
  )

dea_2018_interp <- dea_2018_anchors %>%
  group_by(ctu_name, ctu_class, utility) %>%
  filter(n() == 2) %>%
  summarize(
    inventory_year  = 2018,
    residential_mwh = mean(residential_mwh),
    business_mwh    = mean(business_mwh),
    total_mwh       = mean(total_mwh),
    .groups         = "drop"
  )

cat("\n=== Dakota Electric 2018 interpolated (clean anchors only) ===\n")
cat(sprintf("%d cities interpolated\n", nrow(dea_2018_interp)))

# Replace the NA 2018 rows with interpolated values
ctu_utility_year <- ctu_utility_year %>%
  anti_join(dea_2018_interp,
            by = c("ctu_name", "ctu_class", "utility", "inventory_year")
  ) %>%
  bind_rows(dea_2018_interp)


### compare to rii totals and supplement where possible

# get a list of city-year combos we think are complete

ctu_year_complete <- ctu_utility_year %>%
  group_by(ctu_name, ctu_class, inventory_year) %>%
  filter(!any(is.na(total_mwh))) %>%
  summarize(total_mwh = sum(total_mwh)) %>%
  ungroup()

# 136/210 ctus have at least one complete year, 72 for 2021

rii <- read_rds("_energy/data/rii_electricity_2007_2023.rds")

# merge with ctu_year_complete
rii_ctu_comp <- inner_join(ctu_year_complete,
  rii %>%
    group_by(ctu_name, ctu_class, year) %>%
    summarize(rii_mwh = sum(mwh_delivered)) %>%
    ungroup(),
  by = c("ctu_name", "ctu_class",
    "inventory_year" = "year"
  )
)

ggplot(data = rii_ctu_comp, aes(x = total_mwh, y = rii_mwh)) +
  geom_point() +
  geom_abline(slope = 1) +
  theme_bw()
## fairly tight, a few notable departure

rii_ctu_comp %>% filter(
  abs(total_mwh - rii_mwh) > 10000
)


### put RII data in for city-years without any utility data (skip partial sets)

rii_wide <- rii %>%
  mutate(sector_use = if_else(sector == "Residential",
    "Residential",
    "Business"
  )) %>%
  select(-sector) %>%
  pivot_wider(
    names_from = sector_use, values_from = mwh_delivered,
    names_glue = "{tolower(sector_use)}_mwh"
  ) %>%
  mutate(total_mwh = rowSums(across(c(business_mwh, residential_mwh)), na.rm = TRUE),
         total_mwh = if_else(is.na(business_mwh) & is.na(residential_mwh), NA_real_, total_mwh))

# id cities with NO utility data

empty_city_years <- ctu_utility_year %>%
  group_by(ctu_name, ctu_class, inventory_year) %>%
  summarise(all_na = all(is.na(business_mwh) & is.na(residential_mwh) & is.na(total_mwh)), .groups = "drop") %>%
  filter(all_na) %>%
  select(ctu_name, ctu_class, inventory_year)

# pull out rii data matching above

rii_fill <- empty_city_years %>%
  left_join(rii_wide %>% rename(inventory_year = year),
    by = c("ctu_name", "ctu_class", "inventory_year")
  ) %>%
  select(ctu_name, ctu_class, inventory_year, utility, business_mwh, residential_mwh, total_mwh) %>%
  mutate(total_mwh = rowSums(across(c(business_mwh, residential_mwh)), na.rm = TRUE),
         total_mwh = if_else(is.na(business_mwh) & is.na(residential_mwh), NA_real_, total_mwh)) %>%
  filter(!(is.na(business_mwh) | is.na(residential_mwh)))

ctu_utility_year <- ctu_utility_year %>%
  anti_join(rii_fill %>% select(ctu_name, ctu_class, inventory_year),
    by = c("ctu_name", "ctu_class", "inventory_year")
  ) %>%
  bind_rows(., rii_fill)


# --- Compiled-level YoY QC (post-merge, pre-RII) ---
# Only compare city-years where ALL utilities reported
ctu_complete_years <- ctu_utility_year %>%
  group_by(ctu_name, ctu_class, inventory_year) %>%
  filter(!any(is.na(total_mwh))) %>%
  summarise(total_mwh = sum(total_mwh, na.rm = TRUE), .groups = "drop")

ctu_compiled_yoy <- ctu_complete_years %>%
  filter(total_mwh > 0) %>%
  arrange(ctu_name, ctu_class, inventory_year) %>%
  group_by(ctu_name, ctu_class) %>%
  mutate(
    prev_mwh = lag(total_mwh),
    prev_year = lag(inventory_year),
    pct_change = (total_mwh - prev_mwh) / prev_mwh * 100
  ) %>%
  ungroup()

compiled_flags <- ctu_compiled_yoy %>%
  filter(abs(pct_change) >= 20,
         inventory_year - prev_year == 1) %>%
  arrange(ctu_name, ctu_class, inventory_year)

compiled_flags %>% print(n = Inf)

## Xcel 2022 reporting change?

xcel_2022_jump <- ctu_utility_year %>%
  filter(utility == "Xcel Energy",
         inventory_year %in% c(2021, 2022)) %>%
  select(ctu_name, ctu_class, inventory_year, residential_mwh, business_mwh) %>%
  pivot_wider(names_from = inventory_year, 
              values_from = c(residential_mwh, business_mwh)) %>%
  mutate(
    res_pct = (residential_mwh_2022 - residential_mwh_2021) / residential_mwh_2021 * 100,
    biz_pct = (business_mwh_2022 - business_mwh_2021) / business_mwh_2021 * 100
  ) %>%
  filter(biz_pct >= 20, abs(res_pct) < 10)

xcel_2022_jump %>% print(n = Inf)

ctu_utility_year %>%
  filter(utility == "Xcel Energy", inventory_year %in% 2019:2023) %>%
  group_by(inventory_year) %>%
  summarise(total_res = sum(residential_mwh, na.rm = TRUE),
            total_biz = sum(business_mwh, na.rm = TRUE))
# unclear if this is systemic change, reallocations, or just normal noise.
# assuming the latter for now.

# ── Remove phantom utility rows ──────────────────────────────────────────────
# The spatial intersection scaffold assigns utilities to cities they don't
# actually serve. Drop any utility × city combo that never reported data —
# these are scaffold artifacts, not real service relationships.
# This ensures downstream completeness checks (e.g. !any(is.na(total_mwh)))
# only fire against utilities that genuinely serve each city.

phantoms <- ctu_utility_year %>%
  group_by(ctu_name, ctu_class, utility) %>%
  summarize(ever_reported = any(!is.na(total_mwh)), .groups = "drop") %>%
  filter(!ever_reported)

cat("=== Phantom utility rows removed ===\n")
phantoms %>% arrange(ctu_name, utility) %>% print(n = Inf)

ctu_utility_year <- ctu_utility_year %>%
  anti_join(phantoms, by = c("ctu_name", "ctu_class", "utility"))

cat(sprintf(
  "Removed %d phantom utility × city combos (%d rows)\n",
  nrow(phantoms),
  nrow(phantoms) * n_distinct(ctu_utility_year$inventory_year)
))


# ── De minimis utility filter ─────────────────────────────────────────────────
# Small fringe service from a second utility (e.g. Xcel's 3 MWh in Chaska vs
# City of Chaska Electric's 350K) shouldn't block a city-year from being
# marked complete when the fringe utility has NAs. Identify utility × city
# combos where the utility never exceeds 1% of the city total, and remove them.

de_minimis_threshold <- 0.05

util_shares <- ctu_utility_year %>%
  filter(!is.na(total_mwh)) %>%
  group_by(ctu_name, ctu_class, inventory_year) %>%
  mutate(city_total = sum(total_mwh, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(city_total > 0) %>%
  mutate(share = total_mwh / city_total) %>%
  group_by(ctu_name, ctu_class, utility) %>%
  summarize(max_share = max(share, na.rm = TRUE), .groups = "drop") %>%
  filter(max_share < de_minimis_threshold)

cat("=== De minimis utility × city combos removed ===\n")
util_shares %>%
  arrange(ctu_name, utility) %>%
  print(n = Inf)

ctu_utility_year <- ctu_utility_year %>%
  anti_join(util_shares, by = c("ctu_name", "ctu_class", "utility"))

## save output file

saveRDS(
  ctu_utility_year,
  "_energy/data/ctu_utility_mwh.RDS"
)
