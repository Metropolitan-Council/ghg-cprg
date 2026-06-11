# develop natural gas sector proportioning script based on met council data analyses
# basic process is:
#   county utility total (7610)
#     - residential  (CTU model-derived)
#     - power plants (GHGRP subpart D, backcasted to 2005)
#     - industrial combustion (GHGRP subpart C, non-powerplant)
#     - industrial natural gas (MPCA flagged)
#     = commercial remainder


source("R/_load_pkgs.R")
source("R/global_warming_potential.R")

# CTU natural gas (residential + business)

ctu_ng <- readRDS(file.path(
  here::here(), "_energy/data/_ctu_natgas_emissions.rds"
))

# county 7610 totals 
county_mcf <- readRDS("_energy/data/county_natgas_activity.RDS")

## ghgrp - industrial combustion

subpart_c_emissions <- readRDS(file.path(
  here::here(), "_industrial/data/fuel_combustion_activity.rds"
))

### pull out powerplants and backcast to 2005

# Power plant gas by county-year
powerplant_natgas_measured <- subpart_c_emissions %>%
  filter(
    power_plant       == TRUE,
    general_fuel_type == "Natural Gas",
    facility_name     != "Minnesota River Station" # only three very inconsistent years of reporting
  ) %>%
  group_by(county_name, reporting_year) %>%
  summarize(
    mcf_powerplant = sum(value_activity / 1000, na.rm = TRUE),
    n_facilities   = n_distinct(facility_name),
    facility_names = paste(sort(unique(facility_name)), collapse = "; "),
    .groups = "drop"
  ) %>%
  rename(emissions_year = reporting_year)

# Backcast to 2005–2010 via Kalman smoothing
all_pp_counties <- unique(powerplant_natgas_measured$county_name)

powerplant_natgas <- expand.grid(
  emissions_year = 2005:2023,
  county_name    = all_pp_counties,
  stringsAsFactors = FALSE
) %>%
  left_join(powerplant_natgas_measured, by = c("emissions_year", "county_name")) %>%
  group_by(county_name) %>%
  arrange(emissions_year) %>%
  mutate(
    mcf_powerplant = na_kalman(mcf_powerplant),
    # Fill facility info from measured years (constant per county)
    facility_names = first(na.omit(facility_names)),
    n_facilities   = first(na.omit(n_facilities)),
    pp_data_type   = case_when(
      !is.na(mcf_powerplant) ~ "measured",
      emissions_year < 2011  ~ "backcasted",
      TRUE                   ~ "interpolated"
    )
  ) %>%
  ungroup()

# All natural gas combustion by county-year (industrial, non-powerplant)
ghgrp_industrial_combustion <- subpart_c_emissions %>%
  filter(
    general_fuel_type == "Natural Gas",
    power_plant == FALSE,
    !county_name %in% c("Sherburne","Chisago")
  ) 

ghgrp_facilities <- ghgrp_industrial_combustion %>% 
  distinct(city_name, facility_name)

ghgrp_county_combustion <- ghgrp_industrial_combustion %>%
  filter(!facility_name %in% c("Flint Hills Resources Pine Bend Refinery",
                                "St. Paul Park Refining Company, LLC")) %>% 
  group_by(county_name, reporting_year) %>%
  summarize(
    mcf_ghgrp_industrial = sum(value_activity / 1000, na.rm = TRUE),
    n_facilities         = n_distinct(facility_name),
    .groups = "drop"
  ) %>%
  rename(emissions_year = reporting_year)

# ── MPCA industrial natural gas ───────────────────────────────────────────────
mpca_industrial <- readRDS("_industrial/data/mpca_fuel_activity.RDS") %>%
  mutate(ctu_name = str_replace_all(ctu_name, "St.", "Saint")) %>%
  filter(
    sector       == "Industrial",
    fuel_type == "Natural Gas",
    !county_name %in% c("Sherburne","Chisago")
  ) %>% 
  rename(emissions_year = inventory_year)

mpca_facilities <- mpca_industrial %>% 
  distinct(ctu_name, source_name)

# Facilities present in GHGRP are excluded from MPCA to avoid double-counting.
# Matches identified by cross-referencing ghgrp_facilities and mpca_facilities;
# MPCA renames facilities across years so we match on source_name patterns
# rather than exact strings.
mpca_ghgrp_overlap <- c(
  # 3M Cottage Grove — multiple MPCA entries for same complex
  "3M - Cottage Grove - Specialty Additives",
  "3M - Cottage Grove - Utilities/Support Svcs",
  "3M - Cottage Grove Center Utilities",
  "3M - Cottage Grove - Abrasive Systems Division",
  "3M - Cottage Grove",
  "3M Cottage Grove",
  "3M - Cottage Grove - Bldg 133/Traffic Safety",
  "3M - Cottage Grove - Tape Manufacturing",
  # 3M Maplewood
  "3M CO",
  # Anchor Glass Shakopee
  "Anchor Glass Container Corp",
  # Consolidated Precision Products Bloomington
  "Consolidated Precision Products",
  "Consolidated Precision Products - Minneapolis",
  # Gerdau Saint Paul
  "Gerdau Ameristeel US Inc - Saint Paul Mill",
  "Gerdau - Saint Paul Mill",
  # Gopher Resource Eagan
  "Gopher Resource",
  "Gopher Resource LLC",
  # Honeywell Plymouth
  "Honeywell-Plymouth Operations",
  "Honeywell - Plymouth Operation",
  # Polar Semiconductor Bloomington
  "Polar Semiconductor LLC",
  # Rahr Malting Shakopee
  "Rahr Malting Co",
  "Rahr Malting Co.",
  # SkyWater Technology Bloomington
  "SkyWater Technology Foundry",
  "SkyWater Technology Foundry INC",
  # WestRock Saint Paul
  "WestRock MN Corp",
  # Flint Hills Pine Bend Refinery (Dakota) — all MPCA name variants
  "Flint Hills Resources Pine Bend Refinery",
  "Flint Hills Resources Pine Bend LLC - Wescott Terminal",
  "Flint Hills Resources Pine Bend LLC - Savage",
  # Saint Paul Park Refinery (Washington) — all MPCA name variants
  "Saint Paul Park Refining Co LLC",
  "Saint Paul Park Refining Co LLC dba Marathon Saint Paul Park Refinery",
  "Marathon Saint Paul Park Refinery"
)

mpca_county <- mpca_industrial %>%
  filter(!source_name %in% mpca_ghgrp_overlap) %>%
  group_by(county_name, emissions_year) %>%
  summarize(
    mcf_mpca_industrial = sum(value_activity / 1000, na.rm = TRUE),
    .groups = "drop"
  )

### extend back to 2005
# Step 1) extend MPCA back to 2011.
# Step 2) sum MPCA and GHGRP counties
# Step 3) extend the summation back to 2005.
# Seems more stable than individual extensions backward

all_industrial_counties <- union(
  unique(ghgrp_county_combustion$county_name),
  unique(mpca_county$county_name)
)

#1
mpca_county_backcasted <- expand.grid(
  emissions_year = 2011:2023,
  county_name    = all_industrial_counties,
  stringsAsFactors = FALSE
) %>%
  left_join(mpca_county, by = c("emissions_year", "county_name")) %>%
  group_by(county_name) %>%
  arrange(emissions_year) %>%
  mutate(
    mcf_mpca_industrial = na_kalman(mcf_mpca_industrial),
    mpca_data_type = case_when(
      emissions_year >= 2016 ~ "measured",
      TRUE                   ~ "backcasted"
    )
  ) %>%
  ungroup()

#2
industrial_combined_2011_2023 <- expand.grid(
  emissions_year = 2011:2023,
  county_name    = all_industrial_counties,
  stringsAsFactors = FALSE
) %>%
  left_join(
    ghgrp_county_combustion %>% select(emissions_year, county_name, mcf_ghgrp_industrial),
    by = c("emissions_year", "county_name")
  ) %>%
  left_join(
    mpca_county_backcasted %>% select(emissions_year, county_name, mcf_mpca_industrial),
    by = c("emissions_year", "county_name")
  ) %>%
  mutate(
    mcf_ghgrp_industrial = replace_na(mcf_ghgrp_industrial, 0),
    mcf_mpca_industrial  = replace_na(mcf_mpca_industrial, 0),
    mcf_industrial_combined = mcf_ghgrp_industrial + mcf_mpca_industrial
  )

#3
industrial_combustion_full <- expand.grid(
  emissions_year = 2005:2023,
  county_name    = all_industrial_counties,
  stringsAsFactors = FALSE
) %>%
  left_join(
    industrial_combined_2011_2023 %>%
      select(emissions_year, county_name, mcf_industrial_combined),
    by = c("emissions_year", "county_name")
  ) %>%
  group_by(county_name) %>%
  arrange(emissions_year) %>%
  mutate(
    mcf_industrial_combined = na_kalman(mcf_industrial_combined),
    data_type = case_when(
      emissions_year >= 2011 ~ "measured/modeled",
      TRUE                   ~ "backcasted"
    )
  ) %>%
  ungroup()


## examine how emissions change at the breakpoints
ggplot(industrial_combustion_full,
       aes(x = emissions_year, y = mcf_industrial_combined, col = county_name)) +
  geom_line() +
  geom_vline(xintercept = c(2011, 2016), linetype = "dashed", alpha = 0.4) +
  labs(title = "Industrial natural gas: combined GHGRP + MPCA with backcast",
       subtitle = "Dashed lines: GHGRP start (2011), MPCA start (2016)",
       y = "mcf", x = NULL)



# ── Sector aggregation from CTU model ────────────────────────────────────────

county_res <- ctu_ng %>% 
  filter(sector == "Residential",
         source == "Natural Gas") %>% 
  group_by(county_name, emissions_year, sector, category, source) %>% 
  summarize(mcf = sum(mcf), .groups = "drop")

county_business_rf <- ctu_ng %>%
  filter(sector == "Business",
         source == "Natural Gas") %>%
  group_by(county_name, emissions_year) %>%
  summarize(mcf_business_rf = sum(mcf, na.rm = TRUE), .groups = "drop")

# ── Build wide county table with all sectors + residual ──────────────────────

county_wide <- county_mcf %>% 
  filter(!county_name %in% c("Chisago", "Sherburne")) %>% 
  left_join(county_res, by = c("county_name", "emissions_year")) %>% 
  left_join(industrial_combustion_full) %>% 
  left_join(powerplant_natgas) %>% 
  mutate(
    mcf_industrial_combined = replace_na(mcf_industrial_combined, 0),
    mcf_powerplant = replace_na(mcf_powerplant, 0)
  ) %>%
  group_by(county_name, emissions_year) %>%
  summarize(
    mcf_delivered = first(mcf_delivered),
    Residential = sum(mcf),
    Industrial = first(mcf_industrial_combined),
    Powerplant = first(mcf_powerplant),
    .groups = "drop"
  ) %>%
  left_join(county_business_rf, by = c("county_name", "emissions_year")) %>%
  rename(Business = mcf_business_rf) %>%
  mutate(
    Business = replace_na(Business, 0),
    Residual = mcf_delivered - (Residential + Business + Industrial + Powerplant)
  )

# ── Save pre-adjustment residual diagnostic graph ────────────────────────────

# (a) Residual diagnostic with bold residual line
p_residual_diagnostic <- county_wide %>%
  filter(emissions_year >= 2013) %>% 
  pivot_longer(Residential:Residual, names_to = "sector", values_to = "mcf") %>%
  ggplot(aes(x = emissions_year, y = mcf / 1e6, color = sector,
             linewidth = sector)) +
  geom_line() +
  geom_point(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
  scale_linewidth_manual(values = c("Residual" = 1.2, "Residential" = 0.5,
                                    "Business" = 0.5, "Industrial" = 0.5,
                                    "Powerplant" = 0.5), guide = "none") +
  facet_wrap(~county_name, scales = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Natural Gas: All Sectors + Residual vs 7610 Total (pre-adjustment)",
       x = NULL, y = "MCF (millions)", color = "Sector") +
  theme_minimal()
p_residual_diagnostic

# (b) Stacked bar for 2022 with 7610 total overlay
county_wide %>%
  filter(emissions_year == 2022) %>%
  pivot_longer(c(Residential, Business, Industrial, Powerplant),
               names_to = "sector", values_to = "mcf") %>%
  ggplot(aes(x = county_name)) +
  geom_col(aes(y = mcf / 1e6, fill = sector)) +
  geom_point(aes(y = mcf_delivered / 1e6), size = 3, shape = 95, stroke = 3) +
  geom_hline(yintercept = 0, alpha = 0.3) +
  scale_fill_manual(values = c(
    "Residential" = "#2166AC",
    "Business"    = "#B2182B",
    "Industrial"  = "#F4A582",
    "Powerplant"  = "#4DAF4A"
  )) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "2022 Natural Gas: Sector Breakdown vs 7610 Total",
       subtitle = "Black marks = 7610 county total",
       x = NULL, y = "MCF (millions)", fill = "Sector") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ── Adjust residential and business to absorb residual ───────────────────────

# 2013-2015 benchmark: average residual as % of (res + business)
residual_benchmark <- county_wide %>%
  filter(emissions_year %in% 2013:2015,
         county_name != "Washington") %>%
  group_by(county_name) %>%
  summarize(
    mean_residual_pct = mean(Residual / (Residential + Business), na.rm = TRUE),
    .groups = "drop"
  )

county_adjusted <- county_wide %>%
  left_join(residual_benchmark, by = "county_name") %>%
  mutate(
    # Washington: residual becomes Refinery
    Refinery = if_else(county_name == "Washington",
                       mcf_delivered - (Residential + Business + Industrial + Powerplant),
                       0),
    # Determine year-specific or benchmark residual
    effective_residual = case_when(
      county_name == "Washington" ~ 0,
      emissions_year >= 2013 ~ Residual,
      TRUE ~ (Residential + Business) * mean_residual_pct
    ),
    # For negative residuals: first absorb from Business up to Industrial amount
    industrial_overlap = case_when(
      effective_residual < 0 ~ pmin(abs(effective_residual), Industrial),
      TRUE ~ 0
    ),
    Business = Business - industrial_overlap,
    remaining_residual = effective_residual + industrial_overlap,  # closer to zero or positive
    # Distribute whatever remains proportionally across res and business
    adj_factor = if_else(
      county_name == "Washington" | (Residential + Business) == 0,
      1,
      1 + remaining_residual / (Residential + Business)
    ),
    Residential = Residential * adj_factor,
    Business = Business * adj_factor
  ) %>%
  select(-mean_residual_pct, -adj_factor, -effective_residual, 
         -industrial_overlap, -remaining_residual)

# ── Final output ─────────────────────────────────────────────────────────────

county_mcf_activity <- county_adjusted %>%
  select(county_name, emissions_year, Residential, Business, Industrial, Powerplant, Refinery) %>%
  pivot_longer(Residential:Refinery, names_to = "sector", values_to = "value_activity") %>%
  filter(value_activity != 0 | sector != "Refinery") %>%  # drop zero-Refinery rows for non-Washington
  mutate(category = "Building fuel",
         source = "Natural gas",
         unit_activity = "MCF delivered")

county_mcf_activity %>%
  ggplot(aes(x = emissions_year, y = value_activity / 1e6, color = sector)) +
  geom_line() +
  geom_point(size = 1) +
  facet_wrap(~county_name, scales = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Natural Gas Deliveries by Sector (adjusted)",
       x = NULL, y = "MCF (millions)", color = "Sector") +
  theme_minimal()

write_rds(county_mcf_activity, here("_energy", "data", "county_natgas_activity_by_sector.RDS"))

### convert to emissions

source("_energy/data-raw/_energy_emissions_factors.R")

county_emissions_by_gas <- county_mcf_activity %>%
  group_by(county_name, emissions_year, sector, category, source) %>%
  summarize(mcf = sum(value_activity)) %>%
  ungroup() %>%
  mutate(
    CO2_emissions_mt = mcf * epa_emissionsHub_naturalGas_factor_lbsCO2_perMCF %>%
      units::as_units("pound") %>%
      units::set_units("metric_ton") %>%
      as.numeric(),
    CH4_emissions_mt = mcf * epa_emissionsHub_naturalGas_factor_lbsCH4_perMCF %>%
      units::as_units("pound") %>%
      units::set_units("metric_ton") %>%
      as.numeric(),
    N2O_emissions_mt = mcf * epa_emissionsHub_naturalGas_factor_lbsN2O_perMCF %>%
      units::as_units("pound") %>%
      units::set_units("metric_ton") %>%
      as.numeric(),
    CO2e_emissions_mt = CO2_emissions_mt + CH4_emissions_mt + N2O_emissions_mt
  )

write_rds(county_emissions_by_gas, here("_energy", "data", "county_natgas_emissions_by_gas.RDS"))

county_emissions_out <- county_emissions_by_gas %>% 
  select(county_name, emissions_year, sector, category, source, value_emissions = CO2e_emissions_mt) %>% 
  mutate(unit_emissions = "Metric tons CO2e",
         data_source = case_when(
           sector == "Residential" ~ "Met Council modeling",
           sector == "Business" ~ "Met Council modeling",
           sector == "Refinery" ~ "Unaccounted for Washington County natural gas",
           sector %in% c("Industrial", "Powerplant") & emissions_year >= 2011 ~ "EPA and MPCA reporting",
           sector %in% c("Industrial", "Powerplant") & emissions_year < 2011 ~ "EPA and MPCA extrapolation",
         ),
         factor_source = "Federal Register EPA; 40 CFR Part 98")

write_rds(county_emissions_out,
             here("_energy", "data", "county_natgas_emissions_by_sector.RDS"))

