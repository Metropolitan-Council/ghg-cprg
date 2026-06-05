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


# start carving sector chunks out of 7610 reporting
#   county utility total (7610)
#     - residential  (CTU model-derived)
#     - power plants (GHGRP subpart D, backcasted to 2005)
#     - industrial combustion (GHGRP subpart C, non-powerplant)
#     - industrial natural gas (MPCA flagged)
#     = commercial remainder

county_res <- ctu_ng %>% 
  filter(sector == "Residential",
         source == "Natural Gas") %>% 
  group_by(county_name, emissions_year, sector, category, source) %>% 
  summarize(mcf = sum(mcf)) %>% 
  ungroup()

county_commercial <- county_mcf %>% 
  filter(!county_name %in% c("Chisago", "Sherburne")) %>% 
  left_join(county_res,
            by = c("county_name",
                   "emissions_year")
  ) %>% 
  left_join(industrial_combustion_full) %>% 
  left_join(powerplant_natgas) %>% 
  mutate(
    mcf_industrial_combined = replace_na(mcf_industrial_combined, 0),
    mcf_powerplant = replace_na(mcf_powerplant, 0),
    mcf_comm = mcf_delivered - (mcf + mcf_powerplant + mcf_industrial_combined),
    sector = "Commercial"
  )
  
county_commercial %>%
  group_by(county_name, emissions_year) %>%
  summarise(
    Residential = sum(mcf),
    Industrial = first(mcf_industrial_combined),
    Commercial = first(mcf_comm),
    Powerplant = first(mcf_powerplant),
    .groups = "drop"
  ) %>%
  pivot_longer(Residential:Powerplant, names_to = "sector", values_to = "mcf") %>%
  ggplot(aes(x = emissions_year, y = mcf / 1e6, color = sector)) +
  geom_line() +
  geom_point(size = 1) +
  facet_wrap(~county_name, scales = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Natural Gas Deliveries by Sector", x = NULL, y = "MCF (millions)", color = "Sector") +
  theme_minimal()

write_csv(county_commercial,
          "C:/Users/WilfahPA/Documents/county_commercial.csv")
