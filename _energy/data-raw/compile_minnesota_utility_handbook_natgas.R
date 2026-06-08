# compile_historical_mn_gas.R
# Compiles historical MN natural gas data from MN utility handbooks (2006, 2012)
# and back-estimates county-level deliveries for pre-2014 years using
# county shares derived from earliest available utility reports.
#
# Data sources: The MN Utility data book
#  https://www.lrl.mn.gov/docs/2009/mandated/090598.pdf
#  https://mn.gov/commerce-stat/pdfs/utility-data-book-2012.pdf
#   - Table 12: MN statewide annual natural gas consumption 1989-2012 (1,000 MCF)
#   - Table 13/14: Company-level deliveries for 2006 and 2012
#   - Existing utility report county-level data (2014+)

source("R/_load_pkgs.R")
source("_energy/data-raw/_energy_emissions_factors.R")


# STATEWIDE ANNUAL TOTALS (Table 12) — units are 1,000 MCF

mn_statewide_gas <- tribble(
  ~year, ~residential, ~commercial, ~industrial, ~electric_gen, ~transport_customers, ~company_use, ~unaccounted_for, ~total,
  2005,  128883, 92825,  34629,  7122,  62576,  168,  2752,  328956,
  2006,  119125, 85914,  29526,  5991,  70756,  181,  5268,  316850,  # note: matches Table 13 total / 1000
  2007,  129208, 80311,  41705,  6463,  77259,  195,  4421,  339562,
  2008,  140152, 90825,  41857,  6427,  78671,  207,  3868,  362008,
  2009,  133172, 91555,  33996,  3114,  74807,  202,  2870,  339714,
  2010,  122787, 78404,  36047,  4131, 100829,  162,  1246,  343647,
  2011,  127351, 76026,  35690,  3341, 106917,  202,    72,  349641,
  2012,  109607, 70810,  30825, 23092, 112014,  331,  1015,  347734
) %>%
  # Convert from 1,000 MCF to MCF for consistency with utility report data
  mutate(across(residential:total, ~ .x * 1000))



# COMPANY-LEVEL TOTALS for metro-relevant utilities


# --- 2006 company data (Table 13) ---
company_detail_2006 <- tribble(
  ~utility_handbook,          ~residential, ~commercial, ~industrial, ~electric_gen, ~transport, ~company_use, ~unaccounted_for, ~total,
  "CenterPoint Energy",        61749831,    48284742,    11436167,     4231559,     18028263,      70584,        2927708,    146729154,
  "Xcel Energy",               31316643,    19102235,    11117889,     1351945,      7633570,      16110,        2320251,     72858643,
  "Centennial Utilities",        207543,      129914,           0,           0,            0,       4721,          -5748,       336430,
  "MN Energy Resources-NMU",   3297586,     3159742,      598224,           0,      7972633,          0,              0,     15028185,
  "MN Energy Resources-PNG",  13633698,     9036350,     1250996,           0,     35176179,          0,              0,     59097222,
  "Greater MN Gas",              220877,       21835,       43301,           0,        54938,          0,              0,       340950
) %>%
  mutate(year = 2006)

# --- 2012 company data (Table 14) ---
company_detail_2012 <- tribble(
  ~utility_handbook,          ~residential, ~commercial, ~industrial, ~electric_gen, ~transport, ~company_use, ~unaccounted_for, ~total,
  "CenterPoint Energy",        57717558,    43828879,     6154958,    21621619,     29482627,      92488,        1011795,    159909924,
  "Xcel Energy",               30038392,    18673629,    10715218,      141288,     26719980,      15922,              0,     86304429,
  "Centennial Utilities",        190705,          NA,           0,           0,       323160,       4378,             NA,       645945,
  "MN Energy Resources-NMU",   2767053,      768480,     2947335,           0,      5080744,       9892,              0,     11573504,
  "MN Energy Resources-PNG",  10945391,      527042,     7384961,           0,     47132316,       7706,              0,     65997416,
  "Greater MN Gas",              276384,      145655,           0,           0,        24928,          0,              0,       446967
) %>%
  mutate(year = 2012)

# --- Combined company detail for both handbook years ---
company_detail_all <- bind_rows(company_detail_2006, company_detail_2012)

# --- Simplified totals view (for back-estimation functions) ---
company_totals_2006 <- company_detail_2006 %>% select(utility_handbook, mcf_total = total)
company_totals_2012 <- company_detail_2012 %>% select(utility_handbook, mcf_total = total)


## use 2006 and 2012 to benchmark how much each utility contributes to state total,
## then use to fill all years

company_props <- company_detail_all %>% 
  mutate(utility_name = if_else(
    grepl("MN Energy", utility_handbook),
    "MN Energy Resources",
    utility_handbook
  )) %>% 
  group_by(utility_name, year) %>% 
  summarize(total_mcf = sum(total)) %>% 
  left_join(mn_statewide_gas,
            by = "year") %>% 
  mutate(utility_prop = total_mcf / total) %>% 
  select(utility_name, year, utility_prop) %>% 
  ungroup() %>%
  # Expand to all years 2005-2012 per utility
  complete(utility_name, year = 2005:2012) %>%
  # Interpolate and extrapolate
  group_by(utility_name) %>%
  arrange(year) %>%
  mutate(
    utility_prop = approx(
      x = year[!is.na(utility_prop)],
      y = utility_prop[!is.na(utility_prop)],
      xout = year,
      rule = 2  # extrapolate using nearest value for 2005
    )$y
  ) %>%
  ungroup()

utility_ests_early <- company_props %>% 
  left_join(mn_statewide_gas,                  
            by = "year") %>% 
  mutate(utility_mcf = total * utility_prop) %>% 
  select(utility_name, utility_mcf, year)

# 3. BUILD COUNTY SHARES FROM 7610 UTILITY REPORTS

# TODO: Update path to match your project structure
utility_county_proportions <- read_rds(here("_energy", "data", "county_natgas_7610_activity.RDS"))



# Select which year's proportions to use for back-estimation
get_county_shares <- function(proportions_data, crosswalk, method = c("earliest", "average")) {
  method <- match.arg(method)
  
  shares <- proportions_data %>%
    left_join(crosswalk, by = "utility")
  
  if (method == "earliest") {
    # Use the earliest year available per utility
    shares <- shares %>%
      group_by(utility) %>%
      filter(year == min(year)) %>%
      ungroup()
  } else {
    # Average proportions across all available years
    shares <- shares %>%
      group_by(utility, utility_handbook, county) %>%
      summarise(
        county_proportion = mean(county_proportion, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  shares %>%
    select(utility, utility_handbook, county, county_proportion)
}

county_shares_avg <- utility_county_proportions %>%
  group_by(utility, county) %>%
  summarise(
    county_proportion = mean(county_proportion, na.rm = TRUE),
    .groups = "drop"
  )

county_share_early <- utility_county_proportions %>%
  group_by(utility) %>%
  filter(year == min(year)) %>%
  ungroup()

# QA: check which years are available per utility
utility_county_proportions %>%
  distinct(utility, year) %>%
  arrange(utility, year) %>%
  print(n = Inf)


# distribute utility total estimates to counties 2005-2012

# --- Mapping from utility folder names to handbook company names ---
utility_name_crosswalk <- tribble(
  ~utility,              ~utility_name,
  "CENTERPOINT ENERGY",  "CenterPoint Energy",
  "NORTHERN STATES POWER CO",         "Xcel Energy",
  "CIRCLE PINES UTILITY CO. (CENTENNIAL)", "Centennial Utilities",
  "GREATER MINNESOTA GAS INC", "Greater MN Gas",
  "MINNESOTA ENERGY RESOURCES",    "MN Energy Resources"
)

utility_county_crosswalk <- utility_ests_early %>% 
  left_join(utility_name_crosswalk,
            by = join_by(utility_name)) %>% 
  left_join(county_shares_avg,
            by = join_by(utility)) %>% 
  ungroup() %>% 
  mutate(mcf_delivered = utility_mcf * county_proportion) %>% 
  select(-c(utility_mcf, utility, county_proportion)) %>% 
  mutate(data_source = "MN utility handbook")

## rebind with 7610 and check for completeness
## MER did not report in 2013 so this is known gap

utility_natgas_activity <- rbind(utility_county_proportions %>% 
                                   left_join(utility_name_crosswalk) %>% 
                                   select(utility_name, county, year, mcf_delivered) %>% 
                                   mutate(data_source = "MN 7610 utility reporting"),
                                 utility_county_crosswalk) %>% 
  rename(county_name = county,
         emissions_year = year)
  
# validation across two reporting formats

# By county
utility_natgas_activity %>%
  group_by(county_name, emissions_year) %>%
  summarise(mcf = sum(mcf_delivered, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = emissions_year, y = mcf / 1e6, color = county_name)) +
  geom_line() +
  geom_vline(xintercept = 2012.5, linetype = "dashed", color = "red") +
  annotate("text", x = 2012.5, y = Inf, label = "Handbook → 7610", 
           vjust = 1.5, hjust = 0.5, color = "red", size = 3) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Natural Gas Deliveries by County", x = NULL, y = "MCF (millions)") +
  theme_minimal()

# By utility
utility_natgas_activity %>%
  group_by(utility_name, emissions_year) %>%
  summarise(mcf = sum(mcf_delivered, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = emissions_year, y = mcf / 1e6, color = utility_name)) +
  geom_line() +
  geom_point(size = 1) +
  geom_vline(xintercept = 2012.5, linetype = "dashed", color = "red") +
  annotate("text", x = 2012.5, y = Inf, label = "Handbook → 7610",
           vjust = 1.5, hjust = 0.5, color = "red", size = 3) +
  scale_y_log10(labels = scales::comma) +
  labs(title = "Natural Gas Deliveries by Utility", x = NULL, y = "MCF (millions, log scale)") +
  theme_minimal()

# There are two gaps: one in MER for 2013 and looks like Centennial is completely blank from 2013 to 2019.

utility_natgas_activity <- utility_natgas_activity %>%
  complete(utility_name, county_name, emissions_year = min(emissions_year):max(emissions_year)) %>%
  group_by(utility_name, county_name) %>%
  arrange(emissions_year) %>%
  mutate(
    mcf_delivered = if (sum(!is.na(mcf_delivered)) >= 2) {
      approx(
        x = emissions_year[!is.na(mcf_delivered)],
        y = mcf_delivered[!is.na(mcf_delivered)],
        xout = emissions_year,
        rule = 1
      )$y
    } else {
      mcf_delivered
    }
  ) %>%
  ungroup() %>%
  # Drop rows that were never real (e.g. Centennial-Carver)
  filter(!is.na(mcf_delivered)) %>% 
  mutate(data_source = if_else(
    is.na(data_source),
    "Interpolated",
    data_source
  ))

write_rds(utility_natgas_activity, here("_energy", "data", "utility_county_natgas_activity.RDS"))

county_natgas_activity <- utility_natgas_activity %>% 
  group_by(county_name, emissions_year) %>% 
  summarize(mcf_delivered = sum(mcf_delivered), .groups = "drop")
  

write_rds(county_natgas_activity, here("_energy", "data", "county_natgas_activity.RDS"))



