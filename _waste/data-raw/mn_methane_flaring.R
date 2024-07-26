# Calculates landfill methane recovered through flaring and landfill gas to energy.
# Not in use currently due to discrepancies with MPCA data.

source("R/_load_pkgs.R")

#cprg_county_proportions <- readRDS(file.path(here::here(), "_meta/data/cprg_county_proportions.RDS"))
mpca_score <- readRDS(file.path(here::here(), "_waste/data-raw/mpca_score_allyrs.RDS"))

# pull in and calculate f_rec ----

# Source: EPA State Inventory Solid Waste tool, methane flaring and LFGTE data
# https://www.epa.gov/statelocalenergy/state-inventory-and-projection-tool

# cleaning xcel data to preserve full values
# Flaring
flaring_data <- readxl::read_xlsx(file.path(here::here(), "_waste/data-raw/solid_waste_flaring.xlsx"),
  range = "A2:AG54"
) %>%
  rename(State = 1) %>%
  filter(State == "MN")


flaring_data <- data.frame(t(flaring_data)) %>%
  tibble::rownames_to_column("Year") %>%
  rename(flared_metric_tons_ch4_mn = t.flaring_data.) # this is currently mmt. it will be converted

flaring_data <- flaring_data[-1, ] %>%
  mutate(
    flared_metric_tons_ch4_mn = as.numeric(flared_metric_tons_ch4_mn) * 10^6 # converting mmt to mt
  )

# LFGTE

lfgte_data <- readxl::read_xlsx(file.path(here::here(), "_waste/data-raw/solid_waste_lfgte.xlsx"),
  range = "A2:AG54"
) %>%
  rename(State = 1) %>%
  filter(State == "MN")

lfgte_data <- data.frame(t(lfgte_data)) %>%
  tibble::rownames_to_column("Year") %>%
  rename(lfgte_metric_tons_ch4_mn = t.lfgte_data.)

lfgte_data <- lfgte_data[-1, ] %>%
  mutate(
    lfgte_metric_tons_ch4_mn = as.numeric(lfgte_metric_tons_ch4_mn) * 10^6
  )

# join dfs, filter to 2021/2005, join county proportions, allocate by population - TO CHANGE
# instead allocate by proportion of MN landfill

methane_recovery_state <- flaring_data %>%
  left_join(lfgte_data, by = join_by(Year)) %>%
  mutate(Year = as.numeric(Year)) %>% 
  select(
    flared_metric_tons_ch4_mn,
    lfgte_metric_tons_ch4_mn,
    Year
  )

# calculate county proportions of landfill waste, join methane recovery numbers, allocate
methane_recovery_counties <- mpca_score %>% 
  filter(Method == "Landfill") %>% 
  mutate(
    landfill_proportion = `Metric Tons`/`Statewide Total`
  ) %>% 
  left_join(methane_recovery_state, by = join_by(Year)) %>% 
  mutate(
    flared_ch4 = flared_metric_tons_ch4_mn * landfill_proportion,
    lfgte_ch4 = lfgte_metric_tons_ch4_mn * landfill_proportion
  ) %>% 
  mutate(
    total_ch4_recovered = flared_ch4 + lfgte_ch4
  ) %>% 
  select(
    County,
    Method,
    Year,
    flared_ch4,
    lfgte_ch4,
    total_ch4_recovered
  )

  

# methane_recovery_counties <- cprg_county_proportions %>%
#   rename(Year = year) %>%
#   filter(Year %in% c(2005, 2021)) %>%
#   left_join(methane_recovery_mn, by = join_by(Year)) %>%
#   mutate(
#     flared_metric_tons_ch4 = flared_metric_tons_ch4_mn * county_proportion_of_state_pop,
#     lfgte_metric_tons_ch4 = lfgte_metric_tons_ch4_mn * county_proportion_of_state_pop,
#     Year = as.numeric(Year)
#   ) %>%
#   mutate(
#     total_metric_tons_ch4_recovered = flared_metric_tons_ch4 + lfgte_metric_tons_ch4
#   ) %>%
#   select(
#     County = NAME,
#     Year,
#     flared_metric_tons_ch4,
#     lfgte_metric_tons_ch4,
#     total_metric_tons_ch4_recovered
#   )

# to test: ensure that each year adds up to total
# meta
methane_recovery_mn_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "County", class(methane_recovery_counties$County), "Emissions estimation county",
    "Method", class(methane_recovery_counties$Method), "Method category of emissions (for methane recovery data this should be Landfill)",
    "Year", class(methane_recovery_counties$Year), "Emissions estimation year",
    "flared_ch4", class(methane_recovery_counties$flared_ch4), "Amount of landfill gas recovered through flaring, in metric tons CH~4~",
    "lfgte_ch4", class(methane_recovery_counties$lfgte_ch4), "Amount of landfill gas recovered through gas to energy efforts, in metric tons CH~4~",
    "total_ch4_recovered", class(methane_recovery_counties$total_ch4_recovered), "Total metric tons CH~4~ recovered through flaring and gas to energy"
  )
# save as rds
saveRDS(methane_recovery_counties, "_waste/data/methane_recovery_mn.RDS")
saveRDS(methane_recovery_mn_meta, "_waste/data/methane_recovery_mn_meta.RDS")
