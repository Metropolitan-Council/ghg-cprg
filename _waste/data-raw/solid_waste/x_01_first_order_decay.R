source("R/_load_pkgs.R")

# use MPCA SCORE data for 1991-2021 ----
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
score_summary <- read_csv(file.path(here::here(), "_waste/data-raw/solid_waste/score_summary.csv"))

# filter to only counties in 9-county MN region, for years between 2005 and 2021

mt_conversion_factor <- 0.90718474

mpca_score_long <- score_summary %>%
  group_by(Year, Method) %>%
  mutate(state_total = sum(Tons) * mt_conversion_factor) %>%
  filter(
    County %in% cprg_county$county_name
  ) %>%
  mutate(
    value_activity = Tons * mt_conversion_factor, # convert short tons to metric tons (for consistency with IPCC values)
    units_activity = "metric tons MSW"
  ) %>%
  left_join(cprg_county, by = join_by(County == county_name)) %>%
  mutate(Method = ifelse(Method == "WTE", "Waste to energy", Method)) %>%
  select(
    geoid,
    source = Method,
    inventory_year = Year,
    value_activity,
    units_activity,
    state_total
  ) %>%
  ungroup() %>% 
  filter(
    source == "Landfill"
  )

# 

