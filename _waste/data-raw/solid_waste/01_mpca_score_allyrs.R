source("R/_load_pkgs.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
## MPCA SCORE ----
# Summary data collected from https://public.tableau.com/app/profile/mpca.data.services/viz/SCOREOverview/1991-2021SCORE


score_summary <- read_csv(file.path(here::here(), "_waste/data-raw/solid_waste/score_summary.csv"))

# filter to only counties in 9-county MN region, for years between 2005 and 2021

mt_conversion_factor <- 0.90718474

score_filtered <- score_summary %>%
  group_by(Year, Method) %>% 
  mutate(state_total = sum(Tons) * mt_conversion_factor) %>% 
  filter(
    County %in% cprg_county$county_name,
    Year %in% 2005:2021
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
  ungroup()

# add score metadata
# mpca_score_meta <- tribble(
#   ~Column, ~Class, ~Description,
#   "County", class(score_filtered$County), "MN county of waste origin",
#   "Management Category", class(score_filtered$`Management Category`), "Waste category
#   (either Mixed Municipal Solid Waste or Combined Recycling and Organics)",
#   "Method", class(score_filtered$Method), "Waste disposal method",
#   "Year", class(score_filtered$Year), "MPCA SCORE data collection year",
#   "Metric Tons", class(score_filtered$`Metric Tons`), "Metric tons of waste collected",
#   "Statewide Total", class(score_filtered$`Statewide Total`), 
#   "Statewide total metric tons collected for given disposal method and year"
# )

saveRDS(score_filtered, paste0("_waste/data-raw/solid_waste/mpca_score_allyrs.RDS"))
# saveRDS(mpca_score_meta, paste0("_waste/data/mpca_score_meta.RDS"))
