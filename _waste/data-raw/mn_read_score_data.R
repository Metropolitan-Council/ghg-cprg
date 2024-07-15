source("R/_load_pkgs.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
## MPCA SCORE ----
# Summary data collected from https://public.tableau.com/app/profile/mpca.data.services/viz/SCOREOverview/1991-2021SCORE


score_summary <- read_csv(file.path(here::here(), "_waste/data-raw/score_summary.csv"))

# filter to only counties in 9-county MN region, for years between 2005 and 2021

mt_conversion_factor <- 0.90718474

score_filtered <- score_summary %>%
  group_by(Year, Method) %>% 
  mutate(`Statewide Total` = sum(Tons)) %>% 
  filter(
    County %in% cprg_county$NAME,
    Year %in% 2005:2021
  ) %>%
  mutate(
    "Metric Tons" = Tons * 0.90718474, # convert short tons to metric tons (for consistency with IPCC values)
    "Statewide Total" = `Statewide Total` * 0.90718474
  ) %>%
  select(
    County,
    "Management Category" = "Mangement Method",
    Method,
    Year,
    "Metric Tons",
    "Statewide Total"
  )

# add score metadata
mpca_score_meta <- tribble(
  ~Column, ~Class, ~Description,
  "County", class(score_filtered$County), "MN county of waste origin",
  "Management Category", class(score_filtered$`Management Category`), "Waste category
  (either Mixed Municipal Solid Waste or Combined Recycling and Organics)",
  "Method", class(score_filtered$Method), "Waste disposal method",
  "Year", class(score_filtered$Year), "MPCA SCORE data collection year",
  "Metric Tons", class(score_filtered$`Metric Tons`), "Metric tons of waste collected",
  "Statewide Total", class(score_filtered$`Statewide Total`), 
  "Statewide total metric tons collected for given disposal method and year"
)

saveRDS(score_filtered, paste0("_waste/data/mpca_score.RDS"))
saveRDS(mpca_score_meta, paste0("_waste/data/mpca_score_meta.RDS"))
