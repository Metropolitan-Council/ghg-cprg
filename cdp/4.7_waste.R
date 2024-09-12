# Takes solid waste and wastewater activity data and summarizes/analyzes to CDP categories

# Total amount of solid waste generated (metric tonnes/year)
# percentage of the total solid waste generated that is utilized for waste to energy (%)
# Percentage of the total solid waste generated that is diverted away from landfill and incineration (%)
# Percentage of the diverted solid waste generated that is recycled (%),
# Percentage of the diverted solid waste generated that is reused (%),
# Percentage of waste collected where separation at source is taking place (%),
# Total annual amount of food waste produced in the jurisdiction (tonnes/year),
# Volume of wastewater produced within the jurisdiction boundary (megalitres/year),
# Percentage of wastewater safely treated to at least secondary level (%)
source("R/_load_pkgs.R")

# Solid waste ----
## read in score data
mpca_score <- readRDS(file.path(here::here(), "_waste/data/mpca_score_2021.RDS"))
met_counties <- c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington")

cdp_waste <- mpca_score %>%
  filter(County %in% met_counties) %>% 
  select(
    !`Management Category`
  ) %>% 
  group_by(`Method`) %>% 
  dplyr::summarize(regional_total = sum(Tons)*0.90718474) %>% #converted to metric tons here
  pivot_wider(
    names_from = `Method`,
    values_from = regional_total
  ) %>% 
  mutate(
    total_waste = rowSums(across(Landfill:WTE)), 
    percentage_wte = WTE/total_waste,
    total_diverted = `MSW Compost` + Organics + Recycling,
    percentage_diverted = total_diverted/total_waste,
    percentage_recycled = Recycling/total_diverted,
    percentage_reused = "Unknown",
    percentage_separation_at_source = "Unknown",
    food_waste = "Unknown",
    wastewater_volume = "Unknown",
    wastewater_treated = "Unknown",
    year = 2021
  ) %>% 
  select(
    !c("Landfill", "MSW Compost", "Onsite", "Organics", "Recycling", "WTE", "total_diverted")
  )

