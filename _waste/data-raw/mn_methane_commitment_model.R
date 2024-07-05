# Import necessary info
source("R/_load_pkgs.R")
if (!exists("score_data")) {
  score_data <- readRDS("_waste/data/mpca_score.RDS")
}
waste_comp <- readRDS("_waste/data/mn_waste_composition.RDS")
methane_recovery_mn <- readRDS("_waste/data/methane_recovery_mn.RDS")

## Methane Commitment model ----

# variables: see documentation for sources ----

# methane correction factor (MCF)
# assuming landfills managed well, semi-aerobic (see GHG Protocol)
mcf = 0.5

# fraction of degradable organic carbon degraded (DOC_f)
doc_f = 0.6

# fraction of methane in landfill gas (F)
f = 0.5

# oxidation factor (OX)
ox = 0.1 # for well-managed landfills


# Calculate DOC

ipcc_doc_factors <- tibble(
  Category = c("Paper", "Textiles", "Organics (Non-Food)", "Organics (Food)", "Wood"),
  Factor = c(0.4, 0.4, 0.17, 0.15, 0.3)
)

doc_sum <- waste_comp %>% 
  inner_join(ipcc_doc_factors, by = join_by(Category)) %>% 
  mutate(doc_content = Mean * Factor) %>% 
  summarize(doc_total = sum(doc_content), degradable_fraction = sum(Mean))

doc <- doc_sum$doc_total

l_0 = mcf * doc * doc_f * f * 16/12

# function deprecated ----

# calc_landfill_emissions <- function(County, Year, Tons, f_rec){
#   mcf = 0.5
#   # calc doc here
#   # placeholder value:
#   doc = 0.5 #PLACEHOLDER
#   doc_f = 0.6
#   f = 0.5
#   l_0 = mcf * doc * doc_f * f * 16/12
#   ox = 0.1
#   msw = Tons * 0.90718474 # conversion factor short tons to metric tons
#   
#   emissions = msw * l_0 * (1-f_rec) * (1-ox)
#   return(tibble(ch4_emissions = emissions, year = Year, county = County))
# }


# landfill data from MPCA SCORE report. Transformed to metric tons ----
landfill_data <- score_data %>% 
  filter(Method == "Landfill") %>% 
  select(County,
         Year, 
         `Metric Tons`,
         Method)

# join both lines of f_rec by county and year
# calculate sum (flaring + lfgte = rec)

# ch4_yearly <- purrr::pmap(landfill_data, calc_landfill_emissions)
# landfill_emissions <- bind_rows(ch4_yearly)

# use mutate to calculate emissions = ((Tons * l_0) - rec) * (1-ox)

# placeholder data
landfill_emissions <- landfill_data %>% 
  filter(Year %in% c(2005, 2021)) %>% 
  left_join(methane_recovery_mn, by = join_by(County, Year)) %>% 
  mutate(
    total_ch4 = (`Metric Tons` * l_0 - total_metric_tons_ch4_recovered) * (1-ox)
  ) %>% 
  select(
    County,
    Method,
    Year,
    total_ch4
  )

