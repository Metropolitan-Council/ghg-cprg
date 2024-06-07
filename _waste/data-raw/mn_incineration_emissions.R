# calculate emissions from WTE and onsite burning using IPCC equations and MPCA data
source("R/_load_pkgs.R")
if (!exists("score_data")) {
  score_data <- readRDS("_waste/data/mpca_score.RDS")
}
if (!exists("gwp")) {
  source("R/global_warming_potential.R")
}

# function deprecated ----
# calc_incineration_emissions <- function(County, Year, Metric_Tons, efficiency, ef_n2o, Source){
#   # calculates co2 and n2o emissions from incineration, in metric tons co2e. see documentation for factor sources.
#   fcc <- .4
#   ffc <- .4
#   
#   emissions_co2 <- tonnes * efficiency * fcc * ffc * (44/12)
#   
#   emissions_n2o <- tonnes * ef_n2o * 10^(-6) * gwp$n2o
#   
#   return(tibble(co2_emissions = emissions_co2, n2o_emissions = emissions_n2o, year = Year, geog_name = County, source = Source))
# }
# 
# calc_wte_emissions <- function(County, Year, Tons){
#   calc_incineration_emissions(County, Year, Tons, efficiency = .95, ef_n2o = 50, Source = "Waste to Energy")
# }
# 
# calc_onsite_emissions <- function(County, Year, Tons){
#   calc_incineration_emissions(County, Year, Tons, efficiency = .71, ef_n2o = 150, Source = "Onsite Burning")
# }

# the real calculations ----
# assign factors
fcc <- .4 # fraction of carbon content in MSW, IPCC default
ffc <- .4 # fraction of fossil carbon in MSW, IPCC default
co2_factor <- fcc * ffc * 44/12
co2_efficiency_wte <- .95 # efficiency of combustion for incineration, IPCC default
co2_efficiency_onsite <- .71 # efficiency of combustion for onsite burning, GHG Protocol default (IPCC does not provide one)
n2o_emissions_factor_wte <- 50 # aggregate emissions factor for incineration, g N2O/metric tons waste, GHG Protocol default
n2o_emissions_factor_onsite <- 150 # aggregate emissions factor for open burning, g N2O/metric tons waste, GHG Protocol default

incin_factors <- tibble(
  Method = c("WTE", "Onsite"),
  co2 = co2_factor * c(co2_efficiency_wte, co2_efficiency_onsite),
  n2o = 10^(-6) * c(n2o_emissions_factor_wte, n2o_emissions_factor_onsite)
)

incineration_data <- score_data %>% 
  filter(Method %in% c("WTE", "Onsite")) %>% 
  select(County,
         Method,
         Year, 
         `Metric Tons`) %>% 
  left_join(incin_factors, by = join_by(Method)) %>% 
  mutate(
    emissions_metric_tons_co2 = `Metric Tons` * co2,
    emissions_metric_tons_n2o = `Metric Tons` * n2o 
  ) %>% 
  select(
    County,
    Method,
    Year,
    emissions_metric_tons_co2,
    emissions_metric_tons_n2o
  )

# decide whether to convert to co2e
# write meta
# save as rds
