# calculate emissions from WTE and onsite burning using IPCC equations and MPCA data
source("R/_load_pkgs.R")
score_data <- readRDS("_waste/data/mpca_score.RDS")
if (!exists("gwp")) {
  source("R/global_warming_potential.R")
}

calc_incineration_emissions <- function(County, Year, Tons, efficiency, ef_n2o, Source){
  # calculates co2 and n2o emissions from incineration, in metric tons co2e. see documentation for factor sources.
  fcc <- .4
  ffc <- .4
  tonnes <- Tons * 0.90718474 # convert short tons (reported by MPCA) to metric tons (expected by IPCC/GHGP)
  
  emissions_co2 <- tonnes * efficiency * fcc * ffc * (44/12)
  
  emissions_n2o <- tonnes * ef_n2o * 10^(-6) * gwp$n2o
  
  return(tibble(co2_emissions = emissions_co2, n2o_emissions = emissions_n2o, year = Year, geog_name = County, source = Source))
}

calc_wte_emissions <- function(County, Year, Tons){
  calc_incineration_emissions(County, Year, Tons, efficiency = .95, ef_n2o = 50, Source = "Waste to Energy")
}

calc_onsite_emissions <- function(County, Year, Tons){
  calc_incineration_emissions(County, Year, Tons, efficiency = .71, ef_n2o = 150, Source = "Onsite Burning")
}

wte_data <- score_data %>% 
  filter(Method == "WTE") %>% 
  select(County,
         Year, 
         Tons)

wte_yearly <- purrr::pmap(wte_data, calc_wte_emissions)
wte_emissions <- bind_rows(wte_yearly)

onsite_data <- score_data %>% 
  filter(Method == "Onsite") %>% 
  select(County,
         Year, 
         Tons)

onsite_yearly <- purrr::pmap(onsite_data, calc_onsite_emissions)
onsite_emissions <- bind_rows(onsite_yearly)
