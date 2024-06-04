# calculate emissions from aerobic composting using IPCC equations and MPCA data.
source("R/_load_pkgs.R")
score_data <- readRDS("_waste/data/mpca_score.RDS")
if (!exists("gwp")) {
  source("R/global_warming_potential.R")
}

calc_compost_emissions <- function(County, Year, Tons){
  # calculates ch4 and n2o compost emissions in metric tons co2e. see documentation for factor sources.
  percent_compost = 1
  percent_ad = 0 # anaerobic digestion
  mass_compost = Tons * 0.90718474 * percent_compost
  mass_ad = Tons * 0.90718474 * percent_ad
  ef_compost = 10
  ef_ad = 2
  recovered_methane = 0
  
  emissions_ch4 <- ((mass_compost * ef_compost + mass_ad * ef_ad) * 10^(-3) - recovered_methane) * gwp$ch4
  
  ef_compost_n2o = 0.6
  
  emissions_n2o <- (mass_compost * ef_compost_n2o * 10^(-3)) *gwp$n2o
  
  return(tibble(ch4_emissions = emissions_ch4, n2o_emissions = emissions_n2o, year = Year, geog_name = County, source = "Compost"))
}

compost_data <- score_data %>% 
  filter(Method == "Organics") %>% 
  select(County,
         Year, 
         Tons)

compost_yearly <- purrr::pmap(compost_data, calc_compost_emissions)
compost_emissions <- bind_rows(compost_yearly)

