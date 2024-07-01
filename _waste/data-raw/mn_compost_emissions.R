# calculate emissions from aerobic composting using IPCC equations and MPCA data.
source("R/_load_pkgs.R")
if (!exists("score_data")) {
  score_data <- readRDS("_waste/data/mpca_score.RDS")
}

# function deprecated ----

# calc_compost_emissions <- function(County, Year, Tons){
#   # calculates ch4 and n2o compost emissions in metric tons co2e. see documentation for factor sources.
#   percent_compost = 1
#   percent_ad = 0 # anaerobic digestion
#   mass_compost = Tons * 0.90718474 * percent_compost
#   mass_ad = Tons * 0.90718474 * percent_ad
#   ef_compost = 10
#   ef_ad = 2
#   recovered_methane = 0
#   
#   emissions_ch4 <- ((mass_compost * ef_compost + mass_ad * ef_ad) * 10^(-3) - recovered_methane) * gwp$ch4
#   
#   ef_compost_n2o = 0.6
#   
#   emissions_n2o <- (mass_compost * ef_compost_n2o * 10^(-3)) *gwp$n2o
#   
#   return(tibble(ch4_emissions = emissions_ch4, n2o_emissions = emissions_n2o, year = Year, geog_name = County, source = "Compost"))
# }

# the real calculations ----

ch4_emissions_factor_compost <- 10 # aggregate emissions factor for aerobic composting, metric tons CH4/thousand metric tons waste, IPCC default
ch4_emissions_factor_ad <- 2 # aggregate emissions factor for anaerobic digestion, metric tons CH4/thousand metric tons waste, IPCC default
# if we were incorporating methane recovered, that would be added as a column to the dataframe

n2o_emissions_factor_compost <- 0.6 # aggregate emissions factor for aerobic composting, metric tons N2O/thousand metric tons waste, IPCC default
# N2O emissions from anaerobic digestion are assumed negligible

compost_factors <- tibble(
  Method = c("Organics", "Organics (Anaerobic Digestion)"),
  ch4 = 10^(-3) * c(ch4_emissions_factor_compost, ch4_emissions_factor_ad),
  n2o = 10^(-3) * c(n2o_emissions_factor_compost, 0)
)

compost_data_organics <- score_data %>% 
  filter(Method == "Organics") %>%
  select(County,
         Year, 
         `Metric Tons`,
         Method)

# anaerobic calculations scaffolding included in case we want to use it later
compost_data_anaerobic <- compost_data_organics %>% 
  # join df with anaerobic percentages
  mutate(`Metric Tons` = `Metric Tons` * 0, # multiply by anaerobic percent
         Method = "Organics (Anaerobic Digestion)") 

compost_data <- compost_data_organics %>% 
  # join df with compost percentages
  mutate(`Metric Tons` = `Metric Tons` * 1) # %>% # multiply by compost percent
  # bind_rows(compost_data_anaerobic)

compost_emissions <- compost_data %>% 
  left_join(compost_factors, by = join_by(Method)) %>% 
  mutate(
    total_ch4 = `Metric Tons` * ch4,
    total_n2o = `Metric Tons` * n2o # decide whether to convert to co2e
  ) %>% 
  select(
    County,
    Method,
    Year,
    total_ch4,
    total_n2o
  )
 
# combined and saved in mn_emissions_all

