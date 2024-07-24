# calculate emissions from aerobic composting using IPCC equations and MPCA data.
source("R/_load_pkgs.R")
if (!exists("score_data")) {
  score_data <- readRDS("_waste/data/mpca_score.RDS")
}

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
  select(
    County,
    Year,
    `Metric Tons`,
    Method
  )

# anaerobic calculations scaffolding included in case we want to use it later
compost_data_anaerobic <- compost_data_organics %>%
  # join df with anaerobic percentages
  mutate(
    `Metric Tons` = `Metric Tons` * 0, # multiply by anaerobic percent
    Method = "Organics (Anaerobic Digestion)"
  )

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

# write meta
compost_emissions_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "County", class(compost_emissions$County), "Name of county",
    "Method", class(compost_emissions$Method), "Subcategory-specific source (e.g., Landfill)",
    "Year", class(compost_emissions$Year), "Emissions estimation year",
    "total_ch4", class(compost_emissions$total_ch4), 
    "Annual total emissions, in metric tons CH~4~, attributed to the given county",
    "total_n2o", class(compost_emissions$total_n2o), 
    "Annual total emissions, in metric tons N~2~O, attributed to the given county"
  )

# save RDS
saveRDS(compost_emissions, "_waste/data/mn_compost_emissions.RDS")
saveRDS(compost_emissions_meta, "_waste/data/mn_compost_emissions_meta.RDS")

