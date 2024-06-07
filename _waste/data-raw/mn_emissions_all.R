# source("R/_load_pkgs.R")
# score_data <- readRDS("_waste/data/mpca_score.RDS") #called in individual scripts
if (!exists("gwp")) {
  source("R/global_warming_potential.R")
}

# read in SCORE data, convert to metric tons, save as RDS
source("_waste/data-raw/mn_read_score_data.R")

# read in methane recovery data, save as RDS (UNFINISHED)
source("_waste/data-raw/mn_methane_flaring.R")

# clean tables with mn waste composition data, save as RDS
source("_waste/data-raw/clean_tabula_tables.R")

# calculate landfill emissions (UNFINISHED), return landfill_emissions df
source("_waste/data-raw/mn_methane_commitment_model.R")

# calculate incineration emissions, return incineration_emissions df
# note that WTE must be listed as energy sector
source("_waste/data-raw/mn_incineration_emissions.R")

# calculate compost emissions, return compost_emissions df
source("_waste/data-raw/mn_compost_emissions.R")

solid_waste_emissions_metric_tons <- landfill_emissions %>% 
  bind_rows(incineration_emissions, compost_emissions) %>% 
  replace(is.na(.), 0)

# write meta
# save RDS

solid_waste_emissions_co2e <- solid_waste_emissions_metric_tons %>% 
  mutate(
    ch4_emissions_metric_tons_co2e = emissions_metric_tons_ch4 * gwp$ch4,
    n2o_emissions_metric_tons_co2e = emissions_metric_tons_n2o * gwp$n2o,
    sector = case_when(
      Method == "WTE" ~ "Energy",
      TRUE ~ "Waste"
    ),
    category = case_when(
      Method == "WTE" ~ "Waste to energy",
      TRUE ~ "Solid waste"
    ),
    source = case_when(
      Method == "WTE" ~ "Waste to energy",
      TRUE ~ Method
    )
  ) %>% 
  mutate(
    emissions_metric_tons_co2e = ch4_emissions_metric_tons_co2e + n2o_emissions_metric_tons_co2e + emissions_metric_tons_co2
  ) %>% 
  select(
    year = Year,
    geog_name = County,
    sector,
    category,
    source,
    emissions_metric_tons_co2e
  )

# write meta
# save RDS

