source("R/_load_pkgs.R")
score_data <- readRDS("_waste/data/mpca_score.RDS")

# landfill
source("_waste/data-raw/mn_methane_commitment_model.R")

# incineration NOTE WTE GOES TO ENERGY SECTOR
source("_waste/data-raw/mn_incineration_emissions.R")

#compost
source("_waste/data-raw/mn_compost_emissions.R")

# using landfill_emissions, compost_emissions, onsite_emissions
