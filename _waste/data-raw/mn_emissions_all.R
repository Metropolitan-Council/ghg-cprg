# source("R/_load_pkgs.R")
# score_data <- readRDS("_waste/data/mpca_score.RDS") #called in individual scripts
# add test so RDS is not read repeatedly

# read in SCORE data, convert to metric tons, save as RDS
source("_waste/data-raw/mn_read_score_data.R")

# read in flaring data, save as RDS
source("_waste/data-raw/mn_methane_flaring.R")

# clean tables with mn waste composition data
source("_waste/data-raw/clean_tabula_tables.R")

# landfill
source("_waste/data-raw/mn_methane_commitment_model.R")

# incineration NOTE WTE GOES TO ENERGY SECTOR
source("_waste/data-raw/mn_incineration_emissions.R")

#compost
source("_waste/data-raw/mn_compost_emissions.R")

# using landfill_emissions, compost_emissions, onsite_emissions
