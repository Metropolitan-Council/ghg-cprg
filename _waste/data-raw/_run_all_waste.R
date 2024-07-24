# wastewater ----

source("_waste/data-raw/wastewater/epa_state_wastewater.R")
# source("_waste/data-raw/wastewater/metc_wastewater.R")
# source("_waste/data-raw/wastewater/_wastewater.R")


# solid waste ----

# 2021 ----
# source("_waste/data-raw/2021_mpca_score.R")
# source("_waste/data-raw/2021_combine_waste_data.R")

# 2005-2021 ----

## MN ----
### source data ----
source("_waste/data-raw/mn_read_score_data.R") # MPCA SCORE activity data
# generates mpca_score.RDS
source("_waste/data-raw/clean_tabula_tables.R") # MPCA waste composition data (2013)
# generates mn_waste_composition.RDS
source("_waste/data-raw/mn_methane_flaring.R") # EPA methane recovery data (not used)
# generates methane_recovery_mn.RDS

### emissions calculations ----
source("_waste/data-raw/mn_methane_commitment_model.R") # landfill
# generates mn_landfill_emissions.RDS
source("_waste/data-raw/mn_compost_emissions.R") # compost
# generates mn_compost_emissions.RDS
source("_waste/data-raw/mn_incineration_emissions.R") # incineration
# generates mn_incineration_emissions.RDS

### combine emissions ----
source("_waste/data-raw/mn_emissions_all.R") 
# create mn_sw_emissions_by_gas.RDS and mn_sw_emissions_co2e.RDS

## WI ----
source("_waste/data-raw/wi_data.R") # run all WI emissions processing
# create wi_emissions.RDS

## MN and WI combined ----
source("_waste/data-raw/combine_waste_data.R")
