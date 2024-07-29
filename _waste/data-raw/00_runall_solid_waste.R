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
source("_waste/data-raw/01_mpca_score_allyrs.R") # MPCA SCORE activity data
# RDS lives in data-raw
source("_waste/data-raw/01_mpca_waste_characterization.R") # MPCA waste composition data (2013)
# RDS lives in data-raw
source("_waste/data-raw/01_epa_mn_methane_recovery.R") # EPA methane recovery data (not used)
# RDS lives in data-raw

### emissions calculations ----
source("_waste/data-raw/02_compile_landfill_MN_allyrs.R") # landfill
# generates landfill_MN_allyrs.RDS
source("_waste/data-raw/02_compile_organics_MN_allyrs.R") # compost
# generates organics_MN_allyrs.RDS
source("_waste/data-raw/02_compile_incineration_MN_allyrs.R") # incineration
# generates incineration_MN_allyrs.RDS

### combine emissions ----
source("_waste/data-raw/mn_emissions_all.R") 
# create mn_sw_emissions_by_gas.RDS and mn_sw_emissions_co2e.RDS

## WI ----
source("_waste/data-raw/wi_data.R") # run all WI emissions processing
# create wi_emissions.RDS

## MN and WI combined ----
source("_waste/data-raw/combine_waste_data.R")
# create county_sw_emissions.RDS
