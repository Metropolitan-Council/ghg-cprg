# run all energy processing
# in specific order

# kerosene and propane
source("_energy/data-raw/propane-estimate-2021.R")
source("_energy/data-raw/kerosene-estimate-2021.R")
source("_energy/data-raw/fuel-estimate-2021.R")

# create spatial data with utility areas
source("_energy/data-raw/minnesota_natGasUtilities.R")
source("_energy/data-raw/minnesota_electricUtilities.R")

source("_energy/data-raw/wisconsin_natGasUtilities.R")
source("_energy/data-raw/wisconsin_electricUtilities.R")

# next, run processing for MN and WI county-level activity data (nat gas, then elec), derived from utility reports to State
source("_energy/data-raw/minnesota_7610reporting_natGas.R")
source("_energy/data-raw/wisconsin_natGas_estimate_2005_and_2021.R")

source("_energy/data-raw/minnesota_7610reporting_electricity.R")
source("_energy/data-raw/wisconsin_elec_estimate_2021.R")

source("_energy/data-raw/nrel_slope_energy.R")

# city-level activity data processing
source("_energy/data-raw/minnesota_xcelCommunityReports_electricity.R")


# Done!
