# run all electricity and natural gas processing 
# in specific order

# create spatial data with utility areas
source("_energy/data-raw/minnesota_natGasUtilities.R")
source("_energy/data-raw/minnesota_electricUtilities.R")

source("_energy/data-raw/wisconsin_natGasUtilities.R")
source("_energy/data-raw/wisconsin_electricUtilities.R")

# next, run processing for MN and WI activity data

source("_energy/data-raw/minnesota_natGas_ActivityAndEmissions.R")
source("_energy/data-raw/wisconsin_natGas_ActivityAndEmissions.R")

# finalize processing 
source("_energy/data-raw/processed_mn_electricUtil_activityData.R")
source("_energy/data-raw/processed_wi_electricUtil_activityData.R")

# Done!
