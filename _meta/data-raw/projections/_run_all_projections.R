## This script will run all of the projections scripts for the CCAP
## (all functions currently live in ~/_meta/data-raw/projections/)


# setup ----
source("_meta/data-raw/projections/01_compile_mpca_gcam.R") # compile gcam data
source("_meta/data-raw/projections/01_compile_regional_housing.R") 


# residential emissions ----
source("_meta/data-raw/projections/02_compile_residential_emissions.R")
source("_meta/data-raw/projections/03_forecast_residential_emissions.R")

# nonresidential emissions ----
source("_meta/data-raw/projections/03_forecast_nonresidential_emissions.R")


# agriculture ----
source("_meta/data-raw/projections/03_forecast_agriculture_emissions.R")




# housing ----

# industrial ----
source("_meta/data-raw/projections/03_forecast_industrial_emissions.R")

# transportation ----
source("_meta/data-raw/projections/03_forecast_transportation_emissions.R")

# waste ----
source("_meta/data-raw/projections/03_forecast_waste_emissions.R")

# electricity ----
source("_meta/data-raw/projections/04_forecast_electricity_emissions.R")





# natural systems ----





