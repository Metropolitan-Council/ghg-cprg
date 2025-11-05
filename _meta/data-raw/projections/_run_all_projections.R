## This script will run all of the projections scripts for the CCAP
## (all functions currently live in ~/_meta/data-raw/projections/)

# setup ----
source("_meta/data-raw/projections/01_compile_taz_demographic_forecast.R") # compile demographic data
source("_meta/data-raw/projections/01_projections_plotter.R") # function for plotting


source("_meta/data-raw/projections/02_compile_mpca_gcam.R") # compile gcam data
source("_meta/data-raw/projections/02_compile_mpca_ghg_inventory_2022.R") # compile mpca ghg data for 2022
source("_meta/data-raw/projections/02_compile_regional_housing.R") # compile housing data

# natural systems ----
source("_meta/data-raw/projections/03_forecast_natural_systems_seq.R")

# residential emissions ----
#source("_meta/data-raw/projections/03_compile_residential_emissions.R") # slow

source("_meta/data-raw/projections/04_forecast_residential_emissions.R")

# nonresidential emissions ----
source("_meta/data-raw/projections/04_forecast_nonresidential_emissions.R")

# agriculture ----
source("_meta/data-raw/projections/04_forecast_agriculture_emissions.R")

# industrial ----
source("_meta/data-raw/projections/04_forecast_industrial_emissions.R")

# transportation ----
source("_meta/data-raw/projections/04_forecast_transportation_emissions.R")

# waste ----
source("_meta/data-raw/projections/04_forecast_waste_emissions.R")

# electricity ----
source("_meta/data-raw/projections/05_forecast_electricity_emissions.R")

rm(list = ls())

# compile all sectors ----
source("_meta/data-raw/projections/06_compile_economy_bau_emissions.R")
source("_meta/data-raw/projections/06_compile_economy_ppp_emissions.R")
