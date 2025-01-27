source("R/_load_pkgs.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")



# download county file from NREL directly
download.file("https://gds-files.nrel.gov/slope/energy_consumption_expenditure_business_as_usual.zip",
  destfile = "_energy/data-raw/nrel_slope/energy_consumption_expenditure_business_as_usual_county.zip"
)
unzip("_energy/data-raw/nrel_slope/energy_consumption_expenditure_business_as_usual.zip",
  exdir = "_energy/data-raw/nrel_slope/"
)



nrelSlope_countyForecasts <- read.csv(here(
  "_energy",
  "data-raw",
  "nrel_slope",
  "energy_consumption_expenditure_business_as_usual_county.csv"
)) %>%
  filter(County.Name %in% cprg_county$NAME &
    State.Name %in% c("Minnesota", "Wisconsin") &
    Year > 2024)


# filters
nrelSlope_cityForecasts <- read.csv(here(
  "_energy",
  "data-raw",
  "nrel_slope",
  "energy_consumption_expenditure_business_as_usual_city.csv"
)) %>%
  filter(State.Name %in% c("Minnesota", "Wisconsin") &
    Year > 2024)
