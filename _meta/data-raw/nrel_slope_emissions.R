# Read in 2020 city-level emissions data from NREL SLOPE tool
source("R/_load_pkgs.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_city <- readRDS("_meta/data/cprg_ctu.RDS")
# https://maps.nrel.gov/slope/
# Zotero key @murphyStateLocalPlanning2024

# City-level GHG emissions for the year 2020 are estimated by sector and energy
# type and reported in units of carbon dioxide equivalent (CO₂e) based on the
# 100-year time horizon global warming potential of other gases relative to (CO₂).

# Estimates leveraged multiple detailed, open-source, sector-specific models and
# datasets, which estimate current (and recent) energy consumption at county-level
# resolution, including ResStock, ComStock, the 2018 Industrial Energy Databook,
# the 2020 National Emissions Inventory, and the City and County Energy Profiles.


# download city level emissions -----
download.file("https://gds-files.nrel.gov/slope/ghg_emissions_baseline.zip",
  destfile = "_meta/data-raw/nrel_slope/emissions/ghg_emissions_baseline.zip"
)
# unzip
unzip("_meta/data-raw/nrel_slope/emissions/ghg_emissions_baseline.zip",
  exdir = "_meta/data-raw/nrel_slope/emissions/"
)

# read in CSV
raw_city_emissions <- read.csv("_meta/data-raw/nrel_slope/emissions/ghg_emissions_baseline_us_places_2020.csv") %>%
  clean_names()

# filtering and cleaning -----
city_emissions <- raw_city_emissions %>%
  mutate(
    # extract state name
    state_name = stringr::str_sub(city_name, start = -2, end = -1),
    # clean city name
    city = stringr::str_remove_all(city_name, " city, MN") %>%
      stringr::str_remove(" city, WI") %>%
      stringr::str_remove(" village, WI")
  ) %>%
  # filter to our states, our cities
  filter(
    state_name %in% c("MN", "WI"),
    city %in% cprg_city$CTU_NAME
  )


city_emissions
