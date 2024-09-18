source("R/_load_pkgs.R")
source("_meta/data-raw/county_geography.R")

epa_nei_envirofacts <- readRDS("_transportation/data/epa_nei.RDS")
epa_nei_onroad <- readRDS("_transportation/data-raw/epa/epa_nei_onroad_emissions.RDS")

# how do the NEI summary values from the EnviroFacts API compare with
# the emissions values pulled from the regional summary files?

