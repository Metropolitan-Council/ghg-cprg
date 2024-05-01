source("R/_load_pkgs.R")
source("R/global_warming_potential.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
install.packages('FedData')
install.packages('tidyUSDA')

library(FedData)
library(tidyUSDA)

# set USDA API key
key <- keyring::key_get('')
