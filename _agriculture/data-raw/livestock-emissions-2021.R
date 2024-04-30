source("R/_load_pkgs.R")
source("R/global_warming_potential.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")

enteric <- read_csv('_agriculture/data-raw/enteric-fermentation.csv')
manure_n2o <- read_csv('_agriculture/data-raw/manure-n2o.csv')
manure_ch4 <- read_csv('_agriculture/data-raw/manure-ch4.csv')
mn_feedlots <- read_csv('_agriculture/data-raw/mn-feedlots.csv')
