#### Create CTU temporary inventory graphs for Dec 10 Climate Summit

source("R/_load_pkgs.R")

### directory to save ggplot items in

wd <- "C:/Users/WilfahPA/OneDrive - Metropolitan Council/CPRG/Steering committee graphics/November meeting/"

### county graphs

cprg_colors <- source("R/cprg_colors.R")

ctu_emissions <- readRDS("_meta/data/ctu_emissions.RDS")
