source("R/_load_pkgs.R")
source("_transportation/data-raw/_calculate_vmt.R")

county21_data <- readRDS("_transportation/data-raw/analysis_runs/county21_data.RDS")
ctu21_data <- readRDS("_transportation/data-raw/analysis_runs/ctu21_data.RDS")

calculate_vmt(county21_data)
calculate_vmt(ctu21_data)
