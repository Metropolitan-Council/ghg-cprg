source("R/_load_pkgs.R")
source("_transportation/data-raw/_calculate_vmt.R")

county21_data <- readRDS("_transportation/data-raw/analysis_runs/county21_data.RDS")
# ctu21_data <- readRDS("_transportation/data-raw/analysis_runs/ctu21_data.RDS")
county21_truck <- readRDS("_transportation/data-raw/analysis_runs/county21_truck_sfcalib_data.rds")

vehicle_miles <- bind_rows(calculate_vmt(county21_data, class = "passenger"),
                           calculate_vmt(county21_truck, class = "commercial"))

