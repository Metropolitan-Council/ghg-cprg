#### Script to read in and process EPA GHG FLIGHT data
source("R/_load_pkgs.R")

cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_ctu <- readRDS("_meta/data/cprg_ctu.RDS")

### download flight data: https://ghgdata.epa.gov/ghgp/main.do
ind_unit_data <-  read_excel(file.path(here::here(), "_industrial/data-raw/emissions_by_unit_and_fuel_type_c_d_aa_09_2023.xlsx"),
                                       sheet = "UNIT_DATA",
                             skip = 6) %>% 
  filter(State %in% c("MN","WI")) %>% 
  clean_names()

mn_flight <- lapply(as.character(2010:2022), function(year) {
  read_excel(file.path(here::here(), "_industrial/data-raw/mn_flight.xls"), 
             sheet = year,
             skip = 5)
}) %>% 
  bind_rows() %>% 
  clean_names()
