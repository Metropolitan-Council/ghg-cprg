# Process Dakota Electrics data
source("R/_load_pkgs.R")

# Read city data
city_raw <- read_xlsx(here("_energy", "data-raw", "dakotaElectricDataRequest", "METC_DEA_Usage_Breakdown.xlsx"))
