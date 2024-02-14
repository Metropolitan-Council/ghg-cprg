source("R/_load_pkgs.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")

### MPCA provides waste water emission estimates
mn_mpca <- readxl::read_xlsx("_waste/data-raw/wastewater/mpca-mn-wastewater.xlsx")

mpca_state_ghg <- mn_mpca %>%
  pivot_longer(7:37,
    names_to = "year"
  ) %>%
  filter(year == 2020) %>%
  select(GHGs, year, value)

mn_state_est <- as.numeric(mn_mpca[mn_mpca$Sector == "Grand Total", "2020"])
