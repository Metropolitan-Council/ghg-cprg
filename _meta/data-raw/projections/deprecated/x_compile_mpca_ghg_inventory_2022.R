source("R/_load_pkgs.R")


mpca_inv <- read_csv("_meta/data-raw/mpca_inventory_2022.csv") %>%
  pivot_longer(cols = -c(1:5), names_to = "inventory_year", values_to = "co2e") %>%
  rename(Subsector = `Source group`)

saveRDS(mpca_inv, "_meta/data/mpca_ghg_inv_2022.RDS")
