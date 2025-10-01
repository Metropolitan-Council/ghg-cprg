source("R/_load_pkgs.R")


mpca_inv <- read_csv("_meta/data-raw/mpca_inventory_2022.csv") %>%
  pivot_longer(cols = -c(1:5), names_to = "inventory_year", values_to = "co2e") %>%
  rename(Subsector = `Source group`)

# waldo::compare(mpca_inv, readRDS("_meta/data/mpca_ghg_inv_2022.RDS"))
message("Saving MPCA greenhouse gas inventory data (2022) to: \n\t _meta/data/mpca_ghg_inv_2022.RDS")
saveRDS(mpca_inv, "_meta/data/mpca_ghg_inv_2022.RDS")
