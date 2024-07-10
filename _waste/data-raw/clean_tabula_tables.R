# This script reads in and cleans MPCA waste breakdown data that has been extracted from pdf by tabula.
source("R/_load_pkgs.R")

# Waste Breakdown ----
## From https://www.pca.state.mn.us/sites/default/files/w-sw1-60.pdf, a 2013 MPCA report on the composition of Municipal Solid Waste.

waste_breakdown <- read_csv(file.path(here::here(), "_waste/data-raw/tabula-waste-characterization-table.csv"))

wb_list <- vector(mode = "list", length = 8)

wb_list[[1]] <- slice(waste_breakdown, 2:11) %>%
  mutate(Category = "Paper")
wb_list[[2]] <- slice(waste_breakdown, 15:26) %>%
  mutate(Category = "Plastic")
wb_list[[3]] <- slice(waste_breakdown, 30:35) %>%
  mutate(Category = "Household Waste")
wb_list[[4]] <- slice(waste_breakdown, 39:42) %>%
  mutate(Category = "Metal")
wb_list[[5]] <- slice(waste_breakdown, 46:48) %>%
  mutate(Category = "Glass")
wb_list[[6]] <- slice(waste_breakdown, 52:56) %>%
  mutate(Category = "Electronics")
wb_list[[7]] <- slice(waste_breakdown, 60:63) %>%
  mutate(Category = case_when(
    Material == "Food Waste" ~ "Organics (Food Waste)",
    Material == "Wood" ~ "Wood",
    TRUE ~ "Organics (Non-Food)"
  ))
wb_list[[8]] <- slice(waste_breakdown, 67:72) %>%
  mutate(Category = case_when(
    (Material == "Textiles & Leather" | Material == "Carpet") ~ "Textiles",
    TRUE ~ "Other"
  ))

waste_breakdown_bind <- bind_rows(wb_list) %>%
  mutate(across(c(Mean, Lower, Upper), ~ as.numeric(gsub("%", "", ., fixed = TRUE)) / 100))

waste_comp_meta <- tribble(
  ~Column, ~Class, ~Description,
  "Material", class(waste_breakdown_bind$Material), "Material category",
  "Mean", class(waste_breakdown_bind$Mean), "Mean fraction of solid waste contributed by this material",
  "Lower", class(waste_breakdown_bind$Lower), "Lower 90% cofidence interval fraction of solid waste",
  "Upper", class(waste_breakdown_bind$Upper), "Upper 90% cofidence interval fraction of solid waste",
  "Category", class(waste_breakdown_bind$Category), "Category of waste materials type"
)

saveRDS(waste_breakdown_bind, paste0("_waste/data/mn_waste_composition.RDS"))
saveRDS(waste_comp_meta, paste0("_waste/data/mn_waste_composition_meta.RDS"))
