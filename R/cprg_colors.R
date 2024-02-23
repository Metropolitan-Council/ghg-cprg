# colors from CPRG logo
# https://www.epa.gov/system/files/styles/small/private/images/2023-09/cprg-flower-logo-full-horiz_0.png?itok=bcB6G-jw

readRDS("_meta/data/sector_category.RDS")

cprg_colors <- list(
  "cprg_green" = "#4a9c2d",
  "cprg_da_green" = "#367639",
  "epa_blue" = "#6994c1",
  "cprg_yellow" = "#ecb81c",
  "cprg_da_yellow" = "#faa819"
)


county_emissions %>% 
  select(sector, source, category) %>% 
  unique()

county_emissions$category %>% unique %>% dput
# category
c("Passenger", 
  "Commercial", 
  "Propane",
  "Kerosene",
  "Electricity", 
  "Natural gas",
  "Wastewater",
  "Solid waste")
county_emissions$source %>% unique %>% dput
# source
c(
  # transportation levels
  "Light-duty vehicles",
  "Medium-duty vehicles",
  "Heavy-duty vehicles",
  # waste levels
  "Landfill",
  "Recycling",
  "Organics",
  "Wastewater",
  # energy levels
  "Electricity",
  "Natural Gas",
  "Propane",
  "Kerosene"
)
