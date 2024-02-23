# colors from CPRG logo
# https://www.epa.gov/system/files/styles/small/private/images/2023-09/cprg-flower-logo-full-horiz_0.png?itok=bcB6G-jw

cprg_colors <- list(
  "cprg_green" = "#4a9c2d",
  "cprg_da_green" = "#367639",
  "epa_blue" = "#6994c1",
  "cprg_yellow" = "#ecb81c",
  "cprg_da_yellow" = "#faa819"
)


# county_emissions %>% 
#   select(sector, source, category) %>% 
#   unique() %>% 
#   arrange(category)

# county_emissions$category %>% unique %>% dput
# category
category_colors <- 
  list(
    # GnBu
    "Residential energy" = "#225ea8",
    "Commercial energy" = "#41b6c4",
    "Industrial energy" = "#7fcdbb",
    "Liquid stationary fuels" = "#a8ddb5", 
    
    
    # YlOrBr
    "Passenger vehicles" = "#ec7014", 
    "Commercial vehicles" = "#fec44f", 
    
    # PuRd
    "Wastewater" = "#ce1256",
    "Solid waste" = "#c994c7")


# county_emissions$source %>% unique %>% dput
# source
source_colors <- list(
  # transportation levels
  "Light-duty vehicles" = "#993404",
  "Medium-duty vehicles" = "#fe9929",
  "Heavy-duty vehicles" = "#fee391",
  
  # waste levels
  "Landfill" = "#f58ab1",
  "Recycling" = "#ed2c73",
  "Organics" = "#ce1256",
  
  "Wastewater" = "#994d96",
  
  # energy levels
  "Electricity" = "#0868ac",
  "Natural gas" = "#4eb3d3",
  
  "Propane" = "#006d2c",
  "Kerosene" = "#a1d99b"
)


vehicle_weight_colors <- list(
  "Passenger" = source_colors$`Light-duty vehicles`,
  "Medium" = source_colors$`Medium-duty vehicles`,
  "Heavy" = source_colors$`Heavy-duty vehicles`,
  
  "Light-duty" = source_colors$`Light-duty vehicles`,
  "Medium-duty" = source_colors$`Medium-duty vehicles`,
  "Heavy-duty" = source_colors$`Heavy-duty vehicles`
)
