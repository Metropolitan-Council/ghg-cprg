# colors from CPRG logo
# https://www.epa.gov/system/files/styles/small/private/images/2023-09/cprg-flower-logo-full-horiz_0.png?itok=bcB6G-jw

cprg_colors <- list(
  "cprg_green" = "#4a9c2d",
  "cprg_da_green" = "#367639",
  "epa_blue" = "#6994c1",
  "cprg_yellow" = "#ecb81c",
  "cprg_da_yellow" = "#faa819"
)

# sector colors
sector_colors <- list("Electricity" = "#1f77b4", 
                      "Transportation" = "#191970", 
                      "Residential" = "#9467bd", 
                      "Commercial" = "#d62728", 
                      "Industrial" = "slategray",
                      "Waste" = "#8c564b",
                      "Agriculture" = "#8fb910", 
                      "Natural Systems" = "#006f3c")


# category
category_colors <-
  list(
    
    #Pu
    "Residential natural gas" = "#483248",
    "Residential electricity" = "#CF9FFF",
    
    "Commercial natural gas" = "#800000",
    "Commercial fuel combustion" = "#DE3163",
    "Commercial electricity" = "#FAA0A0",
    
    #Gray
    "Industrial electricity" = "#E5E4E2",
    "Industrial natural gas" = "#36454F",
    "Industrial processes" = "#B2BEB5",
    "Industrial fuel combustion" = "#818589",
    "Refinery processes" = "#708090",
    
    # YlOrBr
    "Passenger vehicles" = "#0047AB",
    "Buses" = "#6F8FAF",
    "Trucks" = "#6495ED",
    "Aviation" = "#191970",
    
    # Rd
    "Solid waste" = "#8B4513",
    "Wastewater" = "#CD853F",
    
    # Green
    "Cropland" = "#c7e960",
    "Livestock" = "#8fb910",
    
    # Gn
    "Urban greenery" = "#006f3c",
    "Natural systems" = "#27b376"
  )

# source
source_colors <- list(
  # transportation levels
  # "Light-duty vehicles" = "#993404",
  # we still need these colors for DOT VMT docs
  "Medium-duty vehicles" = "#fe9929",
  "Heavy-duty vehicles" = "#fee391",
  "Gasoline fueled vehicles" = "#db4a33",
  "Diesel fueled vehicles" = "#feb24c",
  "Other fueled vehicles" = "#ffeda0",



  # waste levels
  "Landfill" = "#ae017e",
  "Recycling" = "#ed2c73",
  "Organics" = "#fcc5c0",
  "Wastewater" = "#994d96",
  "Waste to energy" = "#df65b0",

  # energy levels
  "Electricity" = "#0868ac",
  "Natural gas" = "#4eb3d3",
  "Propane" = "#2f4f4f",
  "Kerosene" = "#004242",

  # nature
  "Tree" = "#006d2c",
  "Grassland" = "#22b600",
  "Wetland" = "#26cc00",
  "Urban tree" = "#7be382",
  "Urban grassland" = "#a1d99b"
)


vehicle_weight_colors <- list(
  "Passenger" = category_colors$`Passenger vehicles`,
  "Buses" = category_colors$Buses,
  "Trucks" = category_colors$Trucks,
  # we still need these colors for DOT VMT docs
  "Medium-duty" = source_colors$`Medium-duty vehicles`,
  "Heavy-duty" = source_colors$`Heavy-duty vehicles`
)

fuel_type_colors <- list(
  "Gasoline" = "#ffc300",
  "Diesel" = "#9f7a00",
  "Other" = "#604900"
)

management_method_colors <- list(
  "WTE" = source_colors$`Waste to energy`,
  "Waste to energy" = source_colors$`Waste to energy`,
  "Recycling" = source_colors$Recycling,
  "Organics" = source_colors$Organics,
  "Landfill" = source_colors$Landfill,
  "Onsite" = "#750a31"
)

# european space agency colors
esa_color <- c(
  "forestgreen", "brown4", "yellow2", "wheat3", "red",
  "plum", "lightblue", "navy", "mediumturquoise"
)
