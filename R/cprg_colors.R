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
                       "Transportation" = "#8c564b", 
                       "Building energy" = "#9467bd", 
                       "Industrial" = "slategray",
                       "Waste" = "#d62728", 
                       "Agriculture" = "#ff7f0e", 
                       "Nature" = "#2ca02c")


# category
category_colors <-
  list(
    # GnBu
    "Residential electricity" = "#225ea8",
    "Commercial electricity" = "#41b6c4",
    "Industrial electricity" = "#7fcdbb",
    
    #Pu
    "Residential natural gas" = "#fb7cf5",
    "Commercial natural gas" = "#aa55a6",
    "Industrial natural gas" = "#780e73",
    "Liquid stationary fuels" = "#480245",

    #Gray
    "Fuel combustion" = "#7e7e7e",
    "Process" = "#504b4b",
    "Other" = "#d2c5c5",

    # YlOrBr
    "Passenger vehicles" = "#993404",
    "Buses" = "#E9967A",
    "Trucks" = "#fe9929",
    "Aviation" = "#fd6d31",

    # Rd
    "Solid waste" = "#a80d0d",
    "Wastewater" = "#f24b4b",

    # Green
    "Cropland" = "#c7e960",
    "Livestock" = "#8fb910",
    
    # Gn
    "Sequestration" = "#006f3c",
    "Stock" = "#27b376"
  )


# source
source_colors <- list(
  # transportation levels
  # "Light-duty vehicles" = "#993404",
  # "Medium-duty vehicles" = "#fe9929",
  # "Heavy-duty vehicles" = "#fee391",
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
  "Trucks" = category_colors$Trucks
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
