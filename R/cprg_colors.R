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
sector_colors <- list(
  "Transportation" = "#8e440b",
  "Energy" = "#163c6a",
  "Waste" = "#8d0c3b",
  "Agriculture" = "#fec44f",
  "Nature" = "#367639"
)

# category
category_colors <-
  list(
    # GnBu
    "Residential energy" = "#225ea8",
    "Commercial energy" = "#41b6c4",
    "Industrial energy" = "#7fcdbb",
    "Liquid stationary fuels" = "#a8ddb5",


    # YlOrBr
    "Passenger vehicles" = "#6E260E",
    "Commercial vehicles" = "#C19A6B",

    # PuRd
    "Solid waste" = "#ce1256",
    "Wastewater" = "#c994c7",

    # YlOrBr
    "Livestock" = "#ec7014",
    "Cropland" = "#fec44f",
    
    # Gn
    "Sequestration" = "#006f3c",
    "Stock" = "#27b376"
  )


# source
source_colors <- list(
  # transportation levels
  "Light-duty vehicles" = "#6E260E",
  "Medium-duty vehicles" = "#C19A6B",
  "Heavy-duty vehicles" = "#E5AA70",

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

  # agriculture
  "Enteric fermentation"  = "#D27D2D",
  "Manure management"  = "#F28C28",
  "Direct manure soil emissions" = "#DAA520",
  "Indirect manure runoff emissions" = "#E49B0F",
  "Soil residue emissions" = "#FFD700",
  "Onsite fertilizer emissions" = "#FAFA33",
  "Runoff fertilizer emissions" = "#FFFF8F",
  
  # nature
  "Tree" = "#006d2c",
  "Grassland" = "#22b600",
  "Wetland" = "#26cc00",
  "Urban tree" = "#7be382",
  "Urban grassland" = "#a1d99b"
)


vehicle_weight_colors <- list(
  "Passenger" = source_colors$`Light-duty vehicles`,
  "Medium" = source_colors$`Medium-duty vehicles`,
  "Heavy" = source_colors$`Heavy-duty vehicles`,
  "Light-duty" = source_colors$`Light-duty vehicles`,
  "Medium-duty" = source_colors$`Medium-duty vehicles`,
  "Heavy-duty" = source_colors$`Heavy-duty vehicles`
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
