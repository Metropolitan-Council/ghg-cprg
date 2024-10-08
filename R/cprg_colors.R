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
  "Waste" = "#8d0c3b"
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
    "Passenger vehicles" = "#993404",
    "Medium-duty vehicles" = "#fe9929",
    "Heavy-duty vehicles" = "#fee391",
    "Transit vehicles" = "#E9967A",
    "Other vehicles" = "#eaeded",

    # PuRd
    "Solid waste" = "#ce1256",
    "Wastewater" = "#c994c7",

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
  "Medium-duty" = category_colors$`Medium-duty vehicles`,
  "Heavy-duty" = category_colors$`Heavy-duty vehicles`,
  "Transit" = category_colors$`Transit vehicles`,
  "Other" = category_colors$`Other vehicles`
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
