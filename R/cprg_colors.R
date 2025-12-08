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
  "Electricity" = "#FD8D3C",
  "Transportation" = "#60C8E9",
  "Residential" = "#9467bd",
  "Commercial" = "#DB5755",
  "Industrial" = "#A42264",
  "Industrial Processes" = "#A42264",
  "Waste" = "#77645A",
  "Agriculture" = "#A5CF4C",
  "Natural Systems" = "#1C9099"
)

## with building fuel
sector_colors_alt <- list(
  "Electricity" = "#FD8D3C",
  "Transportation" = "#60C8E9",
  "Building Fuel" = "#BA89C2",
  "Industrial" = "#A42264",
  "Industrial Processes" = "#A42264",
  "Waste" = "#ABB6BE",
  "Agriculture" = "#A5CF4C",
  "Natural Systems" = "#1C9099"
)


# category
category_colors <-
  list(

    # Electricity
    "Residential electricity" = "#E6550D",
    "Commercial electricity" = "#FDAE6B",
    "Industrial electricity" = "#FDD0A2",

    # Residential
    "Residential natural gas" = "#483248",
    # "Residential electricity" = "#CF9FFF",
    "Residential liquid fuel" = "#A2627A",

    # Commercial
    "Commercial natural gas" = "#800000",
    "Commercial fuel combustion" = "#DE3163",

    # Building Fuel
    "Residential building fuel" = "#8856A7",
    "Commercial building fuel" = "#8C96C6",
    "Industrial building fuel" = "#BFD3E6",

    # Industrial


    "Industrial natural gas" = "#980043",
    "Industrial processes" = "#DF65B0",
    "Industrial fuel combustion" = "#FA9FB5",
    "Refinery processes" = "#FEEBE2",

    # Transportation
    "Passenger vehicles" = "#D9F0F7",
    "Buses" = "#191970",
    "Trucks" = "#3182BD",
    "Aviation" = "#9ECAE1",

    # Waste
    "Solid waste" = "#77645A",
    "Wastewater" = "#C7B9AF",

    # Agriculture
    "Cropland" = "#31A354",
    "Livestock" = "#ADDD8E",

    # Natural systems
    "Sequestration" = "#CCEBC5",
    "Freshwater" = "#7BCCC4"
  )


# source
source_colors <- list(
  # transportation levels
  # "Light-duty vehicles" = "#993404",
  # we still need these colors for DOT VMT docs
  # Transportation 1
  "On-road" = "#6F8FAF",
  "Off-road" = "#191970",
  "MSP airport" = "#9696e8",
  "Reliever airport" = "#d5d5f6",

  # Electricity
  "Residential electricity" = "#ffc300",
  "Commercial electricity" = "#9f7a00",
  "Industrial electricity" = "#604900",

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
  "Enteric fermentation" = "#ffcc00", # Distinct yellow
  "Manure management" = "#b5c100",
  "Direct manure soil emissions" = "#a0b600",
  "Indirect manure runoff emissions" = "#8da000",
  "Soil residue emissions" = "#7a8a00",
  "Onsite fertilizer emissions" = "#b0d400",
  "Runoff fertilizer emissions" = "#9ccf00",

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

waterway_colors <- c(
  "LakePond" = "steelblue",
  "StreamRiver" = "lightskyblue",
  "Reservoir" = "maroon3",
  "SwampMarsh" = "palegreen3",
  "Lock Chamber" = "purple3",
  "DamWeir" = "darkslategray4",
  "Connector" = "red",
  "CanalDitch" = "goldenrod",
  "Underground Conduit" = "darkorchid1",
  "Pipeline" = "darkslategray",
  "ArtificialPath" = "green"
)
