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

  "Electricity" = "#ecb81c",
  "Transportation" = "#191970",
  "Residential" = "#9467bd",
  "Commercial" = "#DB5755",
  "Industrial" = "slategray",
  "Waste" = "#D47E30",
  "Agriculture" = "#8fb910",
  "Natural Systems" = "#006f3c"
)

## with building fuel
sector_colors_alt <- list(

  "Electricity" = "#ecb81c",
  "Transportation" = "#191970",
  "Building Fuel" = "#DB5755",
  "Industrial Processes" = "slategray",
  "Waste" = "#D47E30",
  "Agriculture" = "#8fb910",
  "Natural Systems" = "#006f3c"
)


# category
category_colors <-
  list(

    # Electricity
    "Residential electricity" = "#FFCE1B",
    "Commercial electricity" = "#DBA400",
    "Industrial electricity" = "#B38600",

    # Residential
    "Residential natural gas" = "#483248",
    "Residential electricity" = "#CF9FFF",
    "Residential liquid fuel" = "#A2627A",

    # Commercial
    "Commercial natural gas" = "#800000",
    "Commercial fuel combustion" = "#DE3163",

    # Building Fuel
    "Residential building fuel" = "#FF9E99",
    "Commercial building fuel" = "#FF746C",
    "Industrial building fuel" = "#7A3734",

    # Industrial


    "Industrial natural gas" = "#36454F",
    "Industrial processes" = "#B2BEB5",
    "Industrial fuel combustion" = "#818589",
    "Refinery processes" = "#708090",

    # Transportation
    "Passenger vehicles" = "#0047AB",
    "Buses" = "#6F8FAF",
    "Trucks" = "#6495ED",
    "Aviation" = "#191970",

    # Waste
    "Solid waste" = "#CD853F",
    "Wastewater" = "#8B4513",

    # Agriculture
    "Cropland" = "#c7e960",
    "Livestock" = "#8fb910",

    # Natural systems
    "Sequestration" = "#006f3c",
    "Freshwater" = "#416bdf"
  )


# source
source_colors <- list(
  # transportation levels
  # "Light-duty vehicles" = "#993404",
  # we still need these colors for DOT VMT docs
  # Transportation 1
  "On-road" = "#6F8FAF",
  "Off-road" = "#191970",

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
