##### Seven county graphs
source("R/_load_pkgs.R")

msa_inv_raw <- readRDS("_meta/data/cprg_county_emissions.RDS") %>%
  filter(!geog_name %in% c("Pierce", "Sherburne", "St. Croix", "Chisago")) %>% 
  filter(year == 2021)

sector_use_category <- tibble::tribble(
  ~sector, ~sector_use, ~source, ~source_use, ~category,
  "Energy", "Electricity", "Electricity", "Commercial Electricity", "Commercial energy",
  "Energy", "Electricity", "Electricity", "Industrial Electricity", "Industrial energy",
  "Energy", "Electricity", "Electricity", "Residential Electricity", "Residential energy",
  "Energy", "Building Fuel", "Natural gas", "Commercial Building Fuel", "Commercial energy",
  "Energy", "Building Fuel", "Natural gas", "Industrial Building Fuel", "Industrial energy",
  "Energy", "Building Fuel", "Propane", "Residential Building Fuel", "Liquid stationary fuels",
  "Energy", "Building Fuel", "Kerosene", "Residential Building Fuel", "Liquid stationary fuels",
  "Energy", "Building Fuel", "Natural gas", "Residential Building Fuel", "Residential energy",
  "Transportation", "Transportation", "Heavy-duty vehicles", "Heavy-duty vehicles", "Heavy-duty vehicles",
  "Transportation", "Transportation", "Light-duty vehicles", "Light-duty vehicles", "Light-duty vehicles",
  "Waste", "Waste", "Landfill", "Solid waste", "Solid waste",
  "Waste", "Waste", "Organics", "Solid waste", "Solid waste",
  "Waste", "Waste", "Recycling", "Solid waste", "Solid waste",
  "Waste", "Waste", "Waste to energy", "Solid waste", "Solid waste",
  "Waste", "Waste", "Wastewater", "Wastewater", "Wastewater",
  "Agriculture", "Agriculture", "Livestock", "Livestock", "Enteric_fermentation",
  "Agriculture", "Agriculture", "Livestock", "Livestock", "Manure_management",
  "Agriculture", "Agriculture", "Livestock", "Livestock", "Direct_manure_soil_emissions",
  "Agriculture", "Agriculture", "Livestock", "Livestock", "Indirect_manure_runoff_emissions",
  "Agriculture", "Agriculture", "Cropland", "Cropland", "Crop_residue_emissions",
  "Agriculture", "Agriculture", "Cropland", "Cropland","Crop_fertilizer_emissions",
  "Agriculture", "Agriculture", "Cropland", "Cropland","Runoff_fertilizer_emissions",
  "Nature", "Nature", "Grassland", "Grassland", "Sequestration",
  "Nature", "Nature", "Tree", "Tree", "Sequestration",
  "Nature", "Nature", "Urban grassland", "Urban grassland", "Sequestration",
  "Nature", "Nature", "Urban tree", "Urban tree", "Sequestration",
  "Nature", "Nature", "Wetland", "Wetland", "Sequestration"
) %>%
  mutate(sector_use = factor(sector_use,
    levels = c(
      "Electricity", "Building Fuel",
      "Transportation", "Waste", "Nature"
    )
  ))


msa_inv <- msa_inv_raw %>%
  left_join(sector_use_category, by = join_by(sector, category, source)) %>%
  arrange(sector_use, source_use) %>% # Arrange by sector_use and source_use alphabetically
  group_by(sector_use) %>% # Group by sector_use to apply within-sector ordering
  mutate(source_use = factor(source_use, levels = sort(unique(source_use)))) %>% # Order source_use alphabetically within each sector_use
  ungroup() # Ungroup to finalize the data frame

msa_total_inv <- msa_inv %>%
  group_by(sector_use, category) %>%
  summarise(
    emissions = sum(emissions_metric_tons_co2e),
    .groups = "keep"
  )


## create color palettes for each sector
generate_shades <- function(palette_name, n) {
  if (n < 3) {
    brewer.pal(3, palette_name)[1:n]
  } else {
    brewer.pal(n, palette_name)
  }
}

# Define base colors for each sector
palette_names <- c(
  Electricity = "Blues",
  `Natural Gas and Fuel` = "Purples",
  Transportation = "Oranges",
  Waste = "Reds",
  Nature = "Greens"
)

color_palette <- msa_total_inv %>%
  group_by(sector_use) %>%
  summarise(subsectors = list(sort(unique(source_use))), .groups = "drop") %>%
  mutate(
    palette_name = palette_names[sector_use],
    num_colors = map_int(subsectors, length),
    shades = map2(palette_name, num_colors, generate_shades)
  ) %>%
  unnest(cols = c(subsectors, shades))

color_palette_vector <- setNames(color_palette$shades, color_palette$subsectors)

# set color palette vector names
color_palette_vector_sector <- setNames(color_palette$shades, color_palette$sector_use)
color_palette_vector_sector <- color_palette_vector_sector[duplicated(names(color_palette_vector_sector))]
color_palette_vector_sector <- color_palette_vector_sector[!duplicated(names(color_palette_vector_sector))]

# sector wide graph
sector_graph <- ggplot(
  msa_total_inv %>% group_by(sector_use) %>% summarize(emissions = sum(emissions)),
  aes(x = sector_use, y = emissions / 1000000, fill = sector_use)
) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = color_palette_vector_sector, guide = FALSE) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 25, vjust = 0.5, hjust = 0.45, size = 14),
    axis.title.y = element_text(size = 16)
  ) +
  ylab("Millions of metric tons of CO2e") +
  xlab("")

sector_graph


###  segmented sector graph

subsector_graph <- ggplot(
  msa_total_inv,
  aes(x = sector_use, y = emissions / 1000000, fill = source_use)
) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = color_palette_vector) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 25, vjust = 0.5, hjust = 0.45, size = 14),
    axis.title.y = element_text(size = 16)
  ) +
  ylab("Millions of metric tons of CO2e") +
  xlab("") +
  labs(fill = "Subsector")

subsector_graph


### county breakdowns

county_elec <- ggplot(
  msa_inv %>% filter(sector_use == "Electricity"),
  aes(x = geog_name, y = emissions_metric_tons_co2e / 1000000, fill = source_use)
) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = color_palette_vector) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.45, size = 14),
    axis.title.y = element_text(size = 16)
  ) +
  ylab("Millions of metric tons of CO2e") +
  xlab("") +
  labs(fill = "Subsector")

county_elec

county_elec_pop <- ggplot(
  msa_inv %>% filter(sector_use == "Electricity"),
  aes(x = geog_name, y = emissions_metric_tons_co2e / county_total_population, fill = source_use)
) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = color_palette_vector) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.45, size = 14),
    axis.title.y = element_text(size = 16)
  ) +
  ylab("Metric tons of CO2e per capita") +
  xlab("") +
  labs(fill = "Subsector")

county_elec_pop

county_building <- ggplot(
  msa_inv %>% filter(sector_use == "Building Fuel"),
  aes(x = geog_name, y = emissions_metric_tons_co2e / 1000000, fill = source_use)
) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = color_palette_vector) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.45, size = 14),
    axis.title.y = element_text(size = 16)
  ) +
  ylab("Millions of metric tons of CO2e") +
  xlab("") +
  labs(fill = "Subsector")

county_building


county_transport <- ggplot(
  msa_inv %>% filter(sector_use == "Transportation"),
  aes(x = geog_name, y = emissions_metric_tons_co2e / 1000000, fill = source_use)
) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = color_palette_vector) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.45, size = 14),
    axis.title.y = element_text(size = 16)
  ) +
  ylab("Millions of metric tons of CO2e") +
  xlab("") +
  labs(fill = "Subsector")

county_transport


county_waste <- ggplot(
  msa_inv %>% filter(sector_use == "Waste"),
  aes(x = geog_name, y = emissions_metric_tons_co2e / 1000000, fill = source_use)
) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = color_palette_vector) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.45, size = 14),
    axis.title.y = element_text(size = 16)
  ) +
  ylab("Millions of metric tons of CO2e") +
  xlab("") +
  labs(fill = "Subsector")

county_waste

county_seq <- ggplot(
  msa_inv %>% filter(sector_use == "Nature"),
  aes(x = geog_name, y = emissions_metric_tons_co2e / 1000000, fill = source_use)
) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = color_palette_vector) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.45, size = 14),
    axis.title.y = element_text(size = 16)
  ) +
  ylab("Millions of metric tons of CO2e") +
  xlab("") +
  labs(fill = "Subsector")

county_seq

# carbon stock potential in natural systems

carbon_stock <- readRDS(file.path(here::here(), "_meta/data/cprg_county_carbon_stock.RDS")) %>%
  filter(!geog_name %in% c("Pierce", "Sherburne", "St. Croix", "Chisago"))

county_stock <- ggplot(
  carbon_stock,
  aes(x = geog_name, y = emissions_metric_tons_co2e / 1000000, fill = source)
) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = color_palette_vector) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.45, size = 14)
  ) +
  ylab("Millions of metric tons of CO2e") +
  xlab("")

county_stock


### total stock vs total emissions

stock_emissions <- bind_rows(
  data.frame(carbon = "Stock\nPotential", metric_tons_co2e = -1 * sum(carbon_stock$emissions_metric_tons_co2e)),
  data.frame(carbon = "Total\nEmissions", metric_tons_co2e = sum(msa_inv$emissions_metric_tons_co2e[msa_inv$sector != "Nature"]))
)

stock_emissions

emissions_v_stock <- ggplot(
  stock_emissions,
  aes(x = carbon, y = metric_tons_co2e / 1000000, fill = carbon)
) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#66CC66", "cornflowerblue"), guide = "none") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 16)
  ) +
  ylab("Millions of metric tons of CO2e") +
  xlab("")

emissions_v_stock
