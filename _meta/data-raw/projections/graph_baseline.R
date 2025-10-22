#### Create county and CTU temporary inventory graphs for 11-14-2024 steering committee meeting
source("R/_load_pkgs.R")

### county graphs

cprg_colors <- source("R/cprg_colors.R")

# create baseline and subsector sectors
county_emissions <- readRDS("_meta/data/cprg_county_emissions.RDS") %>%
  mutate(
    sector_alt = case_when(
      category == "Electricity" ~ category,
      category == "Building Fuel" ~ category,
      category == "Commercial fuel combustion" ~ "Building Fuel",
      category == "Commercial natural gas" ~ "Building Fuel",
      sector == "Industrial" ~ "Industrial Processes",
      TRUE ~ sector
    ),
    category_alt = case_when(
      category == "Electricity" ~ str_to_sentence(paste(sector, category)),
      category == "Building Fuel" ~ str_to_sentence(paste(sector, category)),
      category == "Commercial fuel combustion" ~ "Commercial building fuel",
      category == "Commercial natural gas" ~ "Commercial building fuel",
      TRUE ~ category
    )
  ) %>%
  mutate(sector_alt = factor(sector_alt,
    levels = c(
      "Transportation",
      "Electricity",
      "Building Fuel",
      "Industrial Processes",
      "Waste",
      "Agriculture",
      "Natural Systems"
    )
  )) %>%
  filter(
    emissions_year >= 2005,
    !is.na(value_emissions)
  )

gas_type <- read_rds("_meta/data/county_emissions_by_gas.RDS")

# summarize to sector and reorder sectors
baseline_emissions_sector <- county_emissions %>%
  group_by(emissions_year, sector_alt) %>%
  summarize(value_emissions = sum(value_emissions, na.rm = TRUE)) %>%
  filter(!is.na(sector_alt))

line_break_labeller <- function(x) {
  str_replace_all(x, " ", "\n")
}

### facet plot 2005-2022 ####
baseline_comparison_facet <- ggplot(
  baseline_emissions_sector %>%
    filter(emissions_year >= 2005 & emissions_year <= 2022),
  aes(
    x = emissions_year, y = value_emissions / 1000000,
    fill = sector_alt,
    col = sector_alt
  )
) +
  geom_area(alpha = 0.4) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, size = 1.2, col = "black", linetype = "dashed") +
  labs(fill = "sector_alt") +
  # ggtitle() +
  # ggsubtitle(expression(paste("Million metric tons of ", CO[2], "e"))) +
  scale_fill_manual(values = unlist(sector_colors_alt), guide = "none") +
  scale_color_manual(values = unlist(sector_colors_alt), guide = "none") +
  theme_bw() +
  labs(
    title = "Eleven-County Regional Emissions Inventory",
    subtitle = expression(paste("(Million metric tons of ", CO[2], "e)"))
  ) +
  xlab("Inventory: 2005 - 2022") +
  ylab("") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    text = element_text(size = 20, family = "sans")
  ) +
  facet_grid(. ~ sector_alt, labeller = labeller(sector_alt = line_break_labeller))

baseline_comparison_facet

ggsave(
  plot = baseline_comparison_facet,
  filename = paste0(here::here(), "/imgs/eleven_county_baseline_facet_graph.png"), # add your file path here
  width = 12,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)

# 2022 plot by subsector ####

emissions_subsector <- county_emissions %>%
  group_by(emissions_year, sector_alt, category_alt) %>%
  summarize(value_emissions = sum(value_emissions, na.rm = TRUE)) %>%
  mutate(sector_alt = factor(sector_alt, levels = c(
    "Transportation",
    "Electricity",
    "Building Fuel",
    "Industrial Processes",
    "Waste",
    "Agriculture",
    "Natural Systems"
  ))) %>%
  ungroup()


emissions_subsector %>%
  distinct(sector_alt, category_alt) %>%
  print(n = 50)

category_order <- c(
  "Aviation", "Passenger vehicles", "Buses", "Trucks", # Transportation
  "Residential electricity", "Commercial electricity", "Industrial electricity", # Electricity
  "Residential building fuel", "Commercial building fuel", "Industrial building fuel", # Building Fuel
  "Industrial natural gas", "Industrial fuel combustion", "Industrial processes", "Refinery processes", # Industrial
  "Solid waste", "Wastewater", # Waste
  "Livestock", "Cropland", # Agriculture
  "Freshwater", "Sequestration" # Natural Systems
)

category_colors_vector <- unlist(category_colors, use.names = TRUE)

subsector_comparison <- ggplot(
  emissions_subsector %>%
    filter(emissions_year == 2022) %>%
    mutate(
      category_alt = factor(category_alt, levels = category_order)
    ),
  aes(x = sector_alt, y = value_emissions, fill = category_alt)
) +
  geom_bar(stat = "identity", position = "stack", col = "black") +
  labs(fill = "Subsector") +
  scale_x_discrete(labels = line_break_labeller) +
  scale_fill_manual(values = category_colors_vector) +
  theme_minimal() +
  labs(
    title = "2022 Regional Emissions Profile",
    subtitle = expression(paste("(Million metric tons of ", CO[2], "e)")),
    x = "",
    y = ""
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 17, vjust = 1),
    text = element_text(size = 20, family = "sans")
  ) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, suffix = "M"))

subsector_comparison

ggsave(
  plot = subsector_comparison,
  filename = paste0(here::here(), "/imgs/eleven_county_ghg_inv_2022.png"), # add your file path here
  width = 14,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)

### subsector per capita #####

county_emissions_no_msp <- county_emissions %>%
  filter(county_name != "MSP Airport")

emissions_sector_per_capita <- county_emissions_no_msp %>%
  mutate(emissions_per_capita = value_emissions / county_total_population) %>%
  group_by(emissions_year, county_name, sector_alt) %>%
  summarize(emissions_per_capita = sum(emissions_per_capita, na.rm = TRUE)) %>%
  mutate(sector_alt = factor(sector_alt,
    levels = c(
      "Industrial Processes",
      "Agriculture",
      "Waste",
      "Building Fuel",
      "Electricity",
      "Transportation",
      "Natural Systems"
    )
  ))

emissions_sector_per_county <- county_emissions_no_msp %>%
  group_by(emissions_year, county_name, sector_alt) %>%
  summarize(emissions_total = sum(value_emissions, na.rm = TRUE))

sector_colors_vector <- unlist(sector_colors_alt, use.names = TRUE)


county_order <- county_emissions_no_msp %>%
  filter(emissions_year == 2022) %>%
  group_by(county_name) %>%
  summarize(total_population = max(county_total_population, na.rm = TRUE)) %>%
  arrange(desc(total_population)) %>%
  pull(county_name)

# Plot
sector_per_capita_comparison <- emissions_sector_per_capita %>%
  filter(emissions_year == 2022) %>%
  mutate(county_name = factor(county_name, levels = county_order)) %>%
  ggplot(aes(x = county_name, y = emissions_per_capita, fill = sector_alt)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_fill_manual(values = sector_colors_vector) +
  labs(
    fill = "Sector",
    x = NULL,
    y = "",
    title = expression(paste("Metric tons of ", CO[2], "e per capita"))
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    text = element_text(size = 16, family = "sans")
  )

sector_per_capita_comparison

ggsave(
  plot = sector_per_capita_comparison,
  filename = paste0(here::here(), "/imgs/eleven_county_per_capita_2022.png"), # add your file path here
  width = 14,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)

### gas type graph ####

gas_type_2022 <- gas_type %>%
  mutate(gas_id = case_when(
    units_emissions %in% c("Metric tons CO2") ~ "CO2",
    units_emissions %in% c("Metric tons CH4") ~ "CH4",
    units_emissions %in% c("Metric tons N2O") ~ "N2O",
    units_emissions %in% c(
      "Metric tons HFE",
      "Metric tons HFC"
    ) ~ "HFC",
    units_emissions %in% c(
      "Metric tons PFC",
      "Metric tons Other Fully Fluorinated Gas"
    ) ~ "PFC",
    TRUE ~ "Other fluorinated GHG"
  )) %>%
  group_by(gas_id) %>%
  summarize(
    value_emissions = sum(value_emissions),
    mt_co2e = sum(metric_tons_co2e)
  ) %>%
  mutate(
    perc_gas_weight = value_emissions / sum(value_emissions),
    perc_gas_co2e = mt_co2e / sum(mt_co2e)
  )

write_csv(gas_type_2022,
  file = paste0(here::here(), "/_meta/data-raw/projections/gas_type_2022.csv")
)


# county sector bar charts ####

# ~ Transportation ####


county_transport <- county_emissions %>%
  filter(
    sector_alt == "Transportation",
    emissions_year == 2022
  ) %>%
  group_by(sector_alt, category_alt, county_name) %>%
  summarize(value_emissions = sum(value_emissions))

gg_county_transportation <-
  ggplot(
    county_transport %>%
      mutate(county_name = factor(county_name, levels = c(county_order, "MSP Airport"))),
    aes(x = county_name, y = value_emissions, fill = category_alt)
  ) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_fill_manual(values = unlist(category_colors)) +
  labs(
    fill = "Subsector",
    x = NULL,
    y = "",
    title = "Transportation",
    subtitle = expression(paste("Million metric tons of ", CO[2], "e"))
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    text = element_text(size = 16, family = "sans")
  ) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, suffix = "M"))

gg_county_transportation

ggsave(
  plot = gg_county_transportation,
  paste0(here::here(), "/imgs/eleven_county_transportation_2022.png"), # add your file path here
  width = 14,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)

# ~ Electricity ####

county_electricity <- county_emissions %>%
  filter(
    sector_alt == "Electricity",
    emissions_year == 2022
  ) %>%
  group_by(sector_alt, category_alt, county_name) %>%
  summarize(value_emissions = sum(value_emissions))

gg_county_electricity <-
  ggplot(
    county_electricity %>%
      mutate(county_name = factor(county_name, levels = county_order)),
    aes(x = county_name, y = value_emissions, fill = category_alt)
  ) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_fill_manual(values = unlist(category_colors)) +
  labs(
    fill = "Subsector",
    x = NULL,
    y = "",
    title = "Electricity Demand",
    subtitle = expression(paste("Million metric tons of ", CO[2], "e"))
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    text = element_text(size = 16, family = "sans")
  ) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, suffix = "M"))

gg_county_electricity

ggsave(
  plot = gg_county_electricity,
  filename = paste0(here::here(), "/imgs/eleven_county_electricity_2022.png"), # add your file path here
  width = 14,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)

# ~ Building Fuel ####

county_bf <- county_emissions %>%
  filter(
    sector_alt == "Building Fuel",
    emissions_year == 2022
  ) %>%
  group_by(sector_alt, category_alt, county_name) %>%
  summarize(value_emissions = sum(value_emissions))

gg_county_bf <-
  ggplot(
    county_bf %>%
      mutate(county_name = factor(county_name, levels = county_order)),
    aes(x = county_name, y = value_emissions, fill = category_alt)
  ) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_fill_manual(values = unlist(category_colors)) +
  labs(
    fill = "Subsector",
    x = NULL,
    y = "",
    title = "Building Fuel",
    subtitle = expression(paste("Million metric tons of ", CO[2], "e"))
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    text = element_text(size = 16, family = "sans")
  ) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, suffix = "M"))

gg_county_bf

ggsave(
  plot = gg_county_bf,
  filename = paste0(here::here(), "/imgs/eleven_county_building_fuel_2022.png"), # add your file path here
  width = 14,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)

# ~ Industrial Processes ####

county_ip <- county_emissions %>%
  filter(
    sector_alt == "Industrial Processes",
    emissions_year == 2022
  ) %>%
  group_by(sector_alt, category_alt, county_name) %>%
  summarize(value_emissions = sum(value_emissions)) %>%
  # add Wisconsin counties with zero emissions for graph consistency
  bind_rows(data.frame(
    sector_alt = "Industrial Processes",
    category_alt = "Industrial processes",
    county_name = c("St. Croix", "Pierce"),
    value_emissions = 0
  ))

gg_county_ip <-
  ggplot(
    county_ip %>%
      mutate(county_name = factor(county_name, levels = county_order)),
    aes(x = county_name, y = value_emissions, fill = category_alt)
  ) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_fill_manual(values = unlist(category_colors)) +
  labs(
    fill = "Subsector",
    x = NULL,
    y = "",
    title = "Industrial Processes",
    subtitle = expression(paste("Million metric tons of ", CO[2], "e"))
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    text = element_text(size = 16, family = "sans")
  ) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, suffix = "M"))

gg_county_ip

ggsave(
  plot = gg_county_ip,
  filename = paste0(here::here(), "/imgs/eleven_county_industrial_processes_2022.png"), # add your file path here
  width = 14,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)

# ~ Waste ####

county_waste <- county_emissions %>%
  filter(
    sector_alt == "Waste",
    emissions_year == 2022
  ) %>%
  group_by(sector_alt, category_alt, county_name) %>%
  summarize(value_emissions = sum(value_emissions))

gg_county_waste <-
  ggplot(
    county_waste %>%
      mutate(county_name = factor(county_name, levels = county_order)),
    aes(x = county_name, y = value_emissions, fill = category_alt)
  ) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_fill_manual(values = unlist(category_colors)) +
  labs(
    fill = "Subsector",
    x = NULL,
    y = "",
    title = "Waste",
    subtitle = expression(paste("Million metric tons of ", CO[2], "e"))
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    text = element_text(size = 16, family = "sans")
  ) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, suffix = "M"))

gg_county_waste

ggsave(
  plot = gg_county_waste,
  filename = paste0(here::here(), "/imgs/eleven_county_waste_2022.png"), # add your file path here
  width = 14,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)

# ~ Agriculture ####

county_ag <- county_emissions %>%
  filter(
    sector_alt == "Agriculture",
    emissions_year == 2021
  ) %>%
  group_by(sector_alt, category_alt, county_name) %>%
  summarize(value_emissions = sum(value_emissions))

gg_county_ag <-
  ggplot(
    county_ag %>%
      mutate(county_name = factor(county_name, levels = county_order)),
    aes(x = county_name, y = value_emissions, fill = category_alt)
  ) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_fill_manual(values = unlist(category_colors)) +
  labs(
    fill = "Subsector",
    x = NULL,
    y = "",
    title = "Agriculture",
    subtitle = expression(paste("Million metric tons of ", CO[2], "e"))
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    text = element_text(size = 16, family = "sans")
  ) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, suffix = "M"))

gg_county_ag

ggsave(
  plot = gg_county_ag,
  filename = paste0(here::here(), "/imgs/eleven_county_agriculture_2022.png"), # add your file path here
  width = 14,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)

# ~ Natural Systems ####

county_ns <- county_emissions %>%
  filter(
    sector_alt == "Natural Systems",
    emissions_year == 2022
  ) %>%
  group_by(sector_alt, category_alt, county_name) %>%
  summarize(value_emissions = sum(value_emissions))

gg_county_ns <-
  ggplot(
    county_ns %>%
      mutate(county_name = factor(county_name, levels = county_order)),
    aes(x = county_name, y = value_emissions, fill = category_alt)
  ) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_fill_manual(values = unlist(category_colors)) +
  labs(
    fill = "Subsector",
    x = NULL,
    y = "",
    title = "Natural Systems",
    subtitle = expression(paste("Million metric tons of ", CO[2], "e"))
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    text = element_text(size = 16, family = "sans")
  ) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, suffix = "M"))

gg_county_ns

ggsave(
  plot = gg_county_ns,
  filename = paste0(here::here(), "/imgs/eleven_county_natural_systems_2022.png"), # add your file path here
  width = 14,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)


#### summary stats #####

county_emissions %>%
  filter(
    sector_alt == "Transportation",
    emissions_year %in% c(2005, 2022)
  ) %>%
  group_by(emissions_year, category_alt) %>%
  summarize(value_emissions = sum(value_emissions))

county_emissions %>%
  filter(
    sector_alt == "Electricity",
    emissions_year %in% c(2005, 2022)
  ) %>%
  group_by(emissions_year) %>%
  summarize(value_emissions = sum(value_emissions))

county_emissions %>%
  filter(
    sector_alt == "Building Fuel",
    emissions_year %in% c(2005, 2022)
  ) %>%
  group_by(emissions_year) %>%
  summarize(value_emissions = sum(value_emissions))

county_emissions %>%
  filter(
    sector_alt == "Industrial Processes",
    emissions_year %in% c(2005, 2022)
  ) %>%
  group_by(emissions_year) %>%
  summarize(value_emissions = sum(value_emissions))

county_emissions %>%
  filter(
    sector_alt == "Waste",
    emissions_year %in% c(2005, 2022)
  ) %>%
  group_by(emissions_year) %>%
  summarize(value_emissions = sum(value_emissions))

county_emissions %>%
  filter(
    sector_alt == "Agriculture",
    emissions_year %in% c(2005, 2021)
  ) %>%
  group_by(emissions_year) %>%
  summarize(value_emissions = sum(value_emissions))

county_emissions %>%
  filter(
    sector_alt == "Natural Systems",
    emissions_year %in% c(2005, 2022)
  ) %>%
  group_by(emissions_year) %>%
  summarize(value_emissions = sum(value_emissions))

county_emissions %>%
  filter(
    sector_alt != "Natural Systems",
    emissions_year %in% c(2005, 2022)
  ) %>%
  group_by(emissions_year) %>%
  summarize(value_emissions = sum(value_emissions))

county_emissions %>%
  filter(emissions_year %in% c(2005, 2022)) %>%
  group_by(emissions_year) %>%
  summarize(value_emissions = sum(value_emissions))

county_emissions %>%
  filter(emissions_year %in% c(2005, 2022)) %>%
  group_by(emissions_year, county_name) %>%
  filter(county_name != "MSP Airport") %>%
  summarize(county_pop = max(county_total_population)) %>%
  ungroup() %>%
  group_by(emissions_year) %>%
  summarize(total_pop = sum(county_pop)) %>%
  left_join(
    county_emissions %>% filter(
      sector_alt != "Electricity",
      emissions_year %in% c(2005, 2022)
    ) %>%
      group_by(emissions_year) %>%
      summarize(value_emissions = sum(value_emissions))
  ) %>%
  mutate(emissions_per_capita = value_emissions / total_pop)

county_emissions %>%
  filter(
    category_alt != "Sequestration",
    emissions_year %in% c(2022)
  ) %>%
  group_by(sector_alt) %>%
  summarize(value_emissions = sum(value_emissions)) %>%
  mutate(proportion = value_emissions / sum(value_emissions))

county_emissions %>%
  filter(emissions_year %in% c(2005, 2022)) %>%
  group_by(sector_alt, emissions_year) %>%
  summarize(value_emissions = sum(value_emissions), .groups = "drop") %>%
  group_by(sector_alt) %>%
  summarize(
    emissions_2005 = value_emissions[emissions_year == 2005],
    emissions_2022 = value_emissions[emissions_year == 2022],
    pct_of_2005 = (emissions_2022 / emissions_2005) * 100
  )

# sector_total_comparison <- emissions_sector_per_county %>%
#   filter(emissions_year == 2022) %>%
#   mutate(county_name = factor(county_name, levels = county_order)) %>%
#   ggplot(aes(x = county_name, y = emissions_total, fill = sector_alt)) +
#   geom_bar(stat = "identity", position = "stack") +
#   scale_fill_manual(values = sector_colors_vector, guide = "none") +
#   coord_flip() +
#   labs(
#     fill = "Sector",
#     x = NULL,
#     y = expression(paste("Million metric tons of ", CO[2], "e"))
#   ) +
#   theme_minimal() +
#   theme(
#     panel.grid.major.y = element_blank(),
#     axis.text.y = element_text(size = 14),
#     axis.text.x = element_text(size = 14),
#     text = element_text(size = 16, family = "sans")
#   ) +
#   scale_y_continuous(labels = scales::comma_format(scale = 1e-6, suffix = "M"))
#
# sector_total_comparison

### remove less sensible per capita emissions
