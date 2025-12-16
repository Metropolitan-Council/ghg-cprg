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
      "Building Fuel",
      "Electricity",
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
    x = emissions_year,
    y = value_emissions,
    fill = sector_alt,
    col = sector_alt
  )
) +
  geom_area(alpha = 0.9) +
  geom_line(alpha = 0.9) +
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
    panel.spacing.x = unit(0, "lines"),
    panel.border = element_rect(color = NA),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major = element_line(
      color = "#3A3A3A",
      size = 0.75,
      linetype = "dotted"
    ),
    text = element_text(size = 20, family = "sans")
  ) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, suffix = "M")) +
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

# 2022 plot by sector ####

emissions_sector <- county_emissions %>%
  group_by(emissions_year, sector_alt) %>%
  summarize(value_emissions = sum(value_emissions, na.rm = TRUE)) %>%
  mutate(sector_alt = factor(sector_alt, levels = c(
    "Transportation",
    "Building Fuel",
    "Electricity",
    "Industrial Processes",
    "Waste",
    "Agriculture",
    "Natural Systems"
  ))) %>%
  ungroup()



sector_colors_vector <- unlist(sector_colors_alt, use.names = TRUE)

sector_comparison <- ggplot(
  emissions_sector %>%
    filter(emissions_year == 2022),
  aes(x = sector_alt, y = value_emissions, fill = sector_alt)
) +
  geom_bar(stat = "identity", position = "stack", col = "black", alpha = 0.9) +
  labs(fill = "Sector") +
  scale_x_discrete(labels = line_break_labeller) +
  scale_fill_manual(values = sector_colors_vector, guide = "none") +
  geom_area(alpha = 0.9) +
  theme_minimal() +
  labs(
    title = "2022 Regional Emissions Profile",
    subtitle = expression(paste("(Million metric tons of ", CO[2], "e)")),
    x = "",
    y = ""
  ) +
  theme(
    axis.ticks.x = element_blank(),
    panel.grid.major = element_line(
      color = "#3A3A3A",
      size = 0.75,
      linetype = "dotted"
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    text = element_text(size = 20, family = "sans")
  ) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, suffix = "M"))

sector_comparison

ggsave(
  plot = sector_comparison,
  filename = paste0(here::here(), "/imgs/eleven_county_ghg_inv_2022.png"), # add your file path here
  width = 14,
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
  filename = paste0(here::here(), "/imgs/eleven_county_ghg_inv_2022_subsector.png"), # add your file path here
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
      "Transportation",
      "Electricity",
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
    axis.ticks.x = element_blank(),
    panel.grid.major = element_line(
      color = "#3A3A3A",
      size = 0.75,
      linetype = "dotted"
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    text = element_text(size = 20, family = "sans")
  ) +
  geom_hline(yintercept = 0, size = 1.2, col = "black", linetype = "dashed")


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

### gas type data ####

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

### graphing function

plot_county_emissions <- function(
    data,
    sector_graph,
    year,
    county_order,
    category_colors,
    category_order = NULL,
    y_max = NULL,
    y_min = 0,
    title = NULL,
    add_zero_rows = NULL,
    filename = NULL,
    width = 14,
    height = 6,
    dpi = 300,
    include_airport = FALSE) {
  # Filter and summarize
  df <- data %>%
    filter(sector_alt == sector_graph, emissions_year == year) %>%
    group_by(sector_alt, category_alt, county_name) %>%
    summarize(value_emissions = sum(value_emissions, na.rm = TRUE), .groups = "drop")

  # Optionally add counties with zero emissions
  if (!is.null(add_zero_rows)) {
    df <- df %>%
      bind_rows(add_zero_rows)
  }

  # Reassign all aviation to new "aviation column"

  if (include_airport) {
    df <- df %>%
      mutate(county_name = if_else(category_alt == "Aviation",
        "Aviation",
        county_name
      )) %>%
      group_by(sector_alt, county_name, category_alt) %>%
      summarize(value_emissions = sum(value_emissions)) %>%
      ungroup()
  }

  county_levels <- if (include_airport) c(county_order, "Aviation") else county_order

  # Apply category order if provided
  if (!is.null(category_order)) {
    df <- df %>%
      mutate(category_alt = factor(category_alt, levels = category_order))
  }

  # Build plot title if not provided
  if (is.null(title)) title <- sector_graph

  # Construct ggplot
  gg <- ggplot(
    df %>%
      mutate(county_name = factor(county_name, levels = county_levels)),
    aes(x = county_name, y = value_emissions, fill = category_alt)
  ) +
    geom_bar(stat = "identity", position = "stack", color = "#3A3A3A", size = 0.5, alpha = 0.85) +
    scale_fill_manual(
      values = unlist(category_colors)[
        names(unlist(category_colors)) %in% unique(df$category_alt)
      ],
      drop = TRUE
    ) +
    labs(
      fill = "Subsector",
      x = NULL,
      y = "",
      title = title,
      subtitle = expression(paste("Million metric tons of ", CO[2], "e"))
    ) +
    theme_minimal() +
    theme(
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(size = 14),
      axis.text.x = element_text(size = 14),
      panel.grid.major = element_line(
        color = "#3A3A3A",
        size = 0.75,
        linetype = "dotted"
      ),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      text = element_text(size = 20, family = "sans")
    ) +
    scale_y_continuous(
      labels = scales::comma_format(scale = 1e-6, suffix = "M"),
      limits = if (!is.null(y_max)) c(y_min, y_max) else NULL
    )

  # Optionally save
  if (!is.null(filename)) {
    ggsave(
      plot = gg,
      filename = filename,
      width = width,
      height = height,
      units = "in",
      dpi = dpi,
      bg = "white"
    )
  }

  return(gg)
}

# ~ Transportation ####

gg_transport <- plot_county_emissions(
  data = county_emissions,
  sector_graph = "Transportation",
  year = 2022,
  county_order = county_order,
  category_colors = category_colors,
  category_order = c(
    "Passenger vehicles",
    "Trucks",
    "Buses",
    "Aviation"
  ),
  y_max = 6e6,
  include_airport = TRUE,
  width = 16,
  filename = paste0(here::here(), "/imgs/eleven_county_transportation_2022.png")
)

gg_transport


# ~ Electricity ####

gg_county_electricity <- plot_county_emissions(
  data = county_emissions,
  sector_graph = "Electricity",
  year = 2022,
  county_order = county_order,
  category_colors = category_colors,
  category_order = c(
    "Industrial electricity",
    "Commercial electricity",
    "Residential electricity"
  ),
  y_max = 6e6,
  include_airport = TRUE,
  filename = paste0(here::here(), "/imgs/eleven_county_electricity_2022.png")
)

gg_county_electricity

# ~ Building Fuel ####

gg_county_bf <- plot_county_emissions(
  data = county_emissions,
  sector_graph = "Building Fuel",
  year = 2022,
  county_order = county_order,
  category_colors = category_colors,
  category_order = c(
    "Industrial building fuel",
    "Commercial building fuel",
    "Residential building fuel"
  ),
  y_max = 6e6,
  include_airport = TRUE,
  filename = paste0(here::here(), "/imgs/eleven_county_building_fuel_2022.png")
)

gg_county_bf


# ~ Industrial Processes ####

gg_county_ip <- plot_county_emissions(
  data = county_emissions %>%
    filter(
      sector_alt == "Industrial Processes",
      !category_alt %in% c(
        "Industrial natural gas",
        "Refinery processes"
      )
    ),
  sector_graph = "Industrial Processes",
  year = 2022,
  add_zero_rows = data.frame(
    sector_alt = "Industrial Processes",
    category_alt = "Industrial processes",
    county_name = c("St. Croix", "Pierce"),
    value_emissions = 0
  ),
  county_order = county_order,
  category_colors = category_colors,
  y_max = 0.7e6,
  include_airport = FALSE,
  filename = paste0(here::here(), "/imgs/eleven_county_industrial_processes_2022.png")
)

gg_county_ip

refinery_emissions <- county_emissions %>%
  filter(
    sector_alt == "Industrial Processes",
    category_alt %in% c(
      "Industrial natural gas",
      "Refinery processes"
    ),
    emissions_year == 2022
  )

gg_refinery <- ggplot(
  refinery_emissions,
  aes(
    x = factor(county_name),
    y = value_emissions,
    fill = category_alt
  )
) +
  geom_bar(stat = "identity", position = "stack", color = "black", alpha = 0.85) +
  scale_fill_manual(
    values = unlist(category_colors)[
      names(unlist(category_colors)) %in% unique(refinery_emissions$category_alt)
    ],
    drop = TRUE
  ) +
  labs(
    fill = "Subsector",
    x = NULL,
    y = "",
    title = "Refinery emissions",
    subtitle = expression(paste("Million metric tons of ", CO[2], "e"))
  ) +
  theme_minimal() +
  theme(
    axis.ticks.x = element_blank(),
    panel.grid.major = element_line(
      color = "#3A3A3A",
      size = 0.75,
      linetype = "dotted"
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    text = element_text(size = 20, family = "sans")
  ) +
  scale_y_continuous(
    labels = scales::comma_format(scale = 1e-6, suffix = "M"),
    limits = c(0, 6e6)
  )

gg_refinery

ggsave(
  plot = gg_refinery,
  filename = paste0(here::here(), "/imgs/refinery_emissions_2022.png"), # add your file path here
  width = 6,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)

# ~ Waste ####

gg_county_waste <- plot_county_emissions(
  data = county_emissions,
  sector_graph = "Waste",
  year = 2022,
  county_order = county_order,
  category_colors = category_colors,
  y_max = 0.7e6,
  include_airport = FALSE,
  filename = paste0(here::here(), "/imgs/eleven_county_waste_2022.png")
)

gg_county_waste


# ~ Agriculture ####

gg_county_ag <- plot_county_emissions(
  data = county_emissions,
  sector_graph = "Agriculture",
  year = 2022,
  county_order = county_order,
  category_colors = category_colors,
  y_max = 0.7e6,
  include_airport = FALSE,
  filename = paste0(here::here(), "/imgs/eleven_county_agriculture_2022.png")
)

gg_county_ag


# ~ Natural Systems ####
gg_county_ns <- plot_county_emissions(
  data = county_emissions,
  sector_graph = "Natural Systems",
  year = 2022,
  county_order = county_order,
  category_colors = category_colors,
  y_max = 0.7e6,
  y_min = -0.4e6,
  include_airport = FALSE,
  filename = paste0(here::here(), "/imgs/eleven_county_natural_systems_2022.png")
)

gg_county_ns


#### summary stats #####

# decrease from 2005-2022

(county_emissions %>% filter(emissions_year == 2022) %>% pull(value_emissions) %>% sum() -
  county_emissions %>% filter(emissions_year == 2005) %>% pull(value_emissions) %>% sum()) /
  county_emissions %>%
    filter(emissions_year == 2005) %>%
    pull(value_emissions) %>%
    sum()

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
    # sector != "Natural Systems",
    category_alt != "Freshwater",
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
