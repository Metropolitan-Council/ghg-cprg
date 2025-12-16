#### plot electricity graph

source("R/_load_pkgs.R")
source("R/cprg_colors.R")
source("_meta/data-raw/projections/01_projections_plotter.R")

residential_elec <- readRDS("_meta/data-raw/projections/residential_pathways.RDS") %>%
  filter(scenario == "bau") %>%
  select(inventory_year, scenario, electricity_emissions)


commercial_elec <- readRDS("_meta/data-raw/projections/nonres_elec_bau.rds") %>%
  rename(inventory_year = emissions_year)

electricity <- left_join(residential_elec, commercial_elec,
  by = join_by(inventory_year, scenario)
) %>%
  mutate(value_emissions = electricity_emissions + value_emissions) %>%
  select(-electricity_emissions)

## save bau data
# waldo::compare(electricity, readRDS( "_meta/data-raw/projections/electricity_bau.rds"))

message("Saving electricity BAU projections data to: \n\t _meta/data-raw/projections/electricity_bau.rds")
saveRDS(
  electricity,
  "_meta/data-raw/projections/electricity_bau.rds"
)

base_data <- electricity %>%
  filter(inventory_year <= 2025, scenario == "bau") %>% # Use any scenario since they're identical
  mutate(segment = "base")

#  diverging scenarios (2026+)
diverging_data <- electricity %>%
  filter(inventory_year >= 2025) %>%
  mutate(segment = "diverging")

# net_zero_data <- diverging_data %>% filter(scenario == "nz")

# PPP data - need to merge with net_zero for the lower bound
ppp_data <- diverging_data %>%
  filter(scenario == "bau") %>%
  select(inventory_year, value_emissions) %>%
  rename(ppp_emissions = value_emissions)

# Calculate text position for BAU annotation
text_bau <- diverging_data %>%
  filter(inventory_year == 2050, scenario == "bau") %>%
  pull(value_emissions)

emissions_gg <- ggplot() +
  # Gray fill for historical data (2005-2025)
  geom_ribbon(
    data = base_data,
    aes(x = inventory_year, ymin = 0, ymax = value_emissions),
    fill = "gray80", alpha = 0.7
  ) +

  # Gray fill below BAU line for projections
  geom_ribbon(
    data = diverging_data %>% filter(scenario == "bau"),
    aes(x = inventory_year, ymin = 0, ymax = value_emissions),
    fill = "gray80", alpha = 0.7
  ) +

  # Base line (historical, dashed)
  geom_line(
    data = base_data,
    aes(x = inventory_year, y = value_emissions),
    color = "black", linewidth = 1, linetype = "dashed"
  ) +

  # BAU line (dashed)
  geom_line(
    data = diverging_data %>% filter(scenario == "bau"),
    aes(x = inventory_year, y = value_emissions),
    color = "black", linewidth = 1, linetype = "dashed"
  ) +

  # Axis lines
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_vline(xintercept = 2005, color = "black", linewidth = 0.5) +
  labs(
    x = "",
    y = "",
    title = "Electricity Emissions"
  ) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  scale_x_continuous(
    limits = c(2005, 2059),
    breaks = seq(2010, 2059, by = 10)
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 18),
    axis.text = element_text(size = 14, color = "black"),
    legend.text = element_text(size = 18),
    legend.key.width = unit(1.2, "cm"),
    legend.box = "vertical",
    plot.margin = ggplot2::margin(5.5, 5.5, 30, 5.5, "pt")
  ) +

  # Add text annotation for BAU
  annotate("text",
    x = 2050.5, y = text_bau + 500000,
    label = "Business-as-usual",
    size = 5, hjust = 0, vjust = 0.5, fontface = "bold"
  ) +

  # Add "Inventory" and "Projections" annotations below x-axis
  annotation_custom(
    grob = grid::textGrob("Inventory", gp = grid::gpar(fontsize = 14), vjust = 3),
    xmin = 2010, xmax = 2010, ymin = -Inf, ymax = -Inf
  ) +
  annotation_custom(
    grob = grid::textGrob("Projections", gp = grid::gpar(fontsize = 14), vjust = 3),
    xmin = 2030, xmax = 2030, ymin = -Inf, ymax = -Inf
  ) +
  coord_cartesian(clip = "off")

print(emissions_gg)

message("Saving nonresidential projections plot to: \n\t ~/imgs/electricity_decarbonization_pathways.png")
ggplot2::ggsave(
  plot = emissions_gg,
  filename = paste0(here::here(), "/imgs/electricity_decarbonization_pathways.png"), # add your file path here
  width = 12,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)


message("Finished electricity projections")
