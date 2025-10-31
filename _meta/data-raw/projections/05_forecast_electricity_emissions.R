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

# Create the plot
emissions_gg <- ggplot() +
  # Base fill (2005-2025, gray)
  geom_ribbon(
    data = base_data,
    aes(x = inventory_year, ymin = 0, ymax = value_emissions),
    fill = "gray80", alpha = 0.7
  ) +

  # # Net zero fill (maroon)
  # geom_ribbon(data = net_zero_data,
  #             aes(x = inventory_year, ymin = 0, ymax = total_emissions),
  #             fill = "maroon", alpha = 0.3) +

  # PPP fill (from net_zero to ppp)
  geom_ribbon(
    data = ppp_data,
    aes(x = inventory_year, ymin = 0, ymax = ppp_emissions),
    fill = "#ecb81c", alpha = 0.5
  ) +

  # Base line (2005-2025)
  geom_line(
    data = base_data,
    aes(x = inventory_year, y = value_emissions),
    color = "black", linewidth = 1
  ) +

  # Diverging scenario lines
  geom_line(
    data = diverging_data %>% filter(scenario == "bau"),
    aes(x = inventory_year, y = value_emissions, color = "Business as usual"),
    linetype = "dashed", linewidth = 1
  ) +

  # geom_line(data = diverging_data %>% filter(scenario == "ppp"),
  #           aes(x = inventory_year, y = total_emissions, color = "Potential policy pathways"),
  #           size = 1) +

  # geom_point(
  #   data = data.frame(emissions_year = 2050, value_emissions = res_target),
  #   aes(x = emissions_year, y = value_emissions),
  #   shape = "*",    # asterisk
  #   size = 12,     # make larger or smaller
  #   stroke = 1.5, # line thickness of the asterisk
  #   color = "black"
  # ) +
  #
  geom_segment(aes(x = 2025, xend = 2025, y = 0, yend = base_data %>% filter(inventory_year == 2025) %>% pull(value_emissions)),
    color = "black", linetype = "solid", linewidth = 0.8
  ) +
  # annotate("text", x = 2025, y = max(your_data$total_emissions) * 0.9,
  #          label = "Historical | Projected", angle = 90, hjust = 1, size = 3.5) +

  # Manual color scale with correct order
  scale_color_manual(
    values = c(
      # "Business as usual" = "black",
      # "Net zero" = "maroon",
      "Business as usual" = "black"
    ),
    breaks = c("Business as usual") # Force legend order
  ) +

  # Manual legend guide to show line types
  guides(
    color = guide_legend(
      title = "Scenarios",
      override.aes = list(
        linetype = c("dashed"),
        color = c("black")
      )
    )
  ) +
  labs(
    x = "Year",
    y = "",
    title = "Electricity Emissions \n(Millions of CO2-equivalency)"
  ) +
  scale_y_continuous(labels = label_number(scale = 1e-6)) + # convert to millions
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 18),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 18),
    legend.key.width = unit(1.2, "cm")
  ) +
  xlim(2005, 2050)

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
