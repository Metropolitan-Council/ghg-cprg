#### plot residential pathways graphics

source("R/_load_pkgs.R")

residential_pathways <- readRDS("_meta/data/residential_pathways.RDS")

total_emissions <- residential_pathways %>%
    mutate(total_emissions = electricity_emissions + natural_gas_emissions)%>%
  mutate(scenario = factor(scenario,
                           levels = c("bau","ppp","nz")))

#  base data (2005-2025, identical across scenarios)
base_data <- total_emissions %>%
  filter(inventory_year <= 2025, scenario == "bau") %>%  # Use any scenario since they're identical
  mutate(segment = "base")

#  diverging scenarios (2026+)
diverging_data <- total_emissions %>%
  filter(inventory_year >= 2025) %>%
  mutate(segment = "diverging")

net_zero_data <- diverging_data %>% filter(scenario == "nz")

# PPP data - need to merge with net_zero for the lower bound
ppp_data <- diverging_data %>%
  filter(scenario == "ppp") %>%
  select(inventory_year, total_emissions) %>%
  rename(ppp_emissions = total_emissions)

net_zero_for_ppp <- diverging_data %>%
  filter(scenario == "nz") %>%
  select(inventory_year, total_emissions) %>%
  rename(net_zero_emissions = total_emissions)

ppp_ribbon_data <- ppp_data %>%
  left_join(net_zero_for_ppp, by = "inventory_year")

# Create the plot
emissions_gg <- ggplot() +
  # Base fill (2005-2025, gray)
  geom_ribbon(data = base_data,
              aes(x = inventory_year, ymin = 0, ymax = total_emissions),
              fill = "gray80", alpha = 0.7) +
  
  # Net zero fill (maroon)
  geom_ribbon(data = net_zero_data,
              aes(x = inventory_year, ymin = 0, ymax = total_emissions),
              fill = "maroon", alpha = 0.3) +
  
  # PPP fill (from net_zero to ppp)
  geom_ribbon(data = ppp_ribbon_data,
              aes(x = inventory_year, ymin = net_zero_emissions, ymax = ppp_emissions),
              fill = "rosybrown1", alpha = 0.5) +
  
  # Base line (2005-2025)
  geom_line(data = base_data,
            aes(x = inventory_year, y = total_emissions),
            color = "black", size = 1) +
  
  # Diverging scenario lines
  geom_line(data = diverging_data %>% filter(scenario == "bau"),
            aes(x = inventory_year, y = total_emissions, color = "Business as usual"),
            linetype = "dashed", size = 1) +
  
  geom_line(data = diverging_data %>% filter(scenario == "ppp"),
            aes(x = inventory_year, y = total_emissions, color = "Accelerated policy pathways"),
            size = 1) +
  
  geom_line(data = diverging_data %>% filter(scenario == "nz"),
            aes(x = inventory_year, y = total_emissions, color = "Net zero"),
            size = 1) +
  
  geom_segment(aes(x = 2025, xend = 2025, y = 0, yend = base_data %>% filter(inventory_year == 2025) %>% pull(total_emissions)),
               color = "black", linetype = "solid", size = 0.8) +
  # annotate("text", x = 2025, y = max(your_data$total_emissions) * 0.9,
  #          label = "Historical | Projected", angle = 90, hjust = 1, size = 3.5) +
  
  # Manual color scale with correct order
  scale_color_manual(
    values = c(
      "Business as usual" = "black",
      "Net zero" = "maroon",
      "Accelerated policy pathways" = "rosybrown3"
    ),
    breaks = c("Business as usual", "Accelerated policy pathways", "Net zero")  # Force legend order
  ) +
  
  # Manual legend guide to show line types
  guides(
    color = guide_legend(
      title = "Scenarios",
      override.aes = list(
        linetype = c("dashed", "solid", "solid"),
        color = c("black", "rosybrown3", "maroon")
      )
    )
  ) +
  labs(
    x = "Year",
    y = "",
    title = "Residential Building Emissions \n(Millions of CO2-equivalency)"
  ) +
  scale_y_continuous(labels = label_number(scale = 1e-6)) +  # convert to millions
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 18),
    axis.text =  element_text(size = 14),
    legend.text = element_text(size = 18),
    legend.key.width = unit(1.2, "cm")
  ) +
  xlim(2005, 2050)

print(emissions_gg)

ggplot2::ggsave(plot = emissions_gg,
                filename = paste0(wd,"/residential_decarbonization_pathways.png"),  # add your file path here
                width = 12,
                height = 6,
                units = "in",
                dpi = 300,
                bg = "white")
