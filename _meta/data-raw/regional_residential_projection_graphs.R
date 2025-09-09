#### plot residential pathways graphics

residential_pathways <- readRDS("_meta/data/residential_pathways.RDS")

plot_data <- bind_rows(
  res_bau,
  res_50,
  res_100
) %>%
  mutate()

# Create plotting categories (duplicate 2028 for continuity)
plot_data <- residential_pathways %>%
  mutate(
    total_emissions = electricity_emissions + natural_gas_emissions,
    scenario_plot = case_when(
      inventory_year <= 2028 ~ "pre2028_black",
      inventory_year >= 2028 & scenario == "bau" ~ "bau_post2028",
      inventory_year >= 2028 & scenario == "ppp" ~ "ppp",
      inventory_year >= 2028 & scenario == "nz" ~ "nz"
    )
  )

plot_data <- bind_rows(
  plot_data,
  plot_data %>% filter(inventory_year == 2028) %>%
    mutate(scenario_plot = case_when(
      scenario == "bau" ~ "bau_post2028",
      scenario == "ppp" ~ "ppp",
      scenario == "nz" ~ "nz"
    ))
)

# Prepare special points
# asterisks <- tibble::tribble(
#   ~inventory_year, ~total_emissions, ~label,
#   2030, half_2005, "*",       # 50% reduction
#   max(plot_data$inventory_year), 0, "*"   # Net-zero for "full"
# )

plot_data <- plot_data %>%
  mutate(scenario_plot = factor(scenario_plot, levels = c(
    "pre2028_black",
    "bau_post2028",
    "ppp",
    "nz"
  )))

# Plot
ggplot(plot_data, aes(x = inventory_year, y = total_emissions,
                      color = scenario_plot, linetype = scenario_plot,
                      group = interaction(scenario, scenario_plot))) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.8) +
  geom_line(linewidth = 1) +
  # geom_text(
  #   data = asterisks,
  #   aes(x = inventory_year, y = total_emissions, label = label),
  #   inherit.aes = FALSE,
  #   size = 15
  # ) +
  scale_color_manual(
    values = c(
      "pre2028_black" = "black",
      "bau_post2028" = "black",
      "ppp" = "red",
      "nz" = "blue"
    ),
    labels = c(
      "pre2028_black" = "Inventory",
      "bau_post2028" = "Business as usual",
      "ppp" = "Current strategies",
      "nz" = "Net-zero"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "pre2028_black" = "solid",
      "bau_post2028" = "dotdash",
      "ppp" = "solid",
      "nz" = "solid"
    ),
    labels = c(
      "pre2028_black" = "Inventory",
      "bau_post2028" = "Business as usual",
      "ppp" = "Current strategies",
      "nz" = "Net-zero"
    )
  ) +
  labs(
    x = "Year",
    y = "",
    title = "Residential Emissions (MT CO2e)",
    color = "Scenario",
    linetype = "Scenario"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    plot.title = element_text(size = 22, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16)
  )

ggplot(plot_data, aes(x = inventory_year, y = residential_mwh ,
                      color = scenario_plot, linetype = scenario_plot,
                      group = interaction(scenario, scenario_plot))) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.8) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c(
      "pre2028_black" = "black",
      "bau_post2028" = "black",
      "ppp" = "red",
      "nz" = "blue"
    ),
    labels = c(
      "pre2028_black" = "Inventory",
      "bau_post2028" = "Business as usual",
      "ppp" = "Current strategies",
      "nz" = "Net-zero"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "pre2028_black" = "solid",
      "bau_post2028" = "dotdash",
      "ppp" = "solid",
      "nz" = "solid"
    ),
    labels = c(
      "pre2028_black" = "Inventory",
      "bau_post2028" = "Business as usual",
      "ppp" = "Current strategies",
      "nz" = "Net-zero"
    )
  ) +
  labs(
    x = "Year",
    y = "",
    title = "Residential Electricity Demand (Megawatt Hours)",
    color = "Scenario",
    linetype = "Scenario"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    plot.title = element_text(size = 22, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16)
  )
