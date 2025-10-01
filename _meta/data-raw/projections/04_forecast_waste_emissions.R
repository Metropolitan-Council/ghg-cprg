# rm(list = ls())
source("R/_load_pkgs.R")
source("R/cprg_colors.R")
source("_meta/data-raw/projections/interpolate_emissions.R")

## load state gcam modeling

gcam <- read_rds("_meta/data/gcam/mpca_subsector_gcam.RDS")
unique(gcam$subsector_mc)

waste_scenarios <- gcam %>%
  filter(
    subsector_mc %in% c("Solid waste", "Wastewater"),
    scenario %in% c(
      "PPP after Fed RB",
      "CP after Fed RB"
    )
  )


waste_gcam_targets <- waste_scenarios %>%
  filter(emissions_year %in% c(2030, 2050), )




### set net-zero by regional analysis
county_emissions <- readRDS(file.path(here::here(), "_meta/data/cprg_county_emissions.RDS"))

seq_target <- readRDS(file.path(here::here(), "_meta/data/regional_net_zero_target.RDS")) %>%
  pull(net_zero_target)


waste_emissions <- county_emissions %>%
  filter(sector == "Waste")



### waste target - based on all waste emissions
# asterisk in figure later
waste_target <- (waste_emissions %>%
  filter(emissions_year == 2022) %>%
  pull(value_emissions) %>% sum() / # solid waste emissions
  county_emissions %>%
    filter(
      emissions_year == 2022,
      category != "Electricity"
    ) %>%
    pull(value_emissions) %>%
    sum()) * # regional wide emissions minus electricity
  seq_target * -1 # emissions goal


### project waste emissions based on population growth
total_pop <- read_rds("_meta/data/demographic_forecast_11_county.RDS") %>%
  filter(variable == "total_pop")

emissions_per_capita <- total_pop %>%
  filter(inventory_year == 2022) %>%
  left_join(
    waste_emissions %>%
      filter(emissions_year == 2022) %>%
      group_by(county_name) %>%
      summarise(value_emissions = sum(value_emissions)) %>%
      ungroup(),
    by = "county_name"
  ) %>%
  mutate(emissions_per_capita = value_emissions / value)

waste_emissions_bau <- total_pop %>%
  filter(inventory_year >= 2022) %>%
  left_join(
    emissions_per_capita %>%
      select(county_name, emissions_per_capita),
    by = "county_name"
  ) %>%
  mutate(
    value_emissions = value * emissions_per_capita,
    scenario = "bau"
  ) %>%
  rename(emissions_year = inventory_year) %>%
  group_by(emissions_year, scenario) %>%
  summarize(value_emissions = sum(value_emissions),.groups="keep")



waste_emissions_proj <- waste_emissions %>%
  filter(emissions_year == 2020) %>%
  group_by(sector, category) %>%
  summarize(baseline_emissions = sum(value_emissions, na.rm = "TRUE"), .groups = "keep") %>%
  left_join(
    waste_scenarios %>%
      filter(emissions_year >= 2025),
    by = c("sector",
      "category" = "subsector_mc"
    )
  ) %>%
  mutate(
    value_emissions = baseline_emissions * proportion_of_2020,
    scenario = case_when(
      scenario == "CP after Fed RB" ~ "bau",
      scenario == "PPP after Fed RB" ~ "ppp"
    ),
    emissions_year = as.numeric(emissions_year)
  ) %>%
  filter(!is.na(value_emissions)) %>%
  group_by(
    emissions_year,
    scenario
  ) %>%
  summarize(value_emissions = sum(value_emissions), .groups = "keep") %>%
  ungroup()


# combine and interpolate
waste_emissions_pathways <- interpolate_emissions(bind_rows(
  waste_emissions_bau,
  waste_emissions_proj %>%
    filter(scenario == "ppp")
))


## save bau data
waste_bau <- bind_rows(
  county_emissions %>%
    filter(
      sector %in% c("Waste"),
      emissions_year <= 2021
    ) %>% # Use any scenario since they're identical
    mutate(scenario = "bau") %>%
    group_by(emissions_year, scenario) %>%
    summarize(value_emissions = sum(value_emissions, na.rm = TRUE), .groups="keep") %>%
    ungroup(),
  waste_emissions_pathways
)

# waldo::compare(waste_bau, readRDS( "_meta/data-raw/projections/waste_pathways.rds"))
message("Saving waste projections data to: \n\t _meta/data-raw/projections/waste_pathways.rds")
saveRDS(
  waste_bau,
  "_meta/data-raw/projections/waste_pathways.rds"
)

### graph it!!####

#  base data (2005-2025, identical across scenarios)
base_data <- county_emissions %>%
  filter(
    sector %in% c("Waste"),
    # category %in% c("Solid waste"),
    emissions_year <= 2022
  ) %>% # Use any scenario since they're identical
  mutate(segment = "base", scenario = "bau") %>%
  group_by(emissions_year) %>%
  summarize(value_emissions = sum(value_emissions, na.rm = TRUE)) %>%
  ungroup()

# add 2025 point from bau scenario
bau_2025 <- waste_emissions_pathways %>%
  filter(scenario == "bau", emissions_year == 2025) %>%
  select(emissions_year, value_emissions)

# Append to base_data
extended <- base_data %>%
  bind_rows(bau_2025) %>%
  arrange(emissions_year)

# Fill in missing years by linear interpolation
base_data <- extended %>%
  complete(emissions_year = full_seq(emissions_year, 1)) %>%
  arrange(emissions_year) %>%
  mutate(value_emissions = zoo::na.approx(value_emissions, emissions_year, na.rm = FALSE))

#  diverging scenarios (2026+)
diverging_data <- waste_emissions_pathways %>%
  filter(emissions_year >= 2022) %>%
  mutate(segment = "diverging")

bau_data <- diverging_data %>% filter(scenario == "bau")

# PPP data - need to merge with net_zero for the lower bound
ppp_data <- diverging_data %>%
  filter(scenario == "ppp") %>%
  select(emissions_year, value_emissions) %>%
  rename(ppp_emissions = value_emissions)


# Create the plot
emissions_gg <- ggplot() +
  # Base fill (2005-2025, gray)
  geom_ribbon(
    data = base_data,
    aes(x = emissions_year, ymin = 0, ymax = value_emissions),
    fill = "gray80", alpha = 0.7
  ) +

  # Net zero fill (#36454F)
  # geom_ribbon(data = net_zero_data,
  #             aes(x = emissions_year, ymin = 0, ymax = value_emissions),
  #             fill = "#36454F", alpha = 0.3) +

  # PPP fill (from net_zero to ppp)
  geom_ribbon(
    data = ppp_data,
    aes(x = emissions_year, ymin = 0, ymax = ppp_emissions),
    fill = "orange", alpha = 0.5
  ) +

  # Base line (2005-2025)
  geom_line(
    data = base_data,
    aes(x = emissions_year, y = value_emissions),
    color = "black", size = 1
  ) +

  # Diverging scenario lines
  geom_line(
    data = diverging_data %>% filter(scenario == "bau"),
    aes(x = emissions_year, y = value_emissions, color = "Business as usual"),
    linetype = "dashed", size = 1
  ) +
  geom_line(
    data = diverging_data %>% filter(scenario == "ppp"),
    aes(x = emissions_year, y = value_emissions, color = "Potential policy pathways"),
    size = 1
  ) +
  geom_point(
    data = data.frame(emissions_year = 2050, value_emissions = waste_target),
    aes(x = emissions_year, y = value_emissions),
    shape = "*", # asterisk
    size = 12, # make larger or smaller
    stroke = 1.5, # line thickness of the asterisk
    color = "black"
  ) +
  geom_segment(aes(x = 2025, xend = 2025, y = 0, yend = base_data %>% filter(emissions_year == 2025) %>% pull(value_emissions)),
    color = "black", linetype = "solid", linewidth = 0.8
  ) +
  # annotate("text", x = 2025, y = max(your_data$value_emissions) * 0.9,
  #          label = "Historical | Projected", angle = 90, hjust = 1, size = 3.5) +

  # Manual color scale with correct order
  scale_color_manual(
    values = c(
      "Business as usual" = "black",
      # "Net zero" = "#36454F",
      "Potential policy pathways" = "orange"
    ),
    breaks = c("Business as usual", "Potential policy pathways") # Force legend order
  ) +

  # Manual legend guide to show line types
  guides(
    color = guide_legend(
      title = "Scenarios",
      override.aes = list(
        linetype = c("dashed", "solid"),
        color = c("black", "orange")
      )
    )
  ) +
  labs(
    x = "Year",
    y = "",
    title = "Waste Emissions \n(Millions of CO2-equivalency)"
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


message("Saving waste projections plot to: \n\t ~/imgs/waste_decarbonization_pathways.png")
ggplot2::ggsave(
  plot = emissions_gg,
  filename = paste0(here::here(), "/imgs/waste_decarbonization_pathways.png"), 
  width = 12,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)

### numbers for CCAP document
# sector wide 2030/2050 scenario to BAU comparisons

waste_2030 <- waste_emissions_proj %>%
  filter(emissions_year == 2030)

bau2030 <- waste_2030 %>%
  filter(scenario == "bau") %>%
  pull(value_emissions)

ppp2030 <- waste_2030 %>%
  filter(scenario == "ppp") %>%
  pull(value_emissions) -
  bau2030

# nz2030 <- waste_2030 %>% filter(scenario == "nz") %>% pull(value_emissions) -
# bau2030

ppp2030
ppp2030 / bau2030
# nz2030 / bau2030

# 2050

waste_2050 <- waste_emissions_pathways %>%
  filter(emissions_year == 2050)

bau2050 <- waste_2050 %>%
  filter(scenario == "bau") %>%
  pull(value_emissions)

ppp2050 <- waste_2050 %>%
  filter(scenario == "ppp") %>%
  pull(value_emissions) -
  bau2050

nz2050 <- waste_target - bau2050

ppp2050
nz2050
ppp2050 / bau2050
nz2050 / bau2050



message("Finished waste projections")

