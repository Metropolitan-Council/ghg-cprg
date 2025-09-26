#### Climate working group graphics

source("R/_load_pkgs.R")
source("R/cprg_colors.R")

### compare 2020/2021 emissions sequestration

cprg_county <- read_rds(file.path(here::here(), "_meta/data/cprg_county.RDS"))
county_emissions <- readRDS(file.path(here::here(), "_meta/data/cprg_county_emissions.RDS"))
mpca_economy_projections <- readRDS(file.path(here::here(), "_meta/data/gcam/mpca_economy_wide_gcam.RDS"))
mpca_sector_projections <- readRDS(file.path(here::here(), "_meta/data/gcam/mpca_sector_gcam.RDS"))
mpca_subsector_projections <- readRDS(file.path(here::here(), "_meta/data/gcam/mpca_subsector_gcam.RDS"))


# 2020/2021 emissions/sequestration state/metro comparison

mpca_2020 <- mpca_economy_projections %>%
  filter(emissions_year == 2020, scenario == "Current policies") %>%
  mutate(
    geography = "State",
    value_emissions = values_emissions
  ) %>%
  select(source_sink, value_emissions, geography)

mc_2021 <- county_emissions %>%
  filter(emissions_year == 2021) %>%
  mutate(source_sink = if_else(value_emissions > 0, "Emission", "Sequestration")) %>%
  group_by(source_sink) %>%
  summarize(value_emissions = sum(value_emissions) / 10^6) %>%
  mutate(geography = "Metro")

ggplot(rbind(mpca_2020, mc_2021), aes(x = source_sink, y = value_emissions, fill = source_sink)) +
  geom_bar(stat = "identity", col = "black") +
  scale_fill_manual(values = c("darkorange", "forestgreen"), guide = "none") +
  facet_wrap(. ~ geography) +
  theme_bw() +
  ylab("Million Metric Tons CO2e") +
  xlab("") +
  theme(
    text = element_text(size = 20), # Base text size
    axis.title = element_text(size = 20), # Axis titles
    axis.text = element_text(size = 20), # Axis tick labels
    strip.text = element_text(size = 20) # Facet labels
  )



### subsector downscale

### match county emissions to mpca projections

county_emissions <- county_emissions %>%
  mutate(sector = case_when(
    source == "Electricity" ~ "Electricity",
    TRUE ~ sector
  ))

## now do this by sector emissions

unique(mpca_subsector_projections$subsector_mc)
unique(county_emissions$category)

county_emissions <- county_emissions %>%
  mutate(
    subsector_mc = case_when(
      category == "Natural Gas" ~ paste(sector, "natural gas"),
      TRUE ~ category
    ),
    sector = if_else(category == "Electricity", "Electricity", sector)
  )

sort(unique(county_emissions$subsector_mc))
sort(unique(mpca_subsector_projections$subsector_mc))

# anchoring to 2020 for now
mpca_subsector_projections_use <- mpca_subsector_projections %>%
  filter(
    emissions_year >= 2025,
    # source_sink == "Emission",
    !grepl("not_inventoried", subsector_mc)
  ) %>%
  # mutate(subsector_mc = if_else(grepl("Natural Gas", subsector_mc),
  #                               "Natural Gas",
  #                               subsector_mc)) %>%
  group_by(emissions_year, scenario, subsector_mc) %>%
  summarize(proportion_of_2020 = mean(proportion_of_2020))

subsector_projections <- county_emissions %>%
  filter(
    year == 2021,
    !geog_name %in% c("Chisago", "St. Croix", "Pierce", "Sherburne")
  ) %>%
  left_join(., mpca_subsector_projections_use,
    by = "subsector_mc"
  ) %>%
  mutate(emissions_metric_tons_co2e = proportion_of_2020 * emissions_metric_tons_co2e) %>%
  group_by(emissions_year, sector, scenario) %>%
  summarize(emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e)) %>%
  ungroup()

subsector_inventory_projections <- bind_rows(
  county_emissions %>%
    filter(year == 2021) %>%
    group_by(year, sector) %>%
    summarize(emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e)) %>%
    rename(emissions_year = year) %>%
    ungroup() %>%
    mutate(scenario = "Inventory"),
  subsector_projections %>%
    mutate(emissions_year = as.numeric(emissions_year))
) %>%
  mutate(sector = str_replace_all(sector, "Nature", "Natural Systems"))


sector_colors_vector <- unlist(sector_colors, use.names = TRUE)


### match state colors and order
sector_colors_vector["Electricity"] <- "#fbdb9e" # Change to orange
sector_colors_vector["Transportation"] <- "#1984b0" # Change to green
sector_colors_vector["Residential"] <- "#ee3841" # Change to cyan
sector_colors_vector["Commercial"] <- "#f58a8f" # Change to pink
sector_colors_vector["Industrial"] <- "#954a59" # Change to yellow-green
sector_colors_vector["Waste"] <- "#8e8d80" # Change to gray
sector_colors_vector["Agriculture"] <- "#6ab245" # Change to blue
sector_colors_vector["Natural Systems"] <- "#006400" # Change to light red

subsector_inventory_projections <- subsector_inventory_projections %>%
  mutate(sector = factor(
    sector,
    levels = c(
      "Electricity",
      "Transportation",
      "Industrial",
      "Commercial",
      "Residential",
      "Agriculture",
      "Waste",
      "Natural Systems" # Top
    )
  ))

# Create the plots
ggplot(subsector_inventory_projections %>%
  filter(scenario %in% c("Inventory", "Current policies")), aes(
  x = emissions_year,
  y = emissions_metric_tons_co2e / 10^6,
  fill = sector, # Fill by sector to create stack groups
  group = sector # Ensure stacking is by sector
)) +
  geom_area(position = "stack", alpha = 0.8) + # Stacked area chart
  labs(
    title = "Subsector Current Policies Downscale", # Main title
    subtitle = "", # Subtitle
    x = "Year", # X-axis label
    y = "Million Metric Tons CO2e", # Y-axis label
    fill = "Sector" # Legend title
  ) +
  theme_minimal() + # Minimal theme for a clean look
  scale_fill_manual(values = sector_colors_vector, guide = "none") +
  theme(
    text = element_text(size = 14), # Base text size
    plot.title = element_text(size = 20, face = "bold"), # Title size
    axis.title = element_text(size = 16), # Axis label size
    legend.title = element_text(size = 16), # Legend title size
    legend.text = element_text(size = 14) # Legend text size
  )


ggplot(subsector_inventory_projections %>%
  filter(scenario %in% c("Inventory", "Net-zero pathway")), aes(
  x = emissions_year,
  y = emissions_metric_tons_co2e / 10^6,
  fill = sector, # Fill by sector to create stack groups
  group = sector # Ensure stacking is by sector
)) +
  geom_area(position = "stack", alpha = 0.8) + # Stacked area chart
  labs(
    title = "Subsector Current Policies Downscale", # Main title
    subtitle = "", # Subtitle
    x = "Year", # X-axis label
    y = "Million Metric Tons CO2e", # Y-axis label
    fill = "Sector" # Legend title
  ) +
  theme_minimal() + # Minimal theme for a clean look
  scale_fill_manual(values = sector_colors_vector) +
  theme(
    text = element_text(size = 14), # Base text size
    plot.title = element_text(size = 20, face = "bold"), # Title size
    axis.title = element_text(size = 16), # Axis label size
    legend.title = element_text(size = 16), # Legend title size
    legend.text = element_text(size = 14) # Legend text size
  )
