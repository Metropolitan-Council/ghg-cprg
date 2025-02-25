#### Climate working group graphics

source("R/_load_pkgs.R")
source("R/cprg_colors.R")

### compare 2020/2021 emissions sequestration

cprg_county <- read_rds(file.path(here::here(), "_meta/data/cprg_county.RDS"))
county_emissions <- readRDS(file.path(here::here(), "_meta/data/cprg_county_emissions.RDS"))
mpca_economy_projections <- readRDS(file.path(here::here(), "_meta/data/gcam/mpca_economy_wide_gcam.RDS"))
mpca_sector_projections <- readRDS(file.path(here::here(), "_meta/data/gcam/mpca_sector_gcam.RDS"))
mpca_subsector_projections <- readRDS(file.path(here::here(), "_meta/data/gcam/mpca_subsector_gcam.RDS"))

### hold for processing imagine 2050 forecasts

imagine <- read_xlsx(file.path(here::here(), "_meta/data-raw/imagine_2050_forecasts.xlsx"), skip = 3) %>% 
  clean_names()

## bring in households for residential growth and jobs for commercial/industrial?
pop_trends <- imagine[1:201,1:8] %>% 
  pivot_longer(cols = contains("population"),
               values_to = "population",
               names_to = "projection_year") %>% 
  mutate(projection_year = as.numeric(str_extract(projection_year, "\\d+"))) %>% 
  #just keep counties and regional totals for now
  filter(grepl("County", city_or_township) | grepl("region", city_or_township)) %>% 
  group_by(county) %>%
  mutate(pop_2020 = population[projection_year == 2020],
         pop_percent_2020 = (population / pop_2020)) %>% 
  ungroup()

# 2020/2021 emissions/sequestration state/metro comparison

mpca_2020 <- mpca_economy_projections %>%
  filter(emissions_year == 2020, scenario == "Current policies") %>%
  mutate(
    geography = "State (MN)",
    value_emissions = values_emissions
  ) %>%
  select(source_sink, value_emissions, geography)

mc_2021 <- county_emissions %>%
  filter(emissions_year == 2021) %>%
  mutate(source_sink = if_else(value_emissions > 0, "Emission", "Sequestration")) %>%
  group_by(source_sink) %>%
  summarize(value_emissions = sum(value_emissions) / 10^6) %>%
  mutate(geography = "11-county region")

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
    category == "Electricity" ~ "Electricity",
    TRUE ~ sector
  ))

## now do this by sector emissions

unique(mpca_subsector_projections$subsector_mc)
unique(county_emissions$category)


county_emissions <- county_emissions %>%
  mutate(
    subsector_mc = case_when(
      sector == "Transportation" ~ source,
      category == "Building Fuel" ~ source,
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

population_sectors <- c("Electricity", "Residential", "Commercial", "Industry", "Transportation", "Waste")

population_projections <- expand.grid(
  emissions_year = as.numeric(c(2020, unique(mpca_subsector_projections_use$emissions_year))),
  sector = unique(mpca_subsector_projections$sector)
) %>% 
  left_join(pop_trends %>% 
              filter(county == "Region") %>% 
              select(projection_year, pop_percent_2020),
              by = c("emissions_year" = "projection_year")) %>% 
  mutate(pop_percent_2020 = approx(emissions_year, pop_percent_2020, emissions_year, rule = 2)$y,
         pop_percent_2020 = if_else(sector %in% population_sectors, pop_percent_2020, 1)) %>% 
  left_join(mpca_subsector_projections %>% 
              distinct(sector, subsector_mc)) %>% 
  filter(
    emissions_year >= 2025,
    # source_sink == "Emission",
    !grepl("not_inventoried", subsector_mc)
  ) %>% 
  select(emissions_year, subsector_mc, proportion_of_2020 = pop_percent_2020) %>% 
  mutate(scenario = "Business as usual")

mpca_subsector_projections_use <- bind_rows(mpca_subsector_projections_use %>% 
                                              mutate(emissions_year = as.numeric(emissions_year)),
                                            population_projections)

subsector_projections <- county_emissions %>%
  filter(
    emissions_year == 2021
  ) %>%
  select(-emissions_year) %>% 
  left_join(., mpca_subsector_projections_use,
    by = "subsector_mc"
  ) %>%
  mutate(value_emissions = proportion_of_2020 * value_emissions) %>%
  group_by(emissions_year, sector, scenario) %>%
  summarize(value_emissions = sum(value_emissions)) %>%
  ungroup()

subsector_inventory_projections <- bind_rows(
  county_emissions %>%
    filter(emissions_year == 2021) %>%
    group_by(emissions_year, sector) %>%
    summarize(value_emissions = sum(value_emissions)) %>%
    ungroup() %>%
    mutate(scenario = "Inventory"),
  subsector_projections %>%
    mutate(emissions_year = as.numeric(emissions_year))
)


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


## define path to save graphics

save_path <- ""

# Create the plots

#Business as usual
bau <- ggplot(subsector_inventory_projections %>%
         filter(scenario %in% c("Inventory", "Business as usual")), aes(
           x = emissions_year,
           y = value_emissions / 10^6,
           fill = sector, # Fill by sector to create stack groups
           group = sector # Ensure stacking is by sector
         )) +
  geom_area(position = "stack", alpha = 0.8) + # Stacked area chart
  labs(
    title = "Business As Usual", # Main title
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

ggsave(paste0(save_path, "regional_bau.tiff"),
       width = 5, height = 6,
       bau, dpi = 300)

# Current policies
cp <- ggplot(subsector_inventory_projections %>%
  filter(scenario %in% c("Inventory", "Current policies")), aes(
  x = emissions_year,
  y = value_emissions / 10^6,
  fill = sector, # Fill by sector to create stack groups
  group = sector # Ensure stacking is by sector
)) +
  geom_area(position = "stack", alpha = 0.8) + # Stacked area chart
  labs(
    title = "Current Policies (MN)", # Main title
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

ggsave(paste0(save_path, "regional_cp.tiff"),
       width = 5, height = 6,
       cp, dpi = 300)

# Net zero
nz <- ggplot(subsector_inventory_projections %>%
  filter(scenario %in% c("Inventory", "Net-zero pathway")), aes(
  x = emissions_year,
  y = value_emissions / 10^6,
  fill = sector, # Fill by sector to create stack groups
  group = sector # Ensure stacking is by sector
)) +
  geom_area(position = "stack", alpha = 0.8) + # Stacked area chart
  labs(
    title = "State Net-Zero", # Main title
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

ggsave(paste0(save_path, "regional_net_zero.tiff"),
       width = 6.6, height = 6,
       nz, dpi = 300)

#Net-zero 2050 numbers
subsector_inventory_projections %>%
  filter(scenario  == "Net-zero pathway",
         emissions_year == 2050,
         sector == "Natural Systems") %>% 
  pull(value_emissions) %>% sum()

### map combined projections together

combined_proj <- subsector_inventory_projections %>% 
  filter(sector != "Natural Systems") %>% 
  group_by(emissions_year, scenario) %>% 
  summarize(total_emissions = sum(value_emissions)) %>% 
  bind_rows(., subsector_inventory_projections %>% 
              filter(sector == "Natural Systems",
                     scenario %in% c("Inventory","Net-zero pathway")) %>% 
              select(-sector) %>% 
              mutate(scenario = "Sequestration") %>% 
              rename(total_emissions = value_emissions)) %>% 
 filter(!is.na(total_emissions)) %>% 
  ungroup()

inventory_2021 <- combined_proj %>%
  filter(scenario == "Inventory") %>%
  select(emissions_year, total_emissions)

combined_proj <- combined_proj %>%
  bind_rows(
    combined_proj %>%
      filter(scenario != "Sequestration") %>%
      distinct(scenario) %>%  
      mutate(emissions_year = 2021) %>%
      left_join(inventory_2021, by = "emissions_year") 
  ) %>%
  arrange(scenario, emissions_year) 

scenario_colors <- c(
  "Business as usual" = "black",
  "Current policies" = "blue",
  "Net-zero pathway" = "red",
  "Sequestration" = "darkgreen"
)

# Create the updated plot
scenarios_plot <- ggplot(combined_proj, aes(
  x = emissions_year,
  y = total_emissions / 10^6,
  group = scenario,
  color = scenario  # Color lines by scenario
)) +
  geom_line(size = 1.2) +  # Bolder lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 1) +  # Horizontal line at y=0
  scale_color_manual(values = scenario_colors) +  # Apply custom colors
  labs(
    title = "Regional Emission Scenarios",
    x = "Year",
    y = "Million Metric Tons CO2e",
    color = "Scenario",  # Update legend title
    linetype = "Scenario"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(size = 22, face = "bold"),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    panel.grid.major = element_line(color = "gray85", linetype = "dashed"),
    panel.grid.minor = element_blank()
  )

scenarios_plot

ggsave(paste0(save_path, "regional_scenarios.tiff"),
       scenarios_plot, dpi = 300)
