#### Create county and CTU temporary inventory graphs for 11-14-2024 steering committee meeting
source("R/_load_pkgs.R")


### directory to save ggplot items in
wd <- "C:/Users/WilfahPA/OneDrive - Metropolitan Council/CPRG/Steering committee graphics/"

### county graphs

cprg_colors <- source("R/cprg_colors.R")

# create baseline and subsector sectors
county_emissions <- readRDS("_meta/data/cprg_county_emissions.RDS") %>%
  ## keep 7 counties only for CTU estimates
  # filter(!county_name %in% c("St. Croix", "Pierce", "Chisago", "Sherburne")) %>%
  mutate(baseline_sector = case_when(
    category == "Electricity" ~ "Electricity",
    category == "Building Fuel" ~ "Building Fuel",
    grepl("Commercial", category) ~ "Building Fuel",
    grepl("Industrial", category) ~ "Industrial",
    category == "Refinery processes" ~ "Industrial",
    TRUE ~ sector
  )) 
# mutate(year = case_when(
#   sector == "Industrial" & year == 2011 ~ 2005,
#   sector == "Industrial" & category == "Other" & year == 2020 ~ 2021,
#   TRUE ~ year)) %>%
# filter(year %in% c(2005, 2021))

### break out desired years and data sources for RDG 90%
baseline_emissions_sector <- county_emissions %>%
  group_by(emissions_year, baseline_sector) %>%
  summarize(MT_CO2e = sum(value_emissions, na.rm = TRUE)) %>%
  mutate(baseline_sector = factor(baseline_sector,
    levels = c( "Transportation","Electricity", "Building Fuel", "Industrial", "Waste", "Agriculture", "Natural Systems")
  ))

# Define custom colors for sectors and tones for years
baseline_colors <- c(
  "Electricity" = "#DE3163",
  "Transportation" = "#6994c1",
  "Building Fuel" = "#9467bd",
  "Industrial" = "slategray",
  "Waste" = "#8B4513",
  "Agriculture" = "#8fb910",
  "Natural Systems" = "#006f3c"
)

# Plot by year
baseline_comparison <- ggplot(
  baseline_emissions_sector %>%
    filter(emissions_year >= 2005 & emissions_year <= 2021),
  aes(x = emissions_year, y = MT_CO2e / 1000000, col = baseline_sector)
) +
  geom_line(size = 1.6) +
  geom_hline(yintercept = 0, size = 2, col = "black", linetype = "dashed") +
  labs(fill = "baseline_sector") +
  ggtitle("Eleven-County Regional Emissions Inventory") +
  scale_color_manual(values = baseline_colors, name = "Sector") +
  theme_bw() +
  xlab("") +
  ylab(expression(paste("Million metric tons of ", CO[2], "e"))) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 20),
    text = element_text(size = 20, family = "sans")
  )

baseline_comparison

# Stacked area plot by year
baseline_comparison <- ggplot(
  baseline_emissions_sector %>%
    filter(emissions_year >= 2005 & emissions_year <= 2021),
  aes(x = emissions_year, y = MT_CO2e / 1000000, fill = baseline_sector)
) +
  geom_area(size = 0.3, color = "black", position = "stack") +
  geom_hline(yintercept = 0, size = 2, col = "black", linetype = "dashed") +
  ggtitle("Eleven-County Regional Emissions Inventory") +
  scale_fill_manual(values = baseline_colors, name = "Sector") +
  theme_bw() +
  xlab("") +
  ylab(expression(paste("Million metric tons of ", CO[2], "e"))) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 20),
    text = element_text(size = 20, family = "sans")
  )

baseline_comparison

# ggsave(paste0(wd, "ghg_sector_baseline.png"),
#        baseline_comparison,
#        width = 12,
#        height = 8,
#        units = "in",
#        dpi = 600
# )

baseline_comparison_facet <- ggplot(
  baseline_emissions_sector %>%
    filter(emissions_year >= 2005 & emissions_year <= 2021),
  aes(
    x = emissions_year, y = MT_CO2e / 1000000,
    fill = baseline_sector,
    col = baseline_sector
  )
) +
  geom_area(alpha = 0.4) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, size = 1.2, col = "black", linetype = "dashed") +
  labs(fill = "baseline_sector") +
  ggtitle("Eleven-County Regional Emissions Inventory") +
  scale_fill_manual(values = baseline_colors, guide = "none") +
  scale_color_manual(values = baseline_colors, guide = "none") +
  theme_bw() +
  xlab("") +
  ylab(expression(paste("Million metric tons of ", CO[2], "e"))) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 15, angle = -90, vjust = .25),
    text = element_text(size = 20, family = "sans")
  ) +
  facet_grid(. ~ baseline_sector)

baseline_comparison_facet

# add Spanish version

baseline_emissions_sector <- mutate(baseline_emissions_sector,
  sector_spanish = case_when(
    baseline_sector == "Electricity" ~ "Electricidad",
    baseline_sector == "Transportation" ~ "Transporte",
    baseline_sector == "Building Fuel" ~ "Combustible\npara edificios",
    baseline_sector == "Industrial" ~ "Industria",
    baseline_sector == "Waste" ~ "Residuos",
    baseline_sector == "Agriculture" ~ "Agricultura",
    baseline_sector == "Natural Systems" ~ "Sistemas\nnaturales"
  )
)

baseline_comparison_facet_spanish <- ggplot(
  baseline_emissions_sector %>%
    filter(emissions_year >= 2005 & emissions_year <= 2021),
  aes(
    x = emissions_year, y = MT_CO2e / 1000000,
    fill = baseline_sector,
    col = baseline_sector
  )
) +
  geom_area(alpha = 0.4) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, size = 1.2, col = "black", linetype = "dashed") +
  labs(fill = "sector_spanish") +
  ggtitle("Inventario de emisiones de 11 condados ") +
  scale_fill_manual(values = baseline_colors, guide = "none") +
  scale_color_manual(values = baseline_colors, guide = "none") +
  theme_bw() +
  xlab("") +
  ylab(expression(paste("Millones de toneladas métricas de ", CO[2], "e"))) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 15, angle = -90, vjust = .25),
    text = element_text(size = 20, family = "sans")
  ) +
  facet_grid(. ~ baseline_sector, labeller = as_labeller(setNames(
    baseline_emissions_sector$sector_spanish,
    baseline_emissions_sector$baseline_sector
  )))


baseline_comparison_facet_spanish

# ggsave(paste0(wd, "ghg_sector_2021_spanish.png"),
#   baseline_comparison_facet_spanish,
#   width = 12,
#   height = 8,
#   units = "in"
# )

emissions_sector <- county_emissions %>%
  mutate(category = case_when(
    category == "Electricity" ~ paste(sector, "electricity"),
    category == "Natural Gas" ~ paste(sector, "natural gas"),
    TRUE ~ category
  )) %>%
  group_by(emissions_year, sector) %>%
  summarize(MT_CO2e = sum(value_emissions)) %>%
  mutate(sector = factor(sector, levels = c("Transportation", "Residential", "Commercial", "Industrial", "Waste", "Agriculture", "Natural Systems")))


# Plot by sector
sector_comparison <- ggplot(
  emissions_sector %>% filter(emissions_year == 2021),
  aes(x = sector, y = MT_CO2e / 1000000, fill = sector)
) +
  geom_bar(stat = "identity", position = position_dodge(), col = "black") +
  labs(fill = "sector") +
  ggtitle("2021 Regional Emissions Inventory") +
  scale_fill_manual(values = sector_colors, guide = "none") +
  theme_minimal() +
  xlab("") +
  ylab(expression(paste("Million metric tons of ", CO[2], "e"))) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 16),
    text = element_text(size = 20, family = "sans")
  )


sector_comparison

# ggsave(paste0(wd,"ghg_sector_2021.png"),
#        sector_comparison,
#        width = 12,
#        height = 8,
#        units = "in")



# ggsave(paste0(wd,"ghg_sector_temporal.png"),
#        baseline_comparison,
#        width = 14,
#        height = 8,
#        units = "in",
#        dpi = 400)

# Plot by subsector

category_order <- c(
  "On-road","Off-road", # Transportation
  "Residential electricity", "Residential natural gas", # Residential
  "Commercial electricity", "Commercial natural gas", "Commercial fuel combustion", # Commercial
  "Industrial electricity", "Industrial natural gas", "Industrial fuel combustion",
  "Industrial processes", "Refinery processes", # Industrial
  "Solid waste", "Wastewater", # Waste
  "Livestock", "Cropland", # Agriculture
  "Freshwater", "Sequestration" # Natural Systems
)

emissions_subsector <- county_emissions %>%
  filter(!county_name %in% c("St. Croix", "Pierce", "Sherburne", "Chisago")) %>% 
  mutate(category = case_when(
    category == "Electricity" ~ paste(sector, "electricity"),
    category == "Building Fuel" ~ paste(sector, "natural gas"),
    TRUE ~ category
  )) %>%
  group_by(emissions_year , sector, category) %>%
  summarize(MT_CO2e = sum(value_emissions)) %>%
  mutate(sector = factor(sector, levels = c("Transportation", "Residential", "Commercial", "Industrial", "Waste", "Agriculture", "Natural Systems")))

category_colors_vector <- unlist(category_colors, use.names = TRUE)

subsector_comparison <- ggplot(
  emissions_subsector %>%
    mutate(
      category = factor(category, levels = category_order)
    ) %>%
    filter(emissions_year  == 2021),
  aes(x = sector, y = MT_CO2e / 1000000, fill = category)
) +
  geom_bar(stat = "identity", position = "stack", col = "black") +
  labs(fill = "Subsector") +
  scale_fill_manual(values = category_colors_vector) +
  theme_minimal() +
  xlab("") +
  ggtitle("2021 Regional Emissions Profile") +
  ylab(expression(paste("Million metric tons of ", CO[2], "e"))) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 20, angle = -25),
    text = element_text(size = 20, family = "sans")
  )


subsector_comparison



# ggsave(paste0(wd,"ghg_subsector.png"),
#        subsector_comparison,
#        width = 14,
#        height = 8,
#        units = "in",
#        dpi = 400)

### subsector by county population

emissions_subsector_per_capita <- county_emissions %>%
  mutate(emissions_per_capita = value_emissions / county_total_population) %>%
  group_by(year, county_name, sector, category) %>%
  summarize(emissions_per_capita = sum(emissions_per_capita)) %>%
  mutate(sector = factor(sector, levels = c("Electricity", "Transportation", "Building energy", "Industrial", "Waste", "Agriculture", "Natural Systems"))) %>%
  mutate(year = if_else(
    category == "Small industrial" & year == 2020,
    2021,
    year
  ))

electricity_per_capita_comparison <- ggplot(
  emissions_subsector_per_capita %>%
    filter(year == 2021, sector == "Electricity"),
  aes(x = county_name, y = emissions_per_capita, fill = category)
) +
  geom_bar(stat = "identity", position = "stack") +
  labs(fill = "Subsector") +
  scale_fill_manual(values = category_colors_vector) +
  theme_minimal() +
  xlab("") +
  ylab(expression(paste("Metric tons of ", CO[2], "e per capita"))) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 20, angle = -90),
    text = element_text(size = 20, family = "sans")
  )

electricity_per_capita_comparison

transportation_per_capita_comparison <- ggplot(
  emissions_subsector_per_capita %>%
    filter(year == 2021, sector == "Transportation", county_name != "MSP Airport"),
  aes(x = county_name, y = emissions_per_capita, fill = category)
) +
  geom_bar(stat = "identity", position = "stack") +
  labs(fill = "Subsector") +
  scale_fill_manual(values = category_colors_vector) +
  theme_minimal() +
  xlab("") +
  ylab(expression(paste("Metric tons of ", CO[2], "e per capita"))) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 20, angle = -90),
    text = element_text(size = 20, family = "sans")
  )

transportation_per_capita_comparison

# iterate the subsector graphs across counties

# adding MSP to Hennepin for discussion
county_emissions <- county_emissions %>%
  mutate(county_name = if_else(county_name == "MSP Airport",
    "Hennepin",
    county_name
  ))

for (i in unique(county_emissions$county_name)) {
  emissions_subsector_county <- county_emissions %>%
    filter(county_name == i) %>%
    group_by(year, sector, category) %>%
    summarize(MT_CO2e = sum(value_emissions)) %>%
    mutate(sector = factor(sector, levels = c("Electricity", "Transportation", "Building energy", "Industrial", "Waste", "Agriculture", "Natural Systems"))) %>%
    mutate(year = if_else(
      category == "Small industrial" & year == 2020,
      2021,
      year
    ))

  subsector_comparison <- ggplot(
    emissions_subsector_county %>%
      mutate(
        category = factor(category, levels = emissions_subsector_county %>%
          filter(year == 2021) %>%
          arrange(sector, desc(MT_CO2e)) %>%
          pull(category) %>%
          unique())
      ) %>%
      filter(year == 2021),
    aes(x = sector, y = MT_CO2e / 1000000, fill = category)
  ) +
    geom_bar(stat = "identity", position = "stack") +
    labs(fill = "Subsector") +
    scale_fill_manual(values = category_colors_vector) +
    theme_minimal() +
    xlab("") +
    ylab(expression(paste("Million metric tons of ", CO[2], "e"))) +
    theme(
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(size = 14),
      text = element_text(size = 20, family = "sans")
    )


  subsector_comparison

  ggsave(paste0(wd, i, "_ghg_subsector.png"),
    subsector_comparison,
    width = 14,
    height = 7,
    units = "in",
    dpi = 400
  )
}


#### city emissions ####

for (i in c(
  "Bloomington",
  "Coon Rapids",
  "Carver",
  "Eagan",
  "Saint Paul",
  "Mahtomedi",
  "Minneapolis",
  "Savage"
)) {
  emissions_subsector_ctu <- ctu_emissions %>%
    filter(ctu_name == i) %>%
    group_by(emissions_year, sector, category) %>%
    summarize(MT_CO2e = sum(value_emissions)) %>%
    mutate(sector = factor(sector, levels = c("Electricity", "Transportation", "Residential", "Commercial", "Industrial", "Waste", "Agriculture", "Natural Systems")))

  subsector_comparison <- ggplot(
    emissions_subsector_ctu %>%
      mutate(
        category = factor(category, levels = emissions_subsector_ctu %>%
          filter(emissions_year == 2021) %>%
          arrange(sector, desc(MT_CO2e)) %>%
          pull(category) %>%
          unique())
      ) %>%
      filter(emissions_year == 2021),
    aes(x = sector, y = MT_CO2e / 1000000, fill = category)
  ) +
    geom_bar(stat = "identity", position = "stack") +
    labs(fill = "Subsector") +
    # scale_fill_manual(values = category_colors_vector) +
    theme_minimal() +
    xlab("") +
    ylab(expression(paste("Million metric tons of ", CO[2], "e"))) +
    theme(
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(size = 14),
      text = element_text(size = 20, family = "sans")
    )


  subsector_comparison

  ggsave(paste0(wd, i, "_ghg_subsector.png"),
    subsector_comparison,
    width = 14,
    height = 7,
    units = "in",
    dpi = 400
  )
}
