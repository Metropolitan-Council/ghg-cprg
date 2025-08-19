#### Create county and CTU temporary inventory graphs for 11-14-2024 steering committee meeting
source("R/_load_pkgs.R")

### county graphs

cprg_colors <- source("R/cprg_colors.R")

# create baseline and subsector sectors
county_emissions <- readRDS("_meta/data/cprg_county_emissions.RDS") 

# summarize to sector and reorder sectors
baseline_emissions_sector <- county_emissions %>%
  group_by(emissions_year, sector)  %>%
  summarize(value_emissions = sum(value_emissions, na.rm = TRUE)) %>%
  mutate(sector = factor(sector,
                         levels = c("Transportation", 
                                    "Commercial", 
                                    "Industrial",
                                    "Residential", 
                                    "Waste", 
                                    "Agriculture", 
                                    "Natural Systems")
  ))


# Plot by year
baseline_comparison <- ggplot(
  baseline_emissions_sector %>%
    filter(emissions_year >= 2005 & emissions_year <= 2022),
  aes(x = emissions_year, y = value_emissions / 1000000, col = sector)
) +
  geom_line(size = 1.6) +
  geom_hline(yintercept = 0, size = 2, col = "black", linetype = "dashed") +
  labs(fill = "sector") +
  ggtitle("Eleven-County Regional Emissions Inventory") +
  scale_color_manual(values = unlist(sector_colors), name = "Sector") +
  theme_bw() +
  xlab("") +
  ylab(expression(paste("Million metric tons of ", CO[2], "e"))) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 20),
    text = element_text(size = 20, family = "sans")
  )

baseline_comparison

baseline_comparison_facet <- ggplot(
  baseline_emissions_sector %>%
    filter(emissions_year >= 2005 & emissions_year <= 2022),
  aes(
    x = emissions_year, y = value_emissions / 1000000,
    fill = sector,
    col = sector
  )
) +
  geom_area(alpha = 0.4) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, size = 1.2, col = "black", linetype = "dashed") +
  labs(fill = "sector") +
  # ggtitle() +
  # ggsubtitle(expression(paste("Million metric tons of ", CO[2], "e"))) +
  scale_fill_manual(values = unlist(sector_colors), guide = "none") +
  scale_color_manual(values = unlist(sector_colors), guide = "none") +
  theme_bw() +
  labs(title = "Eleven-County Regional Emissions Inventory",
       subtitle = expression(paste("(Million metric tons of ", CO[2], "e)"))) +
  xlab("") +
  ylab("") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 15, angle = -90, vjust = .25),
    text = element_text(size = 20, family = "sans")
  ) +
  facet_grid(. ~ sector)

baseline_comparison_facet

# 2022 plot by subsector

county_emissions %>% distinct(sector, category) %>% print(n = 50)

category_order <- c(
  "Aviation", "Passenger vehicles", "Buses", "Trucks", # Transportation
  "Commercial electricity", "Commercial natural gas", "Commercial fuel combustion", # Commercial
  "Industrial electricity", "Industrial natural gas", "Industrial fuel combustion", "Industrial processes", "Refinery processes", # Industrial
  "Residential electricity", "Residential natural gas", # Residential
  "Solid waste", "Wastewater", # Waste
  "Livestock", "Cropland", # Agriculture
  "Freshwater", "Sequestration" # Natural Systems
)

emissions_subsector <- county_emissions %>%
  mutate(category = case_when(
    category == "Electricity" ~ str_to_sentence(paste(sector, category)),
    category == "Building Fuel" ~ str_to_sentence(paste(sector, "natural gas")),
    TRUE ~ category
  )) %>% 
  group_by(emissions_year, sector, category) %>%
  summarize(value_emissions = sum(value_emissions, na.rm = TRUE)) %>%
  mutate(sector = factor(sector, levels = c("Transportation", "Commercial", "Industrial", "Residential", "Waste", "Agriculture", "Natural Systems")))

category_colors_vector <- unlist(category_colors, use.names = TRUE)

subsector_comparison <- ggplot(
  emissions_subsector %>%
    filter(emissions_year == 2022) %>%
    mutate(
      category = factor(category, levels = category_order)
    ),
  aes(x = sector, y = value_emissions / 1000000, fill = category)
) +
  geom_bar(stat = "identity", position = "stack", col = "black") +
  labs(fill = "Subsector") +
  scale_fill_manual(values = category_colors_vector) +
  theme_minimal() +
  labs(
    title = "2022 Regional Emissions Profile",
    subtitle = expression(paste("(Million metric tons of ", CO[2], "e)")),
    x = "Sector",
    y = ""
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 20, angle = -25),
    text = element_text(size = 20, family = "sans")
  )


subsector_comparison



ggsave(plot = subsector_comparison,
       filename = paste0(wd,"/seven_county_ghg_inv_2022.png"),  # add your file path here
       width = 10,          
       height = 6,          
       units = "in",
       dpi = 300, 
       bg = "white")

### subsector by county population

emissions_sector_per_capita <- county_emissions %>%
  mutate(emissions_per_capita = value_emissions / county_total_population) %>%
  group_by(emissions_year, county_name, sector) %>%
  summarize(emissions_per_capita = sum(emissions_per_capita, na.rm = TRUE))

emissions_sector_per_county <- county_emissions %>%
  group_by(emissions_year, county_name, sector) %>%
  summarize(emissions_total = sum(value_emissions, na.rm = TRUE))

sector_colors_vector <- unlist(sector_colors, use.names = TRUE)

county_order <- county_emissions %>%
  filter(emissions_year == 2022) %>%
  group_by(county_name) %>%
  summarize(total_emissions = sum(value_emissions, na.rm = TRUE)) %>%
  arrange(desc(total_emissions)) %>%
  pull(county_name)

# Plot
sector_per_capita_comparison <- emissions_sector_per_capita %>%
  filter(emissions_year == 2022) %>%
  mutate(county_name = factor(county_name, levels = county_order)) %>%
  ggplot(aes(x = county_name, y = emissions_per_capita, fill = sector)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = sector_colors_vector) +
  coord_flip() +
  labs(
    fill = "Sector",
    x = NULL,
    y = expression(paste("Metric tons of ", CO[2], "e per capita"))
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    text = element_text(size = 16, family = "sans")
  )

sector_per_capita_comparison

sector_total_comparison <- emissions_sector_per_county %>%
  filter(emissions_year == 2022) %>%
  mutate(county_name = factor(county_name, levels = county_order)) %>%
  ggplot(aes(x = county_name, y = emissions_total, fill = sector)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = sector_colors_vector, guide = "none") +
  coord_flip() +
  labs(
    fill = "Sector",
    x = NULL,
    y = expression(paste("Metric tons of ", CO[2], "e"))
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    text = element_text(size = 16, family = "sans")
  )

sector_total_comparison
