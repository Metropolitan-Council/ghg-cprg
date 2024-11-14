#### Create county and CTU temporary inventory graphs for 11-14-2024 steering committee meeting

### directory to save ggplot items in

wd <- "C:/Users/WilfahPA/OneDrive - Metropolitan Council/CPRG/Steering committee graphics/November meeting/"

### county graphs

cprg_colors <- source("R/cprg_colors.R")

county_emissions <- readRDS("_meta/data/cprg_county_emissions.RDS") %>% 
  mutate(category = case_when(
    sector == "Nature" & grepl("Urban", source) ~ "Urban greenery",
    sector == "Nature" & !grepl("Urban", source) ~ "Natural systems",
    category == "Other" ~ "Small industrial",
    TRUE ~ category
  ))
  # mutate(year = case_when(
  #   sector == "Industrial" & year == 2011 ~ 2005,
  #   sector == "Industrial" & category == "Other" & year == 2020 ~ 2021, 
  #   TRUE ~ year)) %>% 
  # filter(year %in% c(2005, 2021))

ctu_emissions <- readRDS("_meta/data/ctu_emissions.RDS")

### break out desired years and data sources for RDG 90%
emissions_sector <- county_emissions %>% 
  group_by(year, sector) %>% 
  summarize(MT_CO2e = sum(emissions_metric_tons_co2e)) %>% 
  mutate(sector = factor(sector, levels = c("Electricity", "Transportation", "Building energy", "Industrial", "Waste", "Agriculture", "Nature")))

baseline_comparison <- ggplot(emissions_sector %>% filter(year %in% c(2005, 2021)),
                              aes(x = sector, y = MT_CO2e, fill = as.factor(year))) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  labs(fill = "Year")

# Define custom colors for sectors and tones for years
sector_colors <- c("Electricity" = "#1f77b4", 
                   "Transportation" = "#8c564b", 
                   "Building energy" = "#9467bd", 
                   "Industrial" = "slategray",
                   "Waste" = "#d62728", 
                   "Agriculture" = "#ff7f0e", 
                   "Nature" = "#2ca02c")

palette_names <- c(
  Electricity = "Blues",
  `Building energy` = "Purples",
  Transportation = "Browns",
  Waste = "Reds",
  Industrial = "Grays",
  Agriculture = "Oranges",
  Nature = "Greens"
)


# Function to lighten colors for different years
lighten <- function(color, factor=1.4){
  col <- col2rgb(color)
  col <- col + (255 - col) * (factor - 1)
  col[col > 255] <- 255
  rgb(col[1], col[2], col[3], maxColorValue=255)
}

# Custom palette for years
custom_palette <- c(
  "Electricity.2005" = lighten(sector_colors["Electricity"], 1.4),
  "Electricity.2021" = unname(sector_colors["Electricity"]),
  "Transportation.2005" = lighten(sector_colors["Transportation"], 1.4),
  "Transportation.2021" = unname(sector_colors["Transportation"]),
  "Building energy.2005" = lighten(sector_colors["Building energy"], 1.4),
  "Building energy.2021" = unname(sector_colors["Building energy"]),
  "Industrial.2005" = lighten(sector_colors["Industrial"], 1.4),
  "Industrial.2021" = unname(sector_colors["Industrial"]),
  "Waste.2005" = lighten(sector_colors["Waste"], 1.4),
  "Waste.2021" = unname(sector_colors["Waste"]),
  "Agriculture.2005" = lighten(sector_colors["Agriculture"], 1.4),
  "Agriculture.2021" = unname(sector_colors["Agriculture"]),
  "Nature.2005" = lighten(sector_colors["Nature"], 1.4),
  "Nature.2021" = unname(sector_colors["Nature"])
)

emissions_sector <- emissions_sector %>%
  mutate(sector_year = interaction(sector, as.factor(year), sep = "."))

# Plot by sector
sector_comparison <- ggplot(emissions_sector %>% filter(year == 2021),
                            aes(x = sector, y = MT_CO2e/1000000, fill = sector)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  labs(fill = "sector") +
  scale_fill_manual(values = sector_colors, guide = "none") +
  theme_minimal() + xlab("") + ylab(expression(paste("Million metric tons of ",CO[2],"e"))) +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(size = 16),
        text = element_text(size = 20, family="sans"))


sector_comparison

# ggsave(paste0(wd,"ghg_sector_2021.png"),
#        sector_comparison,
#        width = 12,
#        height = 8,
#        units = "in")

# Plot by year
baseline_comparison <- ggplot(emissions_sector %>% 
                                filter(year >= 2005 & year <= 2021),
                            aes(x = year, y = MT_CO2e/1000000, col = sector)) +
  geom_line(size = 1.6) +
  geom_hline(yintercept = 0, size = 2, col = "black", linetype = "dashed")+
  labs(fill = "sector") +
  scale_color_manual(values = sector_colors) +
  theme_bw() + xlab("") + ylab(expression(paste("Million metric tons of ",CO[2],"e"))) +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(size = 20),
        text = element_text(size = 20, family="sans"))

baseline_comparison

# ggsave(paste0(wd,"ghg_sector_temporal.png"),
#        baseline_comparison,
#        width = 14,
#        height = 8,
#        units = "in",
#        dpi = 400)

# Plot by subsector

emissions_subsector <- county_emissions %>% 
  group_by(year, sector, category) %>% 
  summarize(MT_CO2e = sum(emissions_metric_tons_co2e)) %>% 
  mutate(sector = factor(sector, levels = c("Electricity", "Transportation", "Building energy", "Industrial", "Waste", "Agriculture", "Nature"))) %>% 
  mutate(year = if_else(
    category == "Small industrial" & year == 2020,
    2021,
    year
  ))

category_colors_vector <- unlist(category_colors, use.names = TRUE)

subsector_comparison <- ggplot(
  emissions_subsector %>%
    mutate(
      category = factor(category, levels = emissions_subsector %>%
                          filter(year == 2021) %>%
                          arrange(sector, desc(MT_CO2e)) %>%
                          pull(category) %>%
                          unique())
    ) %>%
    filter(year == 2021),
  aes(x = sector, y = MT_CO2e / 1000000, fill = category)
) +
  geom_bar(stat = 'identity', position = 'stack') +
  labs(fill = "Subsector") +
  scale_fill_manual(values = category_colors_vector) +
  theme_minimal() +
  xlab("") +
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

# iterate the subsector graphs across counties

# adding MSP to Hennepin for discussion
county_emissions <- county_emissions %>% 
  mutate(geog_name = if_else(geog_name == "MSP Airport",
                             "Hennepin",
                             geog_name))

for(i in unique(county_emissions$geog_name)) {
  
  emissions_subsector_county <- county_emissions %>% 
    filter(geog_name == i) %>% 
    group_by(year, sector, category) %>% 
    summarize(MT_CO2e = sum(emissions_metric_tons_co2e)) %>% 
    mutate(sector = factor(sector, levels = c("Electricity", "Transportation", "Building energy", "Industrial", "Waste", "Agriculture", "Nature"))) %>% 
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
    geom_bar(stat = 'identity', position = 'stack') +
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

  ggsave(paste0(wd,i,"_ghg_subsector.png"),
         subsector_comparison,
         width = 14,
         height = 7,
         units = "in",
         dpi = 400)
  
}


#### city emissions ####

for(i in c("Bloomington",
           "Coon Rapids",
           "Carver",
           "Eagan",
           "Saint Paul",
           "Mahtomedi",
           "Minneapolis",
           "Savage")) {
  
  emissions_subsector_ctu <- ctu_emissions %>% 
    filter(ctu_name == i) %>% 
    group_by(emissions_year, sector, category) %>% 
    summarize(MT_CO2e = sum(emissions_metric_tons_co2e)) %>% 
    mutate(sector = factor(sector, levels = c("Electricity", "Transportation", "Building energy", "Industrial", "Waste", "Agriculture", "Nature")))
  
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
    geom_bar(stat = 'identity', position = 'stack') +
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
  
  ggsave(paste0(wd,i,"_ghg_subsector.png"),
         subsector_comparison,
         width = 14,
         height = 7,
         units = "in",
         dpi = 400)
  
}


