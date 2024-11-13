### county graphs
county_emissions <- readRDS("_meta/data/cprg_county_emissions.RDS") %>% 
  filter(year %in% c(2005, 2013, 2021))


### break out desired years and data sources for RDG 90%
emissions_rdg_90_baseline <- county_emissions %>% 
  group_by(year, sector) %>% 
  summarize(MT_CO2e = sum(emissions_metric_tons_co2e)) %>% 
  mutate(sector = factor(sector, levels = c("Electricity", "Transportation", "Building energy", "Industrial", "Waste", "Agriculture", "Nature")))


#### remove later

baseline_comparison <- ggplot(emissions_rdg_90_baseline %>% filter(year %in% c(2005, 2021)),
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
  "Industrial.2021" = unname(sector_colors["Industrial"]),
  "Waste.2005" = lighten(sector_colors["Waste"], 1.4),
  "Waste.2021" = unname(sector_colors["Waste"]),
  "Agriculture.2005" = lighten(sector_colors["Agriculture"], 1.4),
  "Agriculture.2021" = unname(sector_colors["Agriculture"]),
  "Nature.2005" = lighten(sector_colors["Nature"], 1.4),
  "Nature.2021" = unname(sector_colors["Nature"])
)

emissions_rdg_90_baseline <- emissions_rdg_90_baseline %>%
  mutate(sector_year = interaction(sector, as.factor(year), sep = "."))
ylab(expression(paste("µg ", CO[2], " - C ", m^-2, " ", h^-1, sep="")))

# Plot with custom settings
sector_comparison <- ggplot(emissions_rdg_90_baseline %>% filter(year %in% c(2021)),
                            aes(x = sector, y = MT_CO2e/1000000, fill = sector)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  labs(fill = "sector") +
  scale_fill_manual(values = sector_colors, guide = "none") +
  theme_minimal() + xlab("") + ylab(expression(paste("Million metric tons of ",CO[2],"e"))) +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(size = 20),
        text = element_text(size = 20, family="sans"))


sector_comparison

## subsector graph example ####

electricity_subsector <- emissions_all %>% 
  group_by(year, geog_name , sector, category ) %>% 
  summarize(MT_CO2e = sum(emissions_metric_tons_co2e)) %>% 
  left_join(., cprg_county_pop %>% group_by(county_name, population_year) %>% summarize(population = sum(population)),
            by = c('year' = 'population_year', "geog_name" = "county_name")
  ) %>% 
  mutate(emissions_per_capita = MT_CO2e / population) %>%
  #mutate(sector = factor(sector, levels = c("Electricity", "Transportation", "Building energy", "Waste", "Agriculture", "Nature"))) %>% 
  filter(sector == "Electricity", year == 2021)

subsector_county_comparison <- ggplot(electricity_subsector,
                                      aes(x = MT_CO2e/1000000, y = reorder(geog_name, desc(geog_name)), 
                                          fill = category)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = c("#9DB9F1", "#4479E4", "#16439C"), guide = FALSE) +
  theme(panel.grid.major.y = element_blank(),
        axis.text.x = element_text(size = 20),
        text = element_text(size = 20, family="sans")
  ) +
  ylab("") +
  xlab(expression(paste("Million metric tons of ",CO[2],"e"))) +
  labs(fill = "Subsector")

subsector_county_comparison

### per capita

subsector_county_comparison_per_capita <- ggplot(electricity_subsector,
                                                 aes(x = emissions_per_capita, y = reorder(geog_name, desc(geog_name)), 
                                                     fill = category)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = c("#9DB9F1", "#4479E4", "#16439C")) +
  theme(panel.grid.major.y = element_blank(),
        axis.text.x = element_text(size = 20),
        text = element_text(size = 20, family="sans")
  ) +
  xlab(expression(paste("Metric tons of ",CO[2],"e per capita"))) +
  ylab("") +
  labs(fill = "Subsector")

subsector_county_comparison_per_capita

### baseline comparison ####

baseline_comparison <- ggplot(emissions_rdg_90_baseline,
                              aes(x = year, y = MT_CO2e/1000000, 
                                  col = as.factor(sector),
                                  shape = as.factor(sector))) +
  theme_minimal() +
  scale_color_manual(values = sector_colors) +
  geom_hline(yintercept = 0, col = "black", lty = 2, size = 1.3) +
  geom_line(size = 1) + geom_point(size = 4) + theme_minimal() + 
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_text(size = 20),
        text = element_text(size = 20, family="sans")
  ) +
  xlab("Year") + 
  ylab(expression(paste("Million metric tons of ",CO[2],"e"))) +
  labs(col = "Sector",
       shape = "Sector")

baseline_comparison

msa_subsector_inv <- emissions_all %>% 
  filter(year  == 2021) %>% 
  group_by(year,geog_name,sector, category,county_total_population ) %>%
  summarise(
    emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e),
    .groups = "keep"
  ) %>% 
  mutate(emissions_per_capita = emissions_metric_tons_co2e / county_total_population) %>% 
  ungroup() %>% 
  select(-county_total_population)


#quick calcs
emissions_rdg_90_baseline %>% filter(year == 2021, sector == "Transportation") %>% pull(MT_CO2e) %>% sum() /
  emissions_rdg_90_baseline %>% filter(year == 2005, sector != "Nature") %>% pull(MT_CO2e) %>% sum()

emissions_rdg_90_baseline %>% filter(year == 2021, sector == "Waste") %>% pull(MT_CO2e) %>% sum() /
  emissions_rdg_90_baseline %>% filter(year == 2005, sector != "Nature") %>% pull(MT_CO2e) %>% sum()

emissions_rdg_90_baseline %>% filter(year == 2021, sector == "Nature") %>% pull(MT_CO2e) %>% sum() /
  emissions_rdg_90_baseline %>% filter(year == 2005, sector != "Nature") %>% pull(MT_CO2e) %>% sum()



emissions_all_meta <- tibble::tribble(
  ~"Column", ~"Class", ~"Description",
  "year", class(emissions_all$year), "Emissions estimation year",
  "geog_level", class(emissions_all$geog_level), "Geography level; city or county",
  "geog_id", class(emissions_all$geog_id), "FIPS code",
  "geog_name", class(emissions_all$geog_name), "Name of geographic area",
  "sector", class(emissions_all$sector), paste0(
    "Emissions sector. One of ",
    paste0(unique(emissions_all$sector), collapse = ", ")
  ),
  "category", class(emissions_all$category), "Category of emissions within given sector",
  "source", class(emissions_all$source), "Source of emissions. Most detailed sub-category in this table",
  "emissions_metric_tons_co2e", class(emissions_all$emissions_metric_tons_co2e), "Annual total metric tons CO~2~ and CO~2~ equivalent attributed to the given geography for given year",
  "data_source", class(emissions_all$data_source), "Activity data source",
  "factor_source", class(emissions_all$factor_source), "Emissions factor data source",
  "county_total_population", class(emissions_all$county_total_population), "Total geography population",
  "population_data_source", class(emissions_all$population_data_source), "Population data source",
  "emissions_per_capita", class(emissions_all$emissions_per_capita), "Metric tons CO~2~e per person living in given county for given sector and category"
)

saveRDS(emissions_all, "_meta/data/cprg_county_emissions.RDS")
saveRDS(emissions_all_meta, "_meta/data/cprg_county_emissions_meta.RDS")
write.csv(emissions_all, "_meta/data/cprg_county_emissions.CSV", row.names = FALSE)
write.csv(emissions_rdg_90_baseline, "_meta/data/baseline_emissions_rdg.csv", row.names = FALSE)


saveRDS(carbon_stock, "_meta/data/cprg_county_carbon_stock.RDS")
saveRDS(emissions_all_meta, "_meta/data/cprg_county_carbon_stock_meta.RDS")

# save emissions to shared drive location
# source("R/fetch_path.R")

# if (fs::dir_exists(fetch_path())) {
#   write.csv(emissions_all, paste0(fetch_path(), "/cprg_county_emissions.CSV"), row.names = FALSE)
# }

msa_subsector_inv <- emissions_all %>% 
  filter(year  == 2021) %>% 
  group_by(year,geog_name,sector, category,county_total_population ) %>%
  summarise(
    emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e),
    .groups = "keep"
  ) %>% 
  mutate(emissions_per_capita = emissions_metric_tons_co2e / county_total_population) %>% 
  ungroup() %>% 
  select(-county_total_population)

write.csv(msa_subsector_inv, "_meta/data/subsector_emissions_rdg.csv", row.names = FALSE)

msa_subsector_inv %>% filter(year == 2021, sector == "Transportation", category == "Light-duty vehicles") %>% pull(emissions_metric_tons_co2e) %>% sum() / 
  msa_subsector_inv %>% filter(year == 2021, sector == "Transportation") %>% pull(emissions_metric_tons_co2e) %>% sum() 



### sequestration by area

cprg_area <- cprg_county %>%
  mutate(area_sq_mi = sf::st_area(cprg_county) %>% units::set_units("mi^2") %>%
           as.numeric()) %>% select(NAME, area_sq_mi) %>% st_drop_geometry()

msa_sequestration <- left_join(emissions_all %>% 
                                 filter(year  == 2021, !geog_name %in% c("Sherburne", "Chisago", "St. Croix", "Pierce"), sector == "Nature"),
                               cprg_area, 
                               by = c("geog_name" = "NAME")) %>% 
  group_by(year,geog_name,sector, source,area_sq_mi) %>%
  summarise(
    sequestration_metric_tons_co2e = sum(emissions_metric_tons_co2e),
    .groups = "keep"
  ) %>% 
  mutate(sequestration_per_sq_mi = sequestration_metric_tons_co2e / area_sq_mi) %>% 
  ungroup() %>% 
  select(-area_sq_mi)

county_seq <- ggplot(
  msa_sequestration,
  aes(x = geog_name, y = sequestration_per_sq_mi / 1000000, fill = source)
) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = color_palette_vector) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.45, size = 14),
    axis.title.y = element_text(size = 16)
  ) +
  ylab("Millions of metric tons of CO2e") +
  xlab("") +
  labs(fill = "Subsector")
