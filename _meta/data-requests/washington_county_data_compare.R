# output washington county data 

source(file.path(here::here(), "R/_load_pkgs.R"))

washington_out <- read_rds("_meta/data/cprg_county_emissions.rds") %>% 
  filter(county_name == "Washington", emissions_year <= 2022)

# outputs are git-ignored (_meta/data-requests/outputs/)
out_dir <- here::here("_meta", "data-requests", "outputs")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

write_csv(washington_out, file.path(out_dir, "washington_county_data_request_2026.csv"))

# previous delivery for comparison; copy it into the outputs folder to run
# the old-vs-new comparisons below
washington_old_path <- file.path(out_dir, "washington_county_data_request.csv")
if (!file.exists(washington_old_path)) {
  stop("Place the previous Washington delivery at: ", washington_old_path)
}
washington_old <- read_csv(washington_old_path)

# Summarize 2022 by sector for each dataset
sum_out <- washington_out %>%
  filter(emissions_year == 2022) %>%
  group_by(sector) %>%
  summarise(total = sum(value_emissions, na.rm = TRUE)) %>%
  mutate(version = "out")

sum_old <- washington_old %>%
  filter(emissions_year == 2022) %>%
  group_by(sector) %>%
  summarise(total = sum(value_emissions, na.rm = TRUE)) %>%
  mutate(version = "old")

bind_rows(sum_out, sum_old) %>%
  ggplot(aes(x = reorder(sector, -total), y = total / 1e3, fill = version)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("out" = "#2c7bb6", "old" = "#d7191c"),
                    labels = c("out" = "New version", "old" = "Previous version")) +
  labs(
    title = "Washington County 2022 Emissions by Sector",
    x = NULL,
    y = "Emissions (thousand metric tons CO₂e)",
    fill = "Dataset"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    legend.position = "top"
  )

washington_old %>% filter(emissions_year == 2022, sector == "Waste")
washington_out %>% filter(emissions_year == 2022, sector == "Waste")

washington_old %>% filter(emissions_year == 2022, sector == "Agriculture")
washington_out %>% filter(emissions_year == 2022, sector == "Agriculture")

washington_old %>% filter(emissions_year == 2022, sector == "Industrial")
washington_out %>% filter(emissions_year == 2022, sector == "Industrial")

washington_old %>% filter(emissions_year == 2022, sector == "Transportation")
washington_out %>% filter(emissions_year == 2022, sector == "Transportation")

# washinton trends

washington_economy <- washington_out %>% 
  group_by(emissions_year, county_name) %>% 
  summarise(value_emissions = sum(value_emissions),
            county_total_population = max(county_total_population),
            .groups = "drop") %>% 
  mutate(per_capita = value_emissions / county_total_population)

washington_trend <- washington_economy %>% filter(emissions_year == 2022) %>% pull(value_emissions) /
  washington_economy %>% filter(emissions_year == 2005) %>% pull(value_emissions)

washington_trend_per_cap <- washington_economy %>% filter(emissions_year == 2022) %>% pull(per_capita) /
  washington_economy %>% filter(emissions_year == 2005) %>% pull(per_capita)

washington_sector <- washington_out %>% 
  group_by(emissions_year, county_name, sector) %>% 
  summarise(value_emissions = sum(value_emissions),
            county_total_population = max(county_total_population),
            .groups = "drop")

## county comparisons

county_emissions <- read_rds("_meta/data/cprg_county_emissions.rds") %>% 
  filter(emissions_year <= 2022,
         county_name %in% c("Carver",
                            "Dakota",
                            "Scott",
                            "Hennepin",
                            "Ramsey",
                            "Washington",
                            "Anoka"))

county_emissions_economy <- county_emissions %>% 
  group_by(emissions_year, county_name) %>% 
  summarise(value_emissions = sum(value_emissions, na.rm = TRUE),
            county_total_population = max(county_total_population),
            .groups = "drop") %>% 
  mutate(per_capita = value_emissions / county_total_population)

county_emissions_economy_no_refinery <- county_emissions %>% 
  filter(!category %in% c("Industrial natural gas",
                          "Refinery processes")) %>% 
  group_by(emissions_year, county_name) %>% 
  summarise(value_emissions = sum(value_emissions, na.rm = TRUE),
            county_total_population = max(county_total_population),
            .groups = "drop") %>% 
  mutate(per_capita = value_emissions / county_total_population)

county_pct <- county_emissions_economy %>%
  group_by(emissions_year) %>%
  mutate(
    pct_emissions = value_emissions / sum(value_emissions) * 100,
    pct_population = county_total_population / sum(county_total_population) * 100
  ) %>% 
  ungroup()

# Filter to 2022 and compute percent of total
county_pct_2022 <- county_emissions_economy %>%
  filter(emissions_year == 2022) %>%
  mutate(
    pct_emissions = value_emissions / sum(value_emissions) * 100,
    pct_population = county_total_population / sum(county_total_population) * 100
  )

# Pivot longer for grouped bar chart
county_pct_long <- county_pct_2022 %>%
  select(county_name, pct_emissions, pct_population) %>%
  pivot_longer(
    cols = c(pct_emissions, pct_population),
    names_to = "metric",
    values_to = "pct"
  ) %>%
  mutate(
    metric = factor(
      recode(metric,
             "pct_emissions"  = "Emissions",
             "pct_population" = "Population"
      ),
      levels = c( "Population", "Emissions")
    )
  )


# Build chart
p_county_pct_2022 <- ggplot(county_pct_long, aes(x = county_name, y = pct, fill = metric)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(
    aes(label = paste0(round(pct, 1), "%")),
    position = position_dodge(width = 0.7),
    vjust = -0.4, size = 3
  ) +
  scale_fill_manual(values = c("Emissions" = "#8766f1", "Population" = "#87a3ef")) +
  scale_y_continuous(labels = \(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "County Share of Regional Emissions vs. Population",
    subtitle = "2022 \u00b7 Seven-county Twin Cities metro \u00b7 Emissions include refineries",
    x = NULL,
    y = "Percent of metro total",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.justification = "right",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey50", size = 10),
    axis.text.x = element_text(size = 10)
  )


p_county_pct_2022
