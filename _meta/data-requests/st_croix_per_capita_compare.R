# St. Croix County data request: per capita GHG comparison with Pierce,
# Carver, Scott (requested) and Washington (added as a neighbor).
#
# Two outputs, long by sector with a total row per county-year:
#   1. Economy-wide: all sectors except Natural Systems (gross emissions)
#   2. Non-point: also excludes the Industrial sector and heavy-duty trucks,
#      keeping light commercial trucks (MOVES source type 32), to better
#      reflect distributed, community-scale emissions.
#
# Notes:
#   - Per capita uses gross emissions; net (with sequestration/freshwater)
#     is included as a separate total row for reference.
#   - Years limited to those with population (<= 2022).
#   - Truck emissions for WI counties step up sharply in 2020+ due to EPA
#     data source changes (see transportation notes); the non-point file
#     removes most of that effect.

source(file.path(here::here(), "R/_load_pkgs.R"))

counties <- c("St. Croix", "Pierce", "Carver", "Scott", "Washington")
max_year <- 2022
out_dir <- here::here("_meta", "data-requests", "outputs")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

emissions <- read_rds(here::here("_meta", "data", "cprg_county_emissions.RDS")) %>%
  filter(county_name %in% counties, emissions_year <= max_year) %>%
  mutate(category = as.character(category))

population <- emissions %>%
  distinct(emissions_year, county_name, county_total_population)

# Light commercial trucks, to keep in the non-point dataset
light_commercial_trucks <- read_rds(here::here(
  "_transportation", "data", "epa_onroad_emissions_compile.RDS"
)) %>%
  filter(
    county_name %in% counties,
    emissions_year <= max_year,
    category == "Trucks",
    vehicle_type == "Light commercial trucks",
    pollutant == "emissions_metric_tons_co2e"
  ) %>%
  group_by(emissions_year, county_name) %>%
  summarise(value_emissions = sum(emissions), .groups = "drop") %>%
  mutate(sector = "Transportation", category = "Light commercial trucks")

# --- helper: sector totals + total rows + per capita -------------------------

summarise_per_capita <- function(df, dataset_label) {
  by_sector <- df %>%
    group_by(emissions_year, county_name, sector) %>%
    summarise(value_emissions = sum(value_emissions, na.rm = TRUE), .groups = "drop")

  gross_total <- by_sector %>%
    filter(sector != "Natural Systems") %>%
    group_by(emissions_year, county_name) %>%
    summarise(value_emissions = sum(value_emissions), .groups = "drop") %>%
    mutate(sector = "Total (gross)")

  net_total <- by_sector %>%
    group_by(emissions_year, county_name) %>%
    summarise(value_emissions = sum(value_emissions), .groups = "drop") %>%
    mutate(sector = "Total (net of natural systems)")

  bind_rows(by_sector, gross_total, net_total) %>%
    left_join(population, by = c("emissions_year", "county_name")) %>%
    mutate(
      dataset = dataset_label,
      emissions_per_capita = value_emissions / county_total_population
    ) %>%
    select(
      dataset, emissions_year, county_name, sector,
      value_emissions, county_total_population, emissions_per_capita
    ) %>%
    arrange(county_name, emissions_year, sector)
}

# --- 1. economy-wide -----------------------------------------------------------

economy_wide <- summarise_per_capita(emissions, "Economy-wide")

# --- 2. non-point: no Industrial sector, no heavy-duty trucks ----------------

non_truck <- emissions %>%
  filter(
    category != "Trucks"
  ) %>%
  bind_rows(light_commercial_trucks) %>%
  summarise_per_capita("Excluding heavy-duty trucks")

# --- write -----------------------------------------------------------------------

write_csv(economy_wide, file.path(out_dir, "st_croix_comparison_per_capita_economy_wide.csv"))
write_csv(non_truck, file.path(out_dir, "st_croix_comparison_per_capita_non_truck.csv"))

# --- category per capita table (single year, four requested counties) ---------
# Wide table: rows = category, columns = county, values = t CO2e per person
# (gross; natural systems excluded). Two versions: with all trucks, and with
# heavy-duty trucks removed (light commercial trucks retained).

table_year <- 2022
table_counties <- c("St. Croix", "Pierce", "Carver", "Scott")

build_category_table <- function(df) {
  df %>%
    filter(
      emissions_year == table_year,
      county_name %in% table_counties,
      sector != "Natural Systems"
    ) %>%
    group_by(county_name, sector, category) %>%
    summarise(value_emissions = sum(value_emissions, na.rm = TRUE), .groups = "drop") %>%
    left_join(
      population %>% filter(emissions_year == table_year),
      by = "county_name"
    ) %>%
    mutate(per_capita = value_emissions / county_total_population) %>%
    select(sector, category, county_name, per_capita) %>%
    bind_rows(
      (.) %>%
        group_by(county_name) %>%
        summarise(per_capita = sum(per_capita), .groups = "drop") %>%
        mutate(sector = "Total", category = "Total (gross)")
    ) %>%
    mutate(per_capita = round(per_capita, 2)) %>%
    pivot_wider(names_from = county_name, values_from = per_capita, values_fill = 0) %>%
    select(sector, category, all_of(table_counties)) %>%
    arrange(sector == "Total", sector, desc(`St. Croix`))
}

per_capita_category_table <- build_category_table(emissions)

per_capita_category_table_no_hd_trucks <- emissions %>%
  filter(category != "Trucks") %>%
  bind_rows(light_commercial_trucks) %>%
  build_category_table()

write_csv(
  per_capita_category_table,
  file.path(out_dir, paste0("st_croix_per_capita_by_category_", table_year, ".csv"))
)
write_csv(
  per_capita_category_table_no_hd_trucks,
  file.path(out_dir, paste0("st_croix_per_capita_by_category_no_hd_trucks_", table_year, ".csv"))
)

per_capita_category_table
per_capita_category_table_no_hd_trucks

# --- simple plot: the four requested counties -----------------------------------

plot_data <- bind_rows(economy_wide, non_truck) %>%
  filter(
    sector == "Total (gross)",
    county_name %in% c("St. Croix", "Pierce", "Carver", "Scott")
  ) %>%
  mutate(dataset = factor(
    dataset,
    c("Economy-wide", "Excluding heavy-duty trucks")
  ))

st_croix_per_capita_plot <- ggplot(
  plot_data,
  aes(emissions_year, emissions_per_capita, color = county_name)
) +
  geom_line(linewidth = 1) +
  facet_wrap(~dataset) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "Per capita greenhouse gas emissions",
    subtitle = "Gross emissions (excludes natural systems sequestration)",
    x = NULL, y = "Metric tons CO2e per person", color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

st_croix_per_capita_plot
