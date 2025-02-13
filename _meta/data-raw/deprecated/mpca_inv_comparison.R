source("R/_load_pkgs.R")

### read in state and MSA inventories
mpca_inv <- read_rds("_meta/data/mpca_ghg_inv_2005_2020.RDS") %>% mutate(geography = "State")
cprg_inv <- read_rds("_meta/data/cprg_county_emissions.RDS")
cprg_pop <- read_rds("_meta/data/cprg_population.RDS")

comp_pop <- sum(cprg_pop$population[!cprg_pop$NAME %in% c("St. Croix", "Sherburne", "Chisago", "Pierce")])

cprg_comp <- cprg_inv %>%
  filter(!geog_name %in% c("St. Croix", "Sherburne", "Chisago", "Pierce")) %>%
  group_by(sector, category, source) %>%
  summarize(co2e = sum(emissions_metric_tons_co2e)) %>%
  mutate(co2e_per_cap = co2e / comp_pop, geography = "MSA", year = 2021)

# grouping most comparable sectors/subsectors/sources for state and MSA comparisons
# Keeping 2019 and 2020 data in for transportation as 2020 was a potentially uncomparable year (2021 was also odd)

### Transportation

transportation <- bind_rows(
  cprg_comp %>%
    filter(sector %in% c("Transportation")) %>%
    ungroup() %>%
    dplyr::select(sector, subsector = source, co2e_per_cap, geography, year),
  mpca_inv %>%
    filter(Subsector %in% c("Light-duty trucks", "Passenger cars", "Heavy-duty trucks", "Motorcycle"), year %in% c(2020)) %>%
    dplyr::select(sector = Sector, subsector = Subsector, co2e_per_cap, geography, year)
)

### Energy

# both mpca and current MC inventory break out natural gas by residential, commercial, industrial
ng <- bind_rows(
  cprg_comp %>% filter(source == "Natural gas") %>%
    ungroup() %>%
    dplyr::select(sector = category, subsector = source, co2e_per_cap, geography, year),
  mpca_inv %>%
    filter(Subsector == "Natural gas" & year == 2020 & Sector %in% c("Commercial", "Residential", "Industrial")) %>%
    dplyr::select(sector = Sector, subsector = Subsector, co2e_per_cap, geography, year)
) %>%
  mutate(sector = str_remove_all(sector, " energy"), subsector = paste(sector, subsector), sector = "Energy")

# MC inventory is energy demand with eGrid mix as emission factor broken out by res, comm, ind; MPCA is energy generation broken out by fuel type
electricity <- bind_rows(
  cprg_comp %>% filter(source == "Electricity") %>%
    mutate(source = paste(str_remove_all(category, " energy"), source)) %>%
    ungroup() %>%
    dplyr::select(sector = sector, subsector = source, co2e_per_cap, geography, year),
  mpca_inv %>%
    filter(Sector == "Electricity generation" & year == 2020) %>%
    group_by(geography) %>%
    summarize(co2e_per_cap = sum(co2e_per_cap)) %>%
    mutate(sector = "Energy", subsector = "Electricity generation", year = 2020)
)

### Waste

waste <- bind_rows(
  cprg_comp %>%
    filter(sector == "Waste") %>%
    group_by(sector, category, geography, year) %>%
    summarize(co2e_per_cap = sum(co2e_per_cap)) %>%
    rename(subsector = category),
  mpca_inv %>%
    filter(Sector == "Waste" & year == 2020 & co2e > 0) %>%
    mutate(Subsector = if_else(Subsector == "Municipal wastewater", "Wastewater", "Solid waste")) %>%
    group_by(Sector, Subsector, geography, year) %>%
    summarize(co2e_per_cap = sum(co2e_per_cap)) %>%
    rename(sector = Sector, subsector = Subsector)
)

### create output RDS

inventory_comparison <- bind_rows(
  transportation, ng, electricity, waste
)

inventory_comparison_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "sector", class(inventory_comparison$sector), "Common inventory sector",
    "subsector", class(inventory_comparison$subsector), "Common inventory subsector",
    "year", class(inventory_comparison$year), "Year of inventory",
    "geography", class(inventory_comparison$geography), "Geographic scope: state or seven-county MSA",
    "co2e_per_cap", class(inventory_comparison$co2e_per_cap), "Metric tonnes of CO2 equivalency generated per capita"
  )

# saveRDS(inventory_comparison, "_meta/data/mpca_inventory_comparison.RDS")
# saveRDS(inventory_comparison_meta, "_meta/data/mpca_inventory_comparison_meta.RDS")


#### temporary ggplots
### redo color palettes and legend ordering if publishing

inventory_comparison <- inventory_comparison %>%
  mutate(sectors = if_else(grepl("Natural gas", subsector), "Natural gas",
    if_else(grepl("Electricity", subsector), "Electricity",
      sector
    )
  )) %>%
  arrange(sectors) %>%
  mutate(subsector = factor(subsector, levels = unique(subsector)))

## create color palettes for each sector
generate_shades <- function(palette_name, n) {
  if (n < 3) {
    brewer.pal(3, palette_name)[1:n]
  } else {
    brewer.pal(n, palette_name)
  }
}

# Define base colors for each sector
palette_names <- c(
  Transportation = "Oranges",
  Electricity = "Blues",
  `Natural gas` = "Greens",
  Waste = "RdPu"
)

# inventory_comparison <- left_join(inventory_comparison,
#                            inventory_comparison %>%
#   group_by(sector) %>%
#   summarise(subsectors = list(unique(subsector)), .groups = 'drop') %>%
#   mutate(
#     palette_name = palette_names[sector],
#     num_colors = map_int(subsectors, length),
#     shades = map2(palette_name, num_colors, generate_shades)
#   ) %>%
#   unnest(cols = c(subsectors, shades)) %>%
#     dplyr::select(subsector = subsectors, shades),
#   by = c("subsector"))

color_palette <- inventory_comparison %>%
  group_by(sectors) %>%
  summarise(subsectors = list(sort(unique(subsector))), .groups = "drop") %>%
  mutate(
    palette_name = palette_names[sectors],
    num_colors = map_int(subsectors, length),
    shades = map2(palette_name, num_colors, generate_shades)
  ) %>%
  unnest(cols = c(subsectors, shades))

color_palette_vector <- setNames(color_palette$shades, color_palette$subsectors)

cross_sector_comparison <- ggplot(
  inventory_comparison,
  aes(x = geography, y = co2e_per_cap, fill = subsector)
) +
  geom_bar(stat = "identity") +
  facet_wrap(~sectors) +
  theme_bw() +
  xlab("") +
  ylab("Metric tons of CO2e per capita") +
  scale_fill_manual(values = color_palette_vector) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

cross_sector_comparison


transportation_comparison <- ggplot(
  inventory_comparison %>%
    filter(sector == "Transportation"),
  aes(x = geography, y = co2e_per_cap, fill = subsector)
) +
  geom_bar(stat = "identity") +
  theme_bw() +
  xlab("") +
  ylab("Metric tons of CO2e per capita") +
  scale_fill_manual(values = color_palette_vector) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

transportation_comparison

transportation_sub <- inventory_comparison %>%
  filter(sector == "Transportation") %>%
  mutate(type = if_else(subsector %in%
    c("Light-duty vehicles", "Light-duty trucks", "Motorcycle", "Passenger cars"),
  "Passenger Vehicles", "Medium/Heavy Duty"
  ))

transportation_subcomparison <- ggplot(
  transportation_sub,
  aes(x = geography, y = co2e_per_cap, fill = subsector)
) +
  geom_bar(stat = "identity") +
  theme_bw() +
  facet_wrap(~type) +
  xlab("") +
  ylab("Metric tons of CO2e per capita") +
  scale_fill_manual(values = color_palette_vector) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


transportation_subcomparison
