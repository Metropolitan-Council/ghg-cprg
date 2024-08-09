# this was ultimately a moot exercise in comparing the data sources
# As noted in the on-road documentation for the NEI (@usepa2020NationalEmissions2023),
# NEI is bottom up,
# GHG Inventory is top-down (starting from fuel consumption,
# which is then apportioned to vehicle and fuel types).
# They cannot be reconciled, regardless of whether they are aggregated to the state level.

source("_transportation/data-raw/epa_nei_transportation.R")
source("_transportation/data-raw/epa_ghg_inventory.R")

county_proportions <- read_rds("_meta/data/cprg_county_proportions.RDS")

county_proportions_summary <- county_proportions %>%
  group_by(STATE, year) %>%
  summarize(region_proportion_of_state_pop = sum(county_proportion_of_state_pop))

state_ipcc_summary <- ipcc_transportation %>%
  group_by(inventory_year) %>%
  summarize(state_emissions = sum(emissions_metric_tons_co2e))

state_economic_summary <- state_economic %>%
  filter(
    # inventory_year %in% epa_nei$nei_inventory_year,
    sector_group == "Transportation",
    `Sector/Source` %in% c(
      "Mobile Combustion",
      "CO2 from Fossil Fuel Combustion",
      "Non-Energy Use of Fuels"
    )
  ) %>%
  group_by(inventory_year) %>%
  summarize(state_emissions = sum(emissions_metric_tons_co2e, na.rm = T))

# nei_summary <- epa_nei %>%
#   mutate(nei_inventory_year = as.character(nei_inventory_year)) %>%
#   left_join(
#     cprg_county %>%
#       mutate(state = STATE) %>%
#       sf::st_drop_geometry(),
#     by = c("county_name" = "NAME")
#   ) %>%
#   group_by(nei_inventory_year) %>%
#   summarize(
#     region_emissions = sum(emissions_metric_tons_co2e, na.rm = T),
#     .groups = "keep"
#   )

nei_state_summary <- nei_state_emissions %>%
  mutate(nei_inventory_year = as.character(nei_inventory_year)) %>%
  group_by(nei_inventory_year) %>%
  summarize(
    region_emissions = sum(emissions_metric_tons_co2e, na.rm = T),
    .groups = "keep"
  ) %>%
  ungroup()

inventory_comp <- state_economic_summary %>%
  left_join(nei_summary,
    by = c("state",
      "inventory_year" = "nei_inventory_year"
    )
  ) %>%
  left_join(county_proportions_summary, by = c(
    "inventory_year" = "year",
    "state" = "STATE"
  )) %>%
  # ungroup() %>%
  mutate(regional_proportion = region_emissions / state_emissions)


state_economic_summary

plot_ly(
  name = "Inventory",
  type = "scatter",
  mode = "lines+markers",
  data = state_ipcc_summary %>%
    filter(inventory_year >= 2008),
  x = ~inventory_year,
  y = ~state_emissions
) %>%
  add_trace(
    name = "NEI",
    type = "scatter",
    inherit = FALSE,
    mode = "lines+markers",
    data = nei_state_summary,
    x = ~nei_inventory_year,
    y = ~region_emissions,
    line = list(
      dash = "dot"
    )
  ) %>%
  plotly_layout(
    main_title = "Significant differences in NEI and Inventory underlying datasets",
    subtitle = "MN + WI state-level summary"
  )
