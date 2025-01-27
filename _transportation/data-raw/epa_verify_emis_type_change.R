# verify differences when we include all emis_types instead of only rate-per-distance
# 2020 will remain the same, because 2020 already included all emis_types
# Otherwise, we expect that emissions will increase
#

source("R/_load_pkgs.R")

# download specific version of epa_onroad_emissions_compile
# from the dev-2005-baseline branch
file_name <- tempfile()
download.file("https://github.com/Metropolitan-Council/ghg-cprg/raw/17b4349fc5874562befaa56a85ef3f740bb1708a/_transportation/data/epa_onroad_emissions_compile.RDS",
  destfile = file_name, mode = "wb"
)

# read in and create previous version aggregation
prev_epa_onroad_emissions_compile <- readRDS(file_name)
prev_onroad_emissions <- prev_epa_onroad_emissions_compile %>%
  # only get CO2e
  filter(
    pollutant == "emissions_metric_tons_co2e",
    !vehicle_type %in% c("Gas stations", "Trucks and buses")
  ) %>%
  unique() %>%
  group_by(
    emissions_year, data_source, geoid, county_name,
    vehicle_weight_label,
    vehicle_fuel_label,
    category
  ) %>%
  # group and summarize emissions
  summarize(
    emissions_metric_tons_co2e = sum(emissions, na.rm = TRUE),
    vehicle_types_included = paste0(unique(vehicle_type), collapse = ", "),
    fuel_types_included = paste0(unique(fuel_type), collapse = ", "),
    scc6_included = paste(unique(scc6), collapse = ", "),
    .groups = "keep"
  ) %>%
  ungroup()

# read in new version and aggregate
epa_onroad_emissions_compile <- readRDS("_transportation/data/epa_onroad_emissions_compile.RDS")
onroad_emissions <- epa_onroad_emissions_compile %>%
  # only get CO2e
  filter(
    pollutant == "emissions_metric_tons_co2e",
    !vehicle_type %in% c("Gas stations", "Trucks and buses")
  ) %>%
  unique() %>%
  group_by(
    emissions_year, data_source, geoid, county_name,
    vehicle_weight_label,
    vehicle_fuel_label,
    category
  ) %>%
  # group and summarize emissions
  summarize(
    emissions_metric_tons_co2e = sum(emissions, na.rm = TRUE),
    vehicle_types_included = paste0(unique(vehicle_type), collapse = ", "),
    fuel_types_included = paste0(unique(fuel_type), collapse = ", "),
    scc6_included = paste(unique(scc6), collapse = ", "),
    .groups = "keep"
  ) %>%
  ungroup()

# compare differences
onroad_compare <- full_join(onroad_emissions,
  prev_onroad_emissions,
  join_by(
    emissions_year, data_source, geoid, county_name, vehicle_weight_label, vehicle_fuel_label, category,
    vehicle_types_included, fuel_types_included, scc6_included
  ),
  suffix = c("_new", "_old")
) %>%
  mutate(
    emissions_diff = emissions_metric_tons_co2e_new - emissions_metric_tons_co2e_old,
    emissions_pct_diff = emissions_diff / emissions_metric_tons_co2e_old
  )


onroad_compare %>%
  group_by(vehicle_weight_label) %>%
  summarize(avg_change = median(emissions_pct_diff, na.rm = T))

onroad_compare %>%
  plot_ly(
    type = "box",
    boxpoints = "all",
    x = ~emissions_pct_diff,
    y = ~vehicle_weight_label,
    color = ~vehicle_fuel_label,
    colors = "Dark2",
    marker = list(
      opacity = 0.4,
      size = 10
    ),
    hoverinfo = "text",
    hovertext = ~ paste0(
      county_name, " County, ", emissions_year, "<br>",
      vehicle_types_included, "<br>",
      fuel_types_included, "<br>",
      scales::percent(round(emissions_pct_diff, digits = 3))
    )
  ) %>%
  plotly_layout(
    main_title = "Change in emissions after including all process types",
    x_title = "% increase in CO<sub>2</sub>e",
    legend_title = "Fuel type"
  ) %>%
  plotly::layout(
    xaxis = list(tickformat = "1%"),
    legend = list(
      traceorder = "reversed",
      orientation = "h",
      y = -0.15
    ),
    boxmode = "group",
    traceorder = "reversed"
  )
