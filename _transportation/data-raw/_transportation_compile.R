# Compile onroad and nonroad transportation emissions for inclusion in
# compile_county_emissions.R
source("R/_load_pkgs.R")

# onroad emissions -----
epa_onroad_emissions_compile <- readRDS("_transportation/data/epa_onroad_emissions_compile.RDS")
epa_onroad_emissions_compile_meta <- readRDS("_transportation/data/epa_onroad_emissions_compile_meta.RDS")
epa_onroad_source_set <- readRDS("_transportation/data/epa_onroad_source_set.RDS") %>%
  mutate(emissions_year = as.numeric(emissions_year))


onroad_emissions <- epa_onroad_emissions_compile %>%
  # only get CO2e
  filter(
    pollutant == "emissions_metric_tons_co2e",
    !vehicle_type %in% c("Gas stations", "Trucks and buses")
  ) %>%
  group_by(
    emissions_year, data_source, geoid, county_name, vehicle_weight_label,
    vehicle_fuel_label
  ) %>%
  # group and summarize emissions
  summarize(
    emissions_metric_tons_co2e = sum(emissions, na.rm = TRUE),
    vehicle_types_included = paste0(unique(vehicle_type), collapse = ", "),
    fuel_types_included = paste0(unique(fuel_type), collapse = ", "),
    scc6_included = paste(unique(scc6), collapse = ", "),
    .groups = "keep"
  ) %>%
  inner_join(
    epa_onroad_source_set %>%
      select(data_source, emissions_year, moves_edition) %>%
      unique(),
    by = join_by(emissions_year, data_source)
  ) %>%
  ungroup()

onroad_emissions_meta <- epa_onroad_emissions_compile_meta %>%
  bind_rows(
    tribble(
      ~"Column", ~"Class", ~"Description",
      "emissions_metric_tons_co2e", class(onroad_emissions$emissions_metric_tons_co2e), "Annual total metric tons CO~2~ and CO~2~ equivalent attributed to the given county",
      "scc6_included", class(onroad_emissions$scc6_included), "Original fuel and vehicle types SCCs included in aggregation",
      "fuel_types_included", class(onroad_emissions$fuel_types_included), "Original SCC fuel types included in aggregation",
      "vehicle_types_included", class(onroad_emissions$vehicle_types_included), "Original SCC vehicle types included in aggregation",
      "moves_edition", class(onroad_emissions$moves_edition), "MOVES version used by EPA"
    )
  ) %>%
  filter(Column %in% names(onroad_emissions)) %>%
  unique()

saveRDS(onroad_emissions, "_transportation/data/onroad_emissions.RDS")
saveRDS(onroad_emissions_meta, "_transportation/data/onroad_emissions_meta.RDS")

# Nonroad emissions -----
