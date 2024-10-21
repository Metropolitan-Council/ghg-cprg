# get data from the 2020 national emissions inventory
# using EnviroFacts API
source("R/_load_pkgs.R")
source("R/global_warming_potential.R")
source("_meta/data-raw/county_geography.R")

cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_county_meta <- read_rds("_meta/data/cprg_county_meta.RDS")
epa_moves <- readRDS("_transportation/data/epa_moves.RDS")

if (!exists("nei_county_multi_year")) {
  source("_meta/data-raw/epa_nei.R")
}
# mobile sectors only
mobile_sectors <- sectors %>%
  filter(sector_one == "Mobile") %>%
  mutate(
    nei_sector_code = sector_code,
    vehicle_weight_label = case_when(
      ei_sector %in% c(
        "Mobile - On-Road Diesel Light Duty Vehicles",
        "Mobile - On-Road non-Diesel Light Duty Vehicles"
      ) ~ "Light-duty",
      ei_sector %in% c(
        "Mobile - On-Road non-Diesel Heavy Duty Vehicles",
        "Mobile - On-Road Diesel Heavy Duty Vehicles"
      ) ~ "Heavy-duty",
      TRUE ~ "Other or not applicable"
    ) %>%
      factor(
        levels = c(
          "Light-duty",
          "Medium-duty",
          "Heavy-duty",
          "Other or not applicable"
        ),
        ordered = TRUE
      ),
    vehicle_type = case_when(
      vehicle_weight_label == "Heavy-duty" ~ "commercial",
      vehicle_weight_label == "Light-duty" ~ "passenger",
      TRUE ~ "Other or not applicable"
    ),
    vehicle_fuel_label =
      case_when(
        str_detect(sector_three, "non-Diesel") ~ "Non-Diesel",
        str_detect(sector_three, "Diesel") ~ "Diesel",
        str_detect(sector_three, "Gasoline") ~ "Gasoline",
        TRUE ~ "Other or not applicable"
      ),
    vehicle_group =
      case_when(
        sector_two == "On-Road" ~ "On-Road",
        sector_two == "Non-Road" ~ "Non-Road Equipment",
        TRUE ~ sector_two
      )
  )

nei_state_emissions <- nei_county_multi_year %>%
  mutate(nei_sector_code = sector_code) %>%
  filter(
    nei_sector_code %in% mobile_sectors$nei_sector_code,
    pollutant_type == "GHG"
  ) %>%
  left_join(mobile_sectors,
    by = join_by(
      nei_sector_code, ei_sector,
      sector_one, sector_two, sector_three
    )
  ) %>%
  group_by(
    state_name,
    nei_inventory_year, nei_sector_code,
    pollutant_type,
    vehicle_type,
    vehicle_group, vehicle_fuel_label, vehicle_weight_label,
    sector_three,
    pollutant_code
  ) %>%
  summarize(
    emissions_grams = sum(emissions_grams),
    .groups = "keep"
  ) %>%
  pivot_wider(
    names_from = pollutant_code,
    values_from = emissions_grams,
    values_fill = 0
  ) %>%
  ungroup() %>%
  clean_names() %>%
  mutate(
    co2_co2_equivalent = co2 + (ch4 * gwp$ch4) + (n2o * gwp$n2o),
    emissions_metric_tons_co2e = co2_co2_equivalent / 1000000
  )

# check unit of measurement
# https://www.epa.gov/air-emissions-inventories/what-are-units-nei-emissions-data
nei_county_emissisons <- nei_county_multi_year %>%
  mutate(nei_sector_code = sector_code) %>%
  filter(
    nei_sector_code %in% mobile_sectors$nei_sector_code,
    pollutant_type == "GHG"
  ) %>%
  left_join(mobile_sectors,
    by = join_by(
      nei_sector_code, ei_sector,
      sector_one, sector_two, sector_three
    )
  ) %>%
  pivot_wider(
    names_from = pollutant_code,
    values_from = emissions_grams,
    id_cols = c(
      state_name,
      geoid,
      cprg_area,
      ei_sector,
      nei_inventory_year,
      nei_sector_code,
      pollutant_type,
      vehicle_type,
      vehicle_group,
      vehicle_fuel_label,
      vehicle_weight_label,
      sector_three,
      sector_two
    ),
    values_fill = 0
  ) %>%
  clean_names() %>%
  rowwise() %>%
  # n2o and ch4 to co2 equivalency
  mutate(
    co2_co2_equivalent = co2 + (ch4 * gwp$ch4) + (n2o * gwp$n2o),
    emissions_metric_tons_co2e = co2_co2_equivalent / 1000000
  )

# aggregate by vehicle weight and county
epa_nei_envirofacts <- nei_county_emissisons %>%
  left_join(county_geography %>%
    select(geoid,
      county_name,
      county_fips = countyfp
    )) %>%
  unique() %>%
  group_by(
    geoid, county_name,
    vehicle_weight_label,
    vehicle_type,
    vehicle_group,
    vehicle_fuel_label,
    nei_sector_code,
    sector_two,
    state_name, cprg_area,
    county_fips,
    nei_inventory_year
  ) %>%
  summarize(
    total_co2 = sum(co2, na.rm = T),
    total_ch4 = sum(ch4, na.rm = T),
    total_n2o = sum(n2o, na.rm = T),
    total_co2_w_equiv = sum(co2_co2_equivalent, na.rm = T),
    emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e, na.rm = T),
    .groups = "keep"
  ) %>%
  ungroup()


epa_nei_envirofacts_meta <- tibble::tribble(
  ~"Column", ~"Class", ~"Description",
  "nei_inventory_year", class(epa_nei_envirofacts$nei_inventory_year), "NEI inventory year",
  "total_co2", class(epa_nei_envirofacts$total_co2), "Annual total grams of CO~2~  attributed to the given county",
  "total_ch4", class(epa_nei_envirofacts$total_ch4), "Annual total grams of CH~4~  attributed to the given county",
  "total_n2o", class(epa_nei_envirofacts$total_n2o), "Annual total grams of N~2~O  attributed to the given county",
  "vehicle_weight_label", paste0(class(epa_nei_envirofacts$vehicle_weight_label), collapse = " "), "\"Light-duty\", \"Medium-duty\", \"Heavy-duty\", or \"Other or not applicable\"",
  "vehicle_fuel_label", class(epa_nei_envirofacts$vehicle_fuel_label), "Diesel, non-diesel, gasoline, or other fuel",
  "vehicle_type", class(epa_nei_envirofacts$vehicle_type), "\"passenger\" or \"commercial\"",
  "vehicle_group", class(epa_nei_envirofacts$vehicle_group), "Vehicle group",
  "nei_sector_code", class(epa_nei_envirofacts$nei_sector_code), "NEI sector code",
  "total_co2_w_equiv", class(epa_nei_envirofacts$total_co2_w_equiv), "Annual total grams of CO~2~ and CO~2~ equivalent attributed to the given county",
  "emissions_metric_tons_co2e", class(epa_nei_envirofacts$emissions_metric_tons_co2e), "Annual total metric tons CO~2~ and CO~2~ equivalent attributed to the given county"
) %>%
  bind_rows(cprg_county_meta) %>%
  filter(Column %in% names(epa_nei_envirofacts)) %>%
  unique()


# waldo::compare(epa_nei_envirofacts, readRDS("_transportation/data/epa_nei_envirofacts.RDS"))
# waldo::compare(epa_nei_envirofacts_meta, readRDS("_transportation/data/epa_nei_envirofacts_meta.RDS"))


saveRDS(epa_nei_envirofacts, "_transportation/data/epa_nei_envirofacts.RDS")
saveRDS(epa_nei_envirofacts_meta, "_transportation/data/epa_nei_envirofacts_meta.RDS")



# create imputed data values -----

epa_nei_envirofacts_full <- epa_nei_envirofacts %>%
  ungroup() %>%
  select(
    -starts_with("total"),
    -state_name
  ) %>%
  filter(cprg_area == TRUE) %>%
  group_by(
    geoid, county_name, cprg_area, county_fips,
    vehicle_type,
    vehicle_weight_label, vehicle_group,
    vehicle_fuel_label, nei_sector_code
  ) %>%
  complete(nei_inventory_year = 2005:2022) %>%
  # we only have a full-ish series for onroad and
  # nonroad equipment.
  filter(!vehicle_group %in% c(
    "Aircraft",
    "Locomotives",
    "Commercial Marine Vessels"
  )) %>%
  unique() %>%
  mutate(interp_emissions = na_kalman(emissions_metric_tons_co2e,
    smooth = TRUE,
    type = "trend"
  )) %>%
  mutate(nei_data_source = ifelse(is.na(emissions_metric_tons_co2e),
    "EPA NEI, interpolated",
    "EPA NEI, reported"
  ))



# quickly plot original and interpolated values
epa_nei_envirofacts_full %>%
  group_by(nei_inventory_year, geoid, county_name, county_fips) %>%
  summarize(
    interp_emissions = sum(interp_emissions) %>%
      round(digits = 0),
    emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e) %>%
      round(digits = 0),
    .groups = "keep"
  ) %>%
  ungroup() %>%
  plot_ly(
    type = "scatter",
    mode = "lines+markers",
    x = ~nei_inventory_year,
    y = ~interp_emissions,
    color = ~county_name
  ) %>%
  add_trace(
    type = "scatter",
    mode = "markers",
    y = ~emissions_metric_tons_co2e,
    marker = list(
      color = "black"
    )
  )



epa_nei_envirofacts_complete <- epa_nei_envirofacts_full %>%
  select(geoid, county_fips, county_name, cprg_area,
    nei_inventory_year, nei_data_source,
    vehicle_weight_label,
    vehicle_fuel_label,
    vehicle_group,
    vehicle_type,
    nei_sector_code,
    emissions_metric_tons_co2e = interp_emissions
  ) %>%
  filter(vehicle_group == "On-Road")

epa_nei_envirofacts_complete_meta <- tibble::tribble(
  ~"Column", ~"Class", ~"Description",
  "nei_data_source", class(epa_nei_envirofacts_complete$nei_data_source), "Emissions esimtate data source, either reported NEI value or interplated NEI value"
) %>%
  bind_rows(
    cprg_county_meta,
    epa_nei_envirofacts_meta
  ) %>%
  filter(Column %in% names(epa_nei_envirofacts_complete)) %>%
  unique()



saveRDS(epa_nei_envirofacts_complete, "_transportation/data/epa_nei_envirofacts_complete.RDS")
saveRDS(epa_nei_envirofacts_complete_meta, "_transportation/data/epa_nei_envirofacts_complete_meta.RDS")



# combine state and county to get relative proportions  -----
nei_county_proportions <- nei_state_emissions %>%
  select(state_name, nei_inventory_year,
    vehicle_weight_label,
    vehicle_group, vehicle_fuel_label,
    state_emissions_metric_tons_co2e = emissions_metric_tons_co2e
  ) %>%
  left_join(
    epa_nei_envirofacts %>%
      ungroup() %>%
      select(county_fips, state_name, county_name, geoid, nei_inventory_year,
        cprg_area,
        vehicle_weight_label, vehicle_fuel_label, vehicle_group,
        county_emissions_metric_tons_co2e = emissions_metric_tons_co2e
      ) %>%
      unique(),
    relationship = "many-to-many"
  ) %>%
  filter(vehicle_group == "On-Road") %>%
  group_by(state_name, geoid, county_name, nei_inventory_year) %>%
  summarize(
    county_emissions_metric_tons_co2e = sum(county_emissions_metric_tons_co2e),
    state_emissions_metric_tons_co2e = sum(state_emissions_metric_tons_co2e),
    .groups = "keep"
  ) %>%
  mutate(
    county_proportion_emissions = county_emissions_metric_tons_co2e / state_emissions_metric_tons_co2e %>%
      round(digits = 6)
  )

saveRDS(nei_county_proportions, "_transportation/data/epa_nei_envirofacts_county_proportions.RDS")
