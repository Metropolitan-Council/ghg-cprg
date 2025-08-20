# compile emissions from all sectors into a single data table
source("R/_load_pkgs.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")

cprg_county_pop <- readRDS("_meta/data/census_county_population.RDS") %>%
  filter(cprg_area == TRUE) %>%
  mutate(
    population_year = as.numeric(population_year)
  ) %>%
  select(-cprg_area)

# transportation -----
transportation_emissions <- readRDS("_transportation/data/onroad_emissions.RDS") %>%
  ungroup() %>%
  filter(emissions_year >= 2005) %>%
  mutate(
    sector = "Transportation",
    source = paste0(vehicle_fuel_label, " fueled vehicles"),
    category = category,
    geog_level = "county",
    sector_alt = sector,
    data_source = data_source,
    factor_source = moves_edition,
    value_emissions = emissions_metric_tons_co2e,
    unit_emissions = "Metric tons CO2e"
  ) %>%
  select(
    emissions_year,
    geog_level,
    county_name,
    sector,
    category,
    source,
    sector_alt,
    value_emissions,
    unit_emissions,
    data_source,
    factor_source
  )

aviation_emissions <- readRDS("_transportation/data/aviation_emissions.RDS") %>%
  mutate(
    sector = "Transportation",
    geog_level = "county",
    county_name = geog_name,
    category = "Off-road",
    source = "Aviation",
    sector_alt = sector,
    data_source = data_source,
    factor_source = data_source,
    value_emissions,
    unit_emissions = "Metric tons CO2e",
    emissions_year = inventory_year
  ) %>%
  select(names(transportation_emissions))


# waste -----
## wastewater ----
ww_emissions <- readRDS("_waste/data/final_wastewater_allyrs.RDS") %>%
  mutate(
    geog_level = "county",
    factor_source = data_source,
    value_emissions = mt_co2e,
    sector_alt = sector,
    unit_emissions = "Metric tons CO2e",
    emissions_year = as.numeric(inventory_year)
  ) %>%
  ungroup() %>%
  select(names(transportation_emissions))


## solid waste -----
solid_waste <- readRDS("_waste/data/final_solid_waste_allyrs.RDS") %>%
  left_join(cprg_county %>% select(county_name, geoid), join_by(geoid)) %>%
  ungroup() %>%
  mutate(
    geog_level = "county",
    emissions_year = as.numeric(inventory_year),
    sector_alt = sector,
    unit_emissions = "Metric tons CO2e"
  ) %>%
  ungroup() %>%
  select(names(transportation_emissions))




# energy -----
electric_natgas_nrel_proportioned <- readRDS("_energy/data/electric_natgas_nrel_proportioned_expanded.RDS")

## electricity ----

electric_emissions <- electric_natgas_nrel_proportioned %>%
  filter(source == "Electricity") %>%
  mutate(
    emissions_year = year,
    sector = str_to_title(sector),
    category = str_to_sentence(paste(sector, category)),
    sector_alt = "Electricity",
    geog_level = "county",
    source = "Building energy"
  ) %>%
  ungroup() %>%
  select(names(transportation_emissions))


## natural gas ----

natural_gas_emissions <- electric_natgas_nrel_proportioned %>%
  filter(source == "Natural gas") %>%
  mutate(
    emissions_year = year,
    sector = str_to_title(sector),
    category = str_to_sentence(paste(sector, category)),
    sector_alt = "Building fuel",
    geog_level = "county",
    source = "Natural gas"
  ) %>%
  ungroup() %>%
  select(names(transportation_emissions))


## propane and kerosene ----

propane_kerosene_emissions <- readRDS("_energy/data/fuel_use.RDS") %>%
  mutate(
    emissions_year = 2021,
    sector = "Residential",
    sector_alt = "Building fuel",
    geog_level = "county",
    county_name = NAME,
    category = "Residential building fuel",
    source = "Propane and kerosene",
    data_source = "EIA RECS (2020)",
    factor_source = "EPA GHG Emission Factors Hub (2021)",
    value_emissions = emissions_metric_tons_co2e,
    unit_emissions = "Metric tons CO2e"
  ) %>%
  ungroup() %>%
  select(names(transportation_emissions))

# agriculture ----

agriculture_emissions <- readRDS("_agriculture/data/_agricultural_emissions.rds") %>%
  left_join(cprg_county %>% select(county_name, geoid)) %>%
  mutate(
    emissions_year = inventory_year,
    sector = "Agriculture",
    geog_level = "county",
    category = category,
    sector_alt = sector,
    value_emissions = mt_co2e,
    unit_emissions = "Metric tons CO2e",
    source = stringr::str_to_sentence(source),
    data_source = data_source,
    factor_source = factor_source
  ) %>%
  ungroup() %>%
  select(names(transportation_emissions))


# industrial ----

industrial_emissions <- readRDS("_industrial/data/modeled_industrial_baseline_emissions.RDS") %>%
  filter(!is.na(value_emissions)) %>% #problem in south saint paul, extra NA lines below actual value
  ungroup() %>%
  mutate(
    value_emissions,
    unit_emissions = "Metric tons CO2e",
    emissions_year = as.numeric(inventory_year),
    geog_level = "county",
    sector_alt = sector,
    source = str_to_sentence(source),
    category = case_when(
      category == "Stationary combustion" & source == "Natural gas" ~ str_to_sentence(paste(sector, source)),
      category == "Stationary combustion" & source != "Natural gas" ~ str_to_sentence(paste(sector, "fuel combustion")),
      TRUE ~ category
    )
  ) %>%
  group_by(emissions_year, unit_emissions, county_name, geog_level, county_id, sector, sector_alt, category, source, data_source, factor_source) %>%
  summarize(value_emissions = sum(value_emissions)) %>%
  ungroup() %>%
  select(names(transportation_emissions))


# natural systems ----

natural_systems_sequestration <- readRDS("_nature/data/nlcd_county_landcover_sequestration_allyrs.RDS") %>%
  filter(inventory_year >= 2005) %>%
  mutate(
    emissions_year = inventory_year,
    sector = "Natural Systems",
    geog_level = "county",
    category = "Sequestration",
    sector_alt = sector,
    factor_source = "Various primary literature",
    value_emissions = sequestration_potential,
    unit_emissions = "Metric tons CO2e"
  ) %>%
  ungroup() %>%
  select(names(transportation_emissions))

freshwater_emissions <- readRDS("_nature/data/nhd_ctu_waterways_emissions_allyrs.RDS") %>%
  filter(inventory_year >= 2005) %>%
  mutate(
    emissions_year = inventory_year,
    sector = "Natural Systems",
    geog_level = "county",
    category = "Freshwater",
    sector_alt = sector,
    source = stringr::str_to_sentence(str_replace_all(source, "_", " ")),
    value_emissions = mt_co2e,
    unit_emissions = "Metric tons CO2e"
  ) %>%
  ungroup() %>%
  select(names(transportation_emissions))


# combine and write metadata----

emissions_all <- bind_rows(
  transportation_emissions,
  aviation_emissions,
  propane_kerosene_emissions,
  electric_emissions,
  natural_gas_emissions,
  industrial_emissions,
  ww_emissions,
  solid_waste,
  agriculture_emissions,
  natural_systems_sequestration,
  freshwater_emissions
) %>%
  left_join(
    cprg_county %>%
      sf::st_drop_geometry() %>%
      select(county_name, geoid),
    by = "county_name"
  ) %>%
  # mutate(
  #   category = factor(
  #     category,
  #     c(
  #       "Electricity",
  #       "Building Fuel",
  #       "On-road",
  #       "Off-road",
  #       "Wastewater",
  #       "Solid waste",
  #       "Livestock",
  #       "Cropland",
  #       "Commercial fuel combustion",
  #       "Commercial natural gas",
  #       "Industrial fuel combustion",
  #       "Industrial natural gas",
  #       "Industrial processes",
  #       "Refinery processes",
  #       "Sequestration",
  #       "Freshwater"
  #     ),
  #     ordered = TRUE
  #   )
  # ) %>%
  #  join county population and calculate per capita emissions
  left_join(
    cprg_county_pop %>%
      select(
        geoid,
        population_year,
        county_total_population = population,
        population_data_source
      ),
    by = join_by(geoid, emissions_year == population_year)
  ) %>%
  rowwise() %>%
  mutate(emissions_per_capita = round(value_emissions / county_total_population, digits = 2)) %>%
  select(emissions_year, geog_level, geoid, county_name, everything()) %>%
  ungroup()

emissions_all_meta <- tibble::tribble(
  ~"Column", ~"Class", ~"Description",
  "emissions_year", class(emissions_all$emissions_year), "Emissions estimation year",
  "geog_level", class(emissions_all$geog_level), "Geography level; city or county",
  "geoid", class(emissions_all$geoid), "FIPS code",
  "county_name", class(emissions_all$emissions_year), "Name of geographic area",
  "sector", class(emissions_all$sector), paste0(
    "Emissions sector. One of ",
    paste0(unique(emissions_all$sector), collapse = ", ")
  ),
  "sector_alt", class(emissions_all$sector), paste0(
    "Alternative sector grouping. One of ",
    paste0(unique(emissions_all$sector_alt), collapse = ", ")
  ),
  "category", class(emissions_all$category), "Category of emissions within given sector",
  "source", class(emissions_all$source), "Source of emissions. Most detailed sub-category in this table",
  "value_emissions", class(emissions_all$value_emissions), "Annual total metric tons CO~2~ and CO~2~ equivalent attributed to the given geography for given year",
  "data_source", class(emissions_all$data_source), "Activity data source",
  "factor_source", class(emissions_all$factor_source), "Emissions factor data source",
  "county_total_population", class(emissions_all$county_total_population), "Total geography population",
  "population_data_source", class(emissions_all$population_data_source), "Population data source",
  "emissions_per_capita", class(emissions_all$emissions_per_capita), "Metric tons CO~2~e per person living in given county for given sector and category"
)

saveRDS(emissions_all, "_meta/data/cprg_county_emissions.RDS")
saveRDS(emissions_all_meta, "_meta/data/cprg_county_emissions_meta.RDS")
write.csv(emissions_all, "_meta/data/cprg_county_emissions.CSV", row.names = FALSE)

waldo::compare(emissions_all, readRDS("_meta/data/cprg_county_emissions.RDS"))
# waldo::compare(electric_natgas_nrel_proportioned, readRDS("_energy/data/electric_natgas_nrel_proportioned_expanded.RDS"))

### carbon stock

natural_systems_stock <- readRDS("_nature/data/nlcd_county_landcover_sequestration_allyrs.RDS") %>%
  filter(inventory_year >= 2005) %>%
  mutate(
    emissions_year = inventory_year,
    sector = "Natural Systems",
    sector_alt = sector,
    geog_level = "county",
    category = "Sequestration",
    factor_source = "Various primary literature",
    value_emissions = stock_potential,
    unit_emissions = "Metric tons CO2e"
  ) %>%
  ungroup() %>%
  select(names(transportation_emissions))

saveRDS(natural_systems_stock, "_meta/data/cprg_county_carbon_stock.RDS")
# saveRDS(emissions_all_meta, "_meta/data/cprg_county_carbon_stock_meta.RDS")

waldo::compare(natural_systems_stock, readRDS("_meta/data/cprg_county_carbon_stock.RDS"))

# save emissions to shared drive location
# source("R/fetch_path.R")

# if (fs::dir_exists(fetch_path())) {
#   write.csv(emissions_all, paste0(fetch_path(), "/cprg_county_emissions.CSV"), row.names = FALSE)
# }
