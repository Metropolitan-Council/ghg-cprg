# compile emissions from all sectors into a single data table
source("R/_load_pkgs.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_county_pop <- readRDS("_meta/data/cprg_population.RDS")

# transportation -----
transportation_emissions <- readRDS("_transportation/data/county_vmt_emissions.RDS") %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    sector = "Transportation",
    geog_level = "county",
    geog_name = zone,
    category = paste0(stringr::str_to_sentence(vehicle_type), " vehicles"),
    source = paste0(vehicle_weight_label, " vehicles"),
    data_source = "StreetLight Data",
    factor_source = paste0("EPA MOVES (", moves_year, ")")
  ) %>%
  select(
    year,
    geog_level,
    geog_name,
    sector,
    category,
    source,
    emissions_metric_tons_co2e,
    data_source,
    factor_source
  )

# waste -----
## wastewater ----
ww_emissions <- readRDS("_waste/data/epa_county_wastewater.RDS") %>%
  mutate(
    sector = "Waste",
    geog_level = "county",
    geog_name = NAME,
    category = "Wastewater",
    source = "Wastewater",
    data_source = "EPA State GHG Inventory and Projection Tool",
    factor_source = data_source,
    emissions_metric_tons_co2e = epa_co2e,
    year = 2021
  ) %>%
  select(names(transportation_emissions))


## solid waste -----
solid_waste <- readRDS("_waste/data/county_sw_emissions.RDS") %>%
  ungroup() %>%
  mutate(
    sector = "Waste",
    geog_level = "county",
    geog_name = county,
    category = "Solid waste",
    source = source,
    data_source,
    factor_source = "EPA GHG Emission Factors Hub (2021)"
  ) %>%
  select(names(transportation_emissions))



# energy -----

electric_natgas_nrel_proportioned <- readRDS("_energy/data/electric_natgas_nrel_proportioned.RDS")

## electricity ----

electric_emissions <- electric_natgas_nrel_proportioned %>%
  filter(source == "Electricity") %>%
  mutate(
    sector = "Energy",
    geog_level = "county",
    geog_name = county,
    category = paste0(category, " energy"),
    source = source,
    data_source = "Individual electric utilities, NREL SLOPE",
    factor_source = "eGRID MROW"
  ) %>%
  select(names(transportation_emissions))


## natural gas ----

natural_gas_emissions <- electric_natgas_nrel_proportioned %>%
  filter(source == "Natural gas") %>%
  mutate(
    sector = "Energy",
    geog_level = "county",
    geog_name = county,
    category = paste0(category, " energy"),
    source = source,
    data_source = "Individual natural gas utilities, NREL SLOPE",
    factor_source = "EPA GHG Emission Factors Hub (2021)"
  ) %>%
  select(names(transportation_emissions))

## propane and kerosene ----

propane_kerosene_emissions <- readRDS("_energy/data/fuel_use.RDS") %>%
  mutate(
    sector = "Energy",
    geog_level = "county",
    geog_name = NAME,
    category = "Liquid stationary fuels",
    source = stringr::str_to_sentence(fuel_type),
    data_source = "EIA RECS (2020)",
    factor_source = "EPA GHG Emission Factors Hub (2021)"
  ) %>%
  select(names(transportation_emissions))

# combine and write metadata----

emissions_all <- bind_rows(
  transportation_emissions,
  propane_kerosene_emissions,
  electric_emissions,
  natural_gas_emissions,
  ww_emissions,
  solid_waste
) %>%
  left_join(
    cprg_county %>%
      sf::st_drop_geometry() %>%
      select(NAME, geog_id = COUNTYFP),
    by = c("geog_name" = "NAME")
  ) %>%
  mutate(
    source = factor(source,
      c(
        # transportation levels
        "Light-duty vehicles",
        "Medium-duty vehicles",
        "Heavy-duty vehicles",
        # waste levels
        "Landfill",
        "Waste to energy",
        "Recycling",
        "Organics",
        "Wastewater",
        # energy levels
        "Electricity",
        "Natural gas",
        "Propane",
        "Kerosene"
      ),
      ordered = TRUE
    ),
    category = factor(
      category,
      c(
        "Residential energy",
        "Commercial energy",
        "Industrial energy",
        "Liquid stationary fuels",
        "Passenger vehicles",
        "Commercial vehicles",
        "Wastewater",
        "Solid waste"
      ),
      ordered = TRUE
    )
  ) %>%
  # join county population and calculate per capita emissions
  left_join(
    cprg_county_pop %>%
      select(
        geog_id = COUNTYFP,
        county_total_population = population,
        population_data_source
      ),
    by = "geog_id"
  ) %>%
  rowwise() %>%
  mutate(emissions_per_capita = round(emissions_metric_tons_co2e / county_total_population, digits = 2)) %>%
  select(year, geog_level, geog_id, geog_name, everything())



mean(emissions_all$emissions_per_capita)

sum(emissions_all$emissions_metric_tons_co2e) / sum(cprg_county_pop$population)

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

county_emissions <- emissions_all

# save emissions to shared drive location
source("R/fetch_path.R")

if (fs::dir_exists(fetch_path())) {
  write.csv(emissions_all, paste0(fetch_path(), "/cprg_county_emissions.CSV"), row.names = FALSE)
}
