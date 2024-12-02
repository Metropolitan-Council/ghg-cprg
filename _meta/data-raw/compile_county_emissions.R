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
  rowwise() %>%
  mutate(
    year = emissions_year,
    sector = "Transportation",
    geog_level = "county",
    geog_name = county_name,
    source = paste0(vehicle_fuel_label, " fueled vehicles"),
    category = category,
    data_source = data_source,
    factor_source = moves_edition
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
    source = str_to_sentence(source),
    data_source,
    factor_source = "EPA GHG Emission Factors Hub (2021)"
  ) %>%
  select(names(transportation_emissions))



# energy -----

electric_natgas_nrel_proportioned <- readRDS("_energy/data/electric_natgas_nrel_proportioned.RDS")

## electricity ----

electric_emissions <- electric_natgas_nrel_proportioned %>%
  filter(source == "Electricity") %>%
  # avoid duplication and NAs until category is infilled later
  filter((year == 2005 & category == "Total") |
    (year == 2021 & category != "Total")) %>%
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
  # avoid duplication and NAs until category is infilled later
  filter((year == 2005 & category == "Total") |
    (year == 2021 & category != "Total")) %>%
  mutate(
    sector = "Energy",
    geog_level = "county",
    geog_name = county,
    category = paste0(category, " energy"),
    source = source,
    data_source = "Individual natural gas utilities, NREL SLOPE (2021)",
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

## agriculture ----

agriculture_emissions <- readRDS("_agriculture/data/_agricultural_emissions.rds") %>%
  left_join(cprg_county %>% select(county_name, geoid)) %>%
  mutate(
    year = inventory_year,
    sector = "Agriculture",
    geog_level = "county",
    geog_name = county_name,
    category = category,
    emissions_metric_tons_co2e = mt_co2e,
    source = stringr::str_to_sentence(source),
    data_source = data_source,
    factor_source = factor_source
  ) %>%
  select(names(transportation_emissions))

## natural systems ----

natural_systems_sequestration <- readRDS("_nature/data/county_landcover_sequestration_2021.RDS") %>%
  mutate(
    sector = "Nature",
    geog_level = "county",
    geog_name = county,
    category = "Sequestration",
    source = stringr::str_to_sentence(str_replace_all(land_cover_type, "_", " ")),
    data_source = "ESA WorldCover & NLCD 2021",
    factor_source = "Various primary literature",
    year = 2021,
    emissions_metric_tons_co2e = sequestration_potential,
  ) %>%
  ungroup() %>%
  select(names(transportation_emissions))

natural_systems_stock <- readRDS("_nature/data/county_landcover_sequestration_2021.RDS") %>%
  mutate(
    sector = "Nature",
    geog_level = "county",
    geog_name = county,
    category = "Stock",
    source = stringr::str_to_sentence(str_replace_all(land_cover_type, "_", " ")),
    data_source = "ESA WorldCover & NLCD 2021",
    factor_source = "Various primary literature",
    year = 2021,
    emissions_metric_tons_co2e = stock_potential,
  ) %>%
  ungroup() %>%
  select(names(transportation_emissions))

# combine and write metadata----

emissions_all <- bind_rows(
  transportation_emissions,
  propane_kerosene_emissions,
  electric_emissions,
  natural_gas_emissions,
  ww_emissions,
  solid_waste,
  agriculture_emissions,
  natural_systems_sequestration,
  natural_systems_stock
) %>%
  left_join(
    cprg_county %>%
      sf::st_drop_geometry() %>%
      select(county_name, geoid),
    by = c("geog_name" = "county_name")
  ) %>%
  mutate(
    source = factor(source,
      c(
        # transportation levels
        "Gasoline fueled vehicles",
        "Diesel fueled vehicles",
        "Other fueled vehicles",
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
        "Kerosene",
        # agriculture levels
        "Enteric_fermentation",
        "Manure_management",
        "Direct_manure_soil_emissions",
        "Indirect_manure_runoff_emissions",
        "Crop_residue_emissions",
        "Crop_fertilizer_emissions",
        "Runoff_fertilizer_emissions",
        # nature levels
        "Urban grassland",
        "Urban tree",
        "Grassland",
        "Tree",
        "Wetland"
      ),
      ordered = TRUE
    ),
    category = factor(
      category,
      c(
        "Residential energy",
        "Commercial energy",
        "Industrial energy",
        "Total energy",
        "Liquid stationary fuels",
        "Passenger vehicles",
        "Buses",
        "Medium-duty vehicles",
        "Trucks",
        "Wastewater",
        "Solid waste",
        "Livestock",
        "Cropland",
        "Sequestration",
        "Stock"
      ),
      ordered = TRUE
    )
  ) %>%
  # join county population and calculate per capita emissions
  left_join(
    cprg_county_pop %>%
      select(
        geoid,
        population_year,
        county_total_population = population,
        population_data_source
      ),
    by = join_by(geoid, year == population_year)
  ) %>%
  rowwise() %>%
  mutate(emissions_per_capita = round(emissions_metric_tons_co2e / county_total_population, digits = 2)) %>%
  select(year, geog_level, geoid, geog_name, everything())

# splitting off carbon stock here as it is a capacity, not a rate
carbon_stock <- emissions_all %>% filter(category == "Stock")
emissions_all <- emissions_all %>% filter(category != "Stock")

mean(emissions_all$emissions_per_capita[!emissions_all$category == "Stock"], na.rm = T)
sum(emissions_all$emissions_metric_tons_co2e[!emissions_all$category == "Stock"], na.rm = T) / sum(cprg_county_pop$population)

emissions_all_meta <- tibble::tribble(
  ~"Column", ~"Class", ~"Description",
  "year", class(emissions_all$year), "Emissions estimation year",
  "geog_level", class(emissions_all$geog_level), "Geography level; city or county",
  "geoid", class(emissions_all$geoid), "FIPS code",
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

saveRDS(carbon_stock, "_meta/data/cprg_county_carbon_stock.RDS")
saveRDS(emissions_all_meta, "_meta/data/cprg_county_carbon_stock_meta.RDS")

# save emissions to shared drive location
# source("R/fetch_path.R")

# if (fs::dir_exists(fetch_path())) {
#   write.csv(emissions_all, paste0(fetch_path(), "/cprg_county_emissions.CSV"), row.names = FALSE)
# }
