# compile emissions from all sectors into a single data table
source("R/_load_pkgs.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")

# transportation -----
transportation_emissions <- readRDS("_transportation/data/county_vmt_emissions.RDS") %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    sector = "Transportation",
    geog_level = "county",
    geog_name = zone,
    category = stringr::str_to_sentence(vehicle_type),
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
ww_emissions <- readRDS("_energy/data/wastewater.RDS") %>%
  mutate(
    sector = "Waste",
    geog_level = "county",
    geog_name = COUNTY_NAME,
    category = "Wastewater",
    source = "Wastewater",
    data_source = source,
    factor_source = source,
    emissions_metric_tons_co2e = co2e
  ) %>%
  select(names(transportation_emissions))


## solid waste -----
# energy -----
## electricity ----
electric_raw <- readRDS(file.path(here::here(), "_energy/data/minnesota_county_ElecEmissions.RDS")) %>%
  bind_rows(readRDS(file.path(here::here(), "_energy/data/wisconsin_county_ElecEmissions.RDS")) %>%
    rename(county = county_name))


electric_emissions <- electric_raw %>%
  mutate(
    sector = "Energy",
    geog_level = "county",
    geog_name = county,
    category = "Electricity",
    source = "Electricity",
    data_source = "Individual electric utilities",
    factor_source = "eGRID MROW"
  ) %>%
  select(names(transportation_emissions))


rm(electric_raw)

## natural gas ----
natgas_raw <- readRDS(file.path(here::here(), "_energy/data/minnesota_county_GasEmissions.RDS")) %>%
  bind_rows(readRDS(file.path(here::here(), "_energy/data/wisconsin_county_GasEmissions.RDS")) %>%
    rename(county = county_name))


natural_gas_emissions <- natgas_raw %>%
  mutate(
    sector = "Energy",
    geog_level = "county",
    geog_name = county,
    category = "Natural Gas",
    source = "Natural Gas",
    data_source = "Individual natural gas utilities",
    factor_source = "EPA GHG Emission Factors Hub (2021)"
  ) %>%
  select(names(transportation_emissions))

## propane and kerosene ----

propane_kerosene_emissions <- readRDS("_energy/data/fuel_use.RDS") %>%
  mutate(
    sector = "Energy",
    geog_level = "county",
    geog_name = NAME,
    category = stringr::str_to_sentence(fuel_type),
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
  ww_emissions
) %>%
  left_join(
    cprg_county %>%
      sf::st_drop_geometry() %>%
      select(NAME, geog_id = COUNTYFP),
    by = c("geog_name" = "NAME")
  ) %>%
  mutate(source = factor(source,
    c(
      # transportation levels
      "Light-duty vehicles",
      "Medium-duty vehicles",
      "Heavy-duty vehicles",
      # waste levels
      "Wastewater",
      # energy levels
      "Electricity",
      "Natural Gas",
      "Propane",
      "Kerosene"
    ),
    ordered = TRUE
  )) %>%
  select(year, geog_level, geog_id, geog_name, everything())

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
  "factor_source", class(emissions_all$factor_source), "Emissions factor data source"
)

saveRDS(emissions_all, "_meta/data/cprg_county_emissions.RDS")
saveRDS(emissions_all_meta, "_meta/data/cprg_county_emissions_meta.RDS")
