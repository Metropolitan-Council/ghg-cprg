# compile emissions from all sectors into a single data table, reducing to CTU
source("R/_load_pkgs.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS") %>% st_drop_geometry()
cprg_county_pop <- readRDS("_meta/data/census_county_population.RDS") %>%
  filter(cprg_area == TRUE) %>%
  mutate(
    population_year = as.numeric(population_year)
  ) %>%
  select(-cprg_area)

## load in current county emissions
county_emissions <- read_rds("_meta/data/cprg_county_emissions.RDS")


ctu_population <- readRDS("_meta/data/ctu_population.RDS") %>%
  left_join(cprg_county %>% select(geoid, county_name))

# assign ctu to county where it has highest population in 2021
ctu_county <- ctu_population %>%
  filter(inventory_year == 2021) %>%
  group_by(ctu_name, ctu_class) %>%
  mutate(max_population = max(ctu_population)) %>%
  filter(ctu_population == max_population) %>%
  distinct(ctu_name, county_name)

mndot_vmt_ctu <- readRDS("_transportation/data/mndot_vmt_ctu.RDS")


ctu_vmt_percent <- left_join(mndot_vmt_ctu, cprg_county %>%
  select(geoid, county_name),
by = "geoid"
) %>%
  group_by(county_name, vmt_year) %>%
  mutate(county_annual_vmt = sum(annual_vmt)) %>%
  ungroup() %>%
  mutate(
    ctu_vmt_percent = annual_vmt / county_annual_vmt,
    vmt_year = as.numeric(vmt_year)
  )


# transportation -----
transportation_emissions <- readRDS("_transportation/data/onroad_emissions.RDS") %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    year = emissions_year,
    sector = "Transportation",
    source = paste0(vehicle_fuel_label, " fueled vehicles"),
    category = category,
    data_source = data_source,
    factor_source = moves_edition
  ) %>%
  group_by(emissions_year, county_name, sector, category) %>%
  summarize(emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e)) %>%
  left_join(ctu_vmt_percent,
    by = c("county_name",
      "emissions_year" = "vmt_year"
    )
  ) %>%
  mutate(
    emissions_metric_tons_co2e = emissions_metric_tons_co2e * ctu_vmt_percent,
    geog_level = "ctu"
  ) %>%
  select(
    emissions_year,
    geog_level,
    ctu_name,
    sector,
    category,
    emissions_metric_tons_co2e
  )


# waste -----
## wastewater ----
ww_emissions <- readRDS("_waste/data/final_wastewater_ctu_allyrs.RDS") %>%
  mutate(
    factor_source = data_source,
    value_emissions = mt_co2e
  ) %>%
  select(names(transportation_emissions))


## solid waste -----
solid_waste <- readRDS("_waste/data/final_solid_waste_ctu_allyrs.RDS") %>%
  left_join(ctu_population %>% distinct(ctu_name, ctuid)) %>%
  left_join(cprg_county %>% select(county_name, geoid)) %>%
  ungroup() %>%
  mutate(
    geog_level = "ctu",
    emissions_year = as.numeric(inventory_year),
    emissions_metric_tons_co2e = value_emissions
  ) %>%
  filter(!is.na(emissions_metric_tons_co2e)) %>%
  select(names(transportation_emissions))



# energy -----
electric_natgas_nrel_proportioned <- readRDS("_energy/data-raw/nrel_slope/nrel_emissions_inv_cityQA_2021.RDS")

## electricity ----

electric_emissions <- electric_natgas_nrel_proportioned %>%
  filter(source == "Electricity") %>%
  mutate(
    sector = str_to_title(sector_raw),
    geog_level = "ctu",
    category = "Electricity",
    emissions_metric_tons_co2e = co2e_city,
    data_source = "Individual electric utilities, NREL SLOPE",
    factor_source = "eGRID MROW",
    emissions_year = as.numeric(year)
  ) %>%
  select(names(transportation_emissions))


## natural gas ----

natural_gas_emissions <- electric_natgas_nrel_proportioned %>%
  filter(source == "Natural gas") %>%
  mutate(
    sector = str_to_title(sector_raw),
    geog_level = "ctu",
    category = "Natural Gas",
    emissions_metric_tons_co2e = co2e_city,
    data_source = "Individual natural gas utilities, NREL SLOPE (2021)",
    factor_source = "EPA GHG Emission Factors Hub (2021)",
    emissions_year = as.numeric(year)
  ) %>%
  select(names(transportation_emissions))

## industrial ----

industrial_emissions <- readRDS("_industrial/data/city_industrial_emissions.RDS") %>%
  ungroup() %>%
  mutate(
    geog_level = "ctu",
    ctu_name = city_name,
    emissions_metric_tons_co2e = value_emissions,
    emissions_year = as.numeric(inventory_year),
    source = str_to_sentence(source),
    category = case_when(
      category == "Stationary combustion" & source == "Natural gas" ~ str_to_sentence(paste(sector, source)),
      category == "Stationary combustion" & source != "Natural gas" ~ str_to_sentence(paste(sector, "fuel combustion")),
      TRUE ~ category
    )
  ) %>%
  # left_join(ctu_population %>% select(ctu_name, county_name, inventory_year),
  #           by = c("ctu_name" = "ctu_name",
  #                  "emissions_year" = "inventory_year")) %>%
  select(names(transportation_emissions))

## agriculture ----


agriculture_emissions <- 
  readRDS(file.path(here::here(), "_agriculture/data/_ctu_agricultural_emissions.RDS")) %>%
  mutate(
    emissions_year = inventory_year,
    sector = "Agriculture",
    geog_level = "ctu"
  ) %>%
  ungroup() %>%
  select(names(transportation_emissions))


## natural systems ----

natural_systems_sequestration <- readRDS("_nature/data/nlcd_ctu_landcover_sequestration_2001_2021_v2.rds") %>%
  ungroup() %>%
  mutate(
    geog_level = "ctu",
    emissions_metric_tons_co2e = sequestration_potential,
    emissions_year = as.numeric(year),
    sector = "Nature",
    category = case_when(
      grepl("Urban", land_cover_type) ~ "Urban greenery",
      !grepl("Urban", land_cover_type) ~ "Natural systems"
    ),
    source = land_cover_type
  ) %>%
  select(names(transportation_emissions))

# combine and write metadata----

emissions_all <- bind_rows(
  transportation_emissions,
  electric_emissions,
  natural_gas_emissions,
  ww_emissions,
  solid_waste,
  agriculture_emissions,
  industrial_emissions,
  natural_systems_sequestration
  # natural_systems_stock
) %>%
  filter(emissions_year >= 2005 & emissions_year <= 2021) %>%
  mutate(
    category = factor(
      category,
      c(
        "Electricity",
        "Natural Gas",
        "Passenger vehicles",
        "Buses",
        "Trucks",
        "Wastewater",
        "Solid waste",
        "Livestock",
        "Cropland",
        "Commercial fuel combustion",
        "Commercial natural gas",
        "Industrial fuel combustion",
        "Industrial natural gas",
        "Industrial processes",
        "Refinery processes",
        "Natural systems",
        "Urban greenery"
        # "Stock"
      ),
      ordered = TRUE
    )
  ) %>%
  ## keep 7 counties only for CTU estimates
  filter(!county_name %in% c("St. Croix", "Pierce", "Chisago", "Sherburne")) %>%
  group_by(county_name, emissions_year, geog_level, ctu_name, sector, category) %>%
  summarize(emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e), .groups = "drop")


# join county population and calculate per capita emissions
# left_join(
#   ctu_population %>%
#     select(
#       ctu_name,
#       county_name,
#       population_year = inventory_year,
#       city_total_population = ctu_population
#     ),
#   by = join_by(ctu_name, county_name, emissions_year == population_year)
# ) %>%
# rowwise() %>%
# mutate(emissions_per_capita = round(emissions_metric_tons_co2e / city_total_population, digits = 2)) %>%
# select(emissions_year, geog_level, ctu_name, everything())


emissions_all %>%
  filter(emissions_year == 2021, !is.na(emissions_metric_tons_co2e)) %>%
  pull(emissions_metric_tons_co2e) %>%
  sum() /
  sum(cprg_county_pop[cprg_county_pop$population_year == 2021, ]$population)

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

saveRDS(emissions_all, "_meta/data/ctu_emissions.RDS")
saveRDS(emissions_all_meta, "_meta/data/cprg_county_emissions_meta.RDS")
write.csv(emissions_all, "_meta/data/cprg_county_emissions.CSV", row.names = FALSE)


saveRDS(carbon_stock, "_meta/data/cprg_county_carbon_stock.RDS")
saveRDS(emissions_all_meta, "_meta/data/cprg_county_carbon_stock_meta.RDS")

# save emissions to shared drive location
# source("R/fetch_path.R")

# if (fs::dir_exists(fetch_path())) {
#   write.csv(emissions_all, paste0(fetch_path(), "/cprg_county_emissions.CSV"), row.names = FALSE)
# }
