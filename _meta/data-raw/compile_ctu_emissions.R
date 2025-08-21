# compile emissions from all sectors into a single data table, reducing to CTU
source("R/_load_pkgs.R")
source("_meta/data-raw/ctu_coctu_index.R")

cprg_county_pop <- readRDS("_meta/data/census_county_population.RDS") %>%
  filter(cprg_area == TRUE) %>%
  mutate(
    population_year = as.numeric(population_year)
  ) %>%
  select(-cprg_area)


## load in current county emissions
county_emissions <- read_rds("_meta/data/cprg_county_emissions.RDS")

ctu_population <- readRDS("_meta/data/ctu_population.RDS") %>%
  left_join(cprg_county %>% select(geoid, county_name),
            by = join_by(geoid)
  )


# transportation -----

# fetch gap-filled/modeled ctu level data from 2010 to 2022
mndot_vmt_ctu_gap_filled <- readRDS("_transportation/data/mndot_vmt_ctu_gap_filled.RDS") %>% 
  filter(inventory_year < 2023)

# fetch mndot vmt by county, 2002-2022
mndot_vmt_county <- readRDS("_transportation/data-raw/mndot/mndot_vmt_county.RDS") %>%
  mutate(
    vmt_year = as.numeric(year),
    county_daily_vmt = daily_vmt,
    county_name = county) %>% 
  left_join(ctu_coctu_index %>% 
              select(geoid, county_name) %>% 
              unique(), 
            by = c("county_name")) %>% 
  # remove Wright, Sherburne, WI counties
  filter(!is.na(geoid))

# find the percentage of county VMT each CTU makes up
# 2010-2022
ctu_vmt_percent <- (mndot_vmt_ctu_gap_filled) %>%
  left_join(mndot_vmt_county, by = c("geoid", "inventory_year" = "vmt_year")) %>% 
  ungroup() %>%
  rowwise() %>% 
  mutate(
    ctu_vmt_percent = final_city_vmt / county_daily_vmt,
    vmt_year = as.numeric(inventory_year)
  ) %>% 
  select(inventory_year, geoid, county_name, gnis, 
         coctu_id_gnis, final_vmt_source, ctu_vmt_percent) %>% 
  unique()

# get the ctu percentage of county VMT for year 2010
ctu_vmt_pct_2010 <- ctu_vmt_percent %>% 
  filter(inventory_year == "2010") %>% 
  mutate(ctu_vmt_percent10 = ctu_vmt_percent) %>% 
  select(coctu_id_gnis, geoid, gnis,county_name, ctu_vmt_percent10) %>% 
  unique()

ctu_county_year_pct_index <- mndot_vmt_county %>% 
  select(vmt_year, geoid, county_name = county, daily_vmt) %>% 
  unique() %>% 
  # get mapping of CTUs to counties
  left_join(ctu_coctu_index %>% 
              select(-imagine_designation, -ctu_name_full, -ctu_name_full_county) %>% 
              unique(),
            relationship = "many-to-many",
            by = join_by(geoid, county_name)) %>% 
  # get CTU VMT percent from 2010-2022
  left_join(ctu_vmt_percent,
            by = join_by(county_name, geoid, gnis, coctu_id_gnis, vmt_year == inventory_year)) %>% 
  unique() %>% 
  # join with 2010 CTU VMT percent
  left_join(ctu_vmt_pct_2010, 
            by = join_by(geoid, county_name, gnis, coctu_id_gnis)) %>% 
  # for years 2002-2010, use 2010 CTU VMT percentage
  # for years 2010-2022, use actual CTU proportion of county VMT
  mutate(ctu_vmt_percent = ifelse(is.na(ctu_vmt_percent), ctu_vmt_percent10, ctu_vmt_percent),
         pct_data_source = ifelse(is.na(final_vmt_source), "2010 CTU share of county VMT", final_vmt_source)) %>% 
  select(-ctu_vmt_percent10)


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
  group_by(emissions_year, geoid, county_name, sector, category, source) %>%
  # summarize up to county level emissions
  summarize(value_emissions = sum(emissions_metric_tons_co2e), .groups = "keep") %>%
  # join with ctu-county VMT percentaegs
  left_join(ctu_county_year_pct_index,
            by = c("county_name",
                   "emissions_year" = "vmt_year",
                   "geoid"
            ),
            relationship = "many-to-many"
  ) %>% 
  ungroup() %>%
  mutate(
    # CTU emissions total is county emissions multiplied by 
    # ctu proportion of county VMT
    value_emissions = value_emissions * ctu_vmt_percent,
    geog_level = "ctu"
  ) %>% 
  # get ctuid column
  left_join(ctu_population %>% 
              select(coctu_id_gnis, ctuid, ctu_class) %>% 
              unique(), by = c("coctu_id_gnis")) %>% 
  # group at the CTU level (not coctu) and summarize
  group_by(emissions_year, geog_level, sector, category, source, ctu_name, ctu_class, ctuid, gnis) %>%
  summarize(value_emissions = sum(value_emissions), .groups = "keep") %>%
  ungroup() %>%
  mutate(sector_alt = sector) %>% # electricity and nat gas need this
  select(
    emissions_year,
    geog_level,
    ctu_name,
    ctu_class,
    sector,
    category,
    sector_alt,
    source,
    value_emissions
  )


# waste -----
## wastewater ----
ww_emissions <- readRDS("_waste/data/final_wastewater_ctu_allyrs.RDS") %>%
  ungroup() %>%
  mutate(
    factor_source = data_source,
    value_emissions = mt_co2e,
    sector_alt = sector,
    geog_level = "ctu"
  ) %>%
  rename(emissions_year = inventory_year) %>%
  select(names(transportation_emissions))


## solid waste -----
solid_waste <- readRDS("_waste/data/final_solid_waste_ctu_allyrs.RDS") %>%
  left_join(ctu_population %>% distinct(ctu_name, ctu_class, ctuid), by = join_by(ctuid)) %>%
  left_join(cprg_county %>% select(county_name, geoid), by = join_by(geoid)) %>%
  ungroup() %>%
  mutate(
    geog_level = "ctu",
    emissions_year = as.numeric(inventory_year),
    emissions_metric_tons_co2e = value_emissions,
    sector_alt = sector
  ) %>%
  filter(!is.na(emissions_metric_tons_co2e)) %>%
  select(names(transportation_emissions))


## electricity ----

electric_emissions <- readRDS("_energy/data/_ctu_electricity_emissions.RDS") %>%
  mutate(
    geog_level = "ctu",
    source = "Building energy",
    category = str_to_sentence(paste(sector, category)),
    sector_alt = "Electricity",
    emissions_year = inventory_year
  ) %>%
  select(names(transportation_emissions))


## natural gas ----

natural_gas_emissions <- readRDS("_energy/data/_ctu_natgas_emissions.RDS") %>%
  mutate(
    geog_level = "ctu",
    source = "Natural gas",
    category = str_to_sentence(paste(sector, "building fuel")),
    sector_alt = "Building fuel",
    emissions_year = inventory_year
  ) %>%
  select(names(transportation_emissions))

## industrial ----

industrial_emissions <- readRDS("_industrial/data/modeled_industrial_baseline_emissions.RDS") %>%
  ungroup() %>%
  mutate(
    geog_level = "ctu",
    ctu_name = city_name,
    ctu_class = "CITY",
    emissions_year = as.numeric(inventory_year),
    sector_alt = sector,
    source = str_to_sentence(source)
  ) %>%
  # left_join(ctu_population %>% select(ctu_name, county_name, inventory_year),
  #           by = c("ctu_name" = "ctu_name",
  #                  "emissions_year" = "inventory_year")) %>%
  select(names(transportation_emissions))

## agriculture ----


agriculture_emissions <-
  readRDS(file.path(here::here(), "_agriculture/data/_ctu_agricultural_emissions.RDS")) %>%
  group_by(ctu_id, ctu_name, ctu_class, inventory_year, sector, category, source, data_source, factor_source) %>%
  summarize(value_emissions = sum(mt_co2e), .groups = "keep") %>%
  mutate(
    emissions_year = inventory_year,
    sector = "Agriculture",
    geog_level = "ctu",
    sector_alt = sector,
    unit_emissions = "Metric tons CO2 equivalency"
  ) %>%
  ungroup() %>%
  select(names(transportation_emissions))


## natural systems ----

natural_systems_sequestration <- readRDS("_nature/data/nlcd_ctu_landcover_sequestration_allyrs.rds") %>%
  filter(inventory_year >= 2005) %>%
  group_by(ctu_id, ctu_name, ctu_class, inventory_year, sector, category, source, data_source) %>%
  summarize(value_emissions = sum(sequestration_potential), .groups = "keep") %>%
  ungroup() %>%
  mutate(
    geog_level = "ctu",
    emissions_year = inventory_year,
    factor_source = "Various primary literature",
    sector_alt = sector,
    unit_emissions = "Metric tons CO2 equivalency"
  ) %>%
  select(names(transportation_emissions))

freshwater_emissions <- readRDS("_nature/data/nhd_ctu_waterways_emissions_allyrs.RDS") %>%
  filter(inventory_year >= 2005) %>%
  group_by(ctu_id, ctu_name, ctu_class, inventory_year, sector, category, source, data_source) %>%
  summarize(value_emissions = sum(value_emissions), .groups = "keep") %>%
  mutate(
    emissions_year = inventory_year,
    sector = "Natural Systems",
    geog_level = "ctu",
    category = "Freshwater",
    sector_alt = sector,
    source = stringr::str_to_sentence(str_replace_all(source, "_", " ")),
    unit_emissions = "Metric tons CO2e"
  ) %>%
  ungroup() %>%
  select(names(transportation_emissions))

# combine and write metadata----

emissions_all <- bind_rows(
  transportation_emissions,
  electric_emissions,
  natural_gas_emissions,
  industrial_emissions,
  ww_emissions,
  solid_waste,
  agriculture_emissions,
  natural_systems_sequestration,
  freshwater_emissions
) %>%
  filter(emissions_year >= 2005 & emissions_year <= 2023) %>%
  # mutate(
  #   category = factor(
  #     category,
  #     c(
  #       "Building energy",
  #       "Passenger vehicles",
  #       "Buses",
  #       "Trucks",
  #       "Wastewater",
  #       "Solid waste",
  #       "Livestock",
  #       "Cropland",
  #       "Stationary combustion",
  #       "Industrial processes",
  #       "Refinery processes",
  #       "Sequestration",
  #       "Freshwater"
  #     ),
  #     ordered = TRUE
  #   )
  # ) %>%
  ## keep 7 counties only for CTU estimates and add population
  right_join(
    ctu_population %>%
      filter(inventory_year >= 2005) %>%
      group_by(ctu_name, ctu_class, ctuid, gnis, inventory_year) %>%
      summarize(ctu_population = sum(ctu_population), .groups = "keep") %>%
      ungroup() %>%
      rename(
        emissions_year = inventory_year,
        ctu_id_fips = ctuid,
        ctu_id_gnis = gnis
      ),
    by = join_by(emissions_year, ctu_name, ctu_class)
  ) %>%
  rename(geog_name = ctu_name) %>%
  mutate(emissions_per_capita = value_emissions / ctu_population)


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
  filter(emissions_year == 2021, !is.na(value_emissions)) %>%
  pull(value_emissions) %>%
  sum() /
  sum(cprg_county_pop[cprg_county_pop$population_year == 2021, ]$population)


emissions_all_meta <- 
  readRDS("_meta/data/ctu_population_meta.RDS") %>% 
  mutate(
    Column = case_when(Column == "gnis" ~ "ctu_id_gnis",
                       Column == "ctuid" ~ "ctu_id_fips",
                       TRUE ~ Column)) %>% 
    filter(Column %in% names(emissions_all)) %>% 
  bind_rows(
  tibble::tribble(
  ~"Column", ~"Class", ~"Description",
  "emissions_year", class(emissions_all$emissions_year), "Emissions estimation year",
  "geog_name", class(emissions_all$geog_name), "Name of geographic area",
  "geog_level", class(emissions_all$geog_level), "Geography level; ctu or county",
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
  # "data_source", class(emissions_all$data_source), "Activity data source",
  # "factor_source", class(emissions_all$factor_source), "Emissions factor data source",
  # "population_data_source", class(emissions_all$population_data_source), "Population data source",
  "emissions_per_capita", class(emissions_all$emissions_per_capita), "Metric tons CO~2~e per person living in given county for given sector and category"
)) %>% 
  arrange(match(Column, names(emissions_all)))


# waldo::compare(emissions_all, readRDS("_meta/data/ctu_emissions.RDS"))

saveRDS(emissions_all, "_meta/data/ctu_emissions.RDS")
saveRDS(emissions_all_meta, "_meta/data/ctu_emissions_meta.RDS")
write.csv(emissions_all, "_meta/data/ctu_emissions.CSV", row.names = FALSE)

# save emissions to shared drive location
# source("R/fetch_path.R")

# if (fs::dir_exists(fetch_path())) {
#   write.csv(emissions_all, paste0(fetch_path(), "/cprg_county_emissions.CSV"), row.names = FALSE)
# }
