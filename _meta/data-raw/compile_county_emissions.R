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

#sloppy shortcut

transportation_emissions <- read_csv("C:\\Users\\WilfahPA\\Documents\\CPRG\\transportation_subsector_rdg.csv") %>% 
#transportation_emissions <- readRDS("_transportation/data/county_vmt_emissions.RDS") %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    sector = "Transportation",
    geog_level = "county",
    geog_name = county_name,
    category = paste0(stringr::str_to_sentence(vehicle_weight_label), " vehicles"),
    source = paste0(vehicle_weight_label, " vehicles"),
    data_source = "NEI",
    factor_source = paste0("EPA MOVES (", nei_inventory_year , ")")
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
ww_emissions <- readRDS("_waste/data/epa_county_wastewater_2005_2021.RDS") %>%
  mutate(
    sector = "Waste",
    geog_level = "county",
    geog_name = NAME,
    category = "Wastewater",
    source = "Wastewater",
    data_source = "EPA State GHG Inventory and Projection Tool",
    factor_source = data_source,
    emissions_metric_tons_co2e = co2e,
    year = as.numeric(year)
  ) %>%
  select(names(transportation_emissions))


## solid waste -----
solid_waste <- readRDS("_waste/data/mn_sw_emissions_co2e.RDS") %>%
  ungroup() %>%
  mutate(
    sector = "Waste",
    geog_level = "county",
    geog_name = geog_name,
    category = "Solid waste",
    source = str_to_sentence(source),
    data_source = "MPCA SCORE",
    factor_source = "EPA GHG Emission Factors Hub (2021)",
    year = as.numeric(year)
  ) %>%
  select(names(transportation_emissions))



# energy -----

electric_natgas_nrel_proportioned <- readRDS("_energy/data/electric_natgas_nrel_proportioned.RDS")

## electricity ----

electric_emissions <- electric_natgas_nrel_proportioned %>%
  filter(source == "Electricity") %>%
  filter((year == 2005 & category == "Total") | (year == 2021 & category != "Total")) %>% #avoid duplication and NAs until category is infilled later
  mutate(
    sector = "Electricity",
    geog_level = "county",
    geog_name = county,
    category = paste0(category, " energy"),
    source = source,
    data_source = "Individual electric utilities, NREL SLOPE (2021)",
    factor_source = "eGRID MROW",
    year = as.numeric(year)
  ) %>%
  select(names(transportation_emissions))


## natural gas ----

natural_gas_emissions <- electric_natgas_nrel_proportioned %>%
  filter(source == "Natural gas") %>%
  filter((year == 2005 & category == "Total") | (year == 2021 & category != "Total")) %>% #avoid duplication and NAs until category is infilled later
  mutate(
    sector = "Building Fuel",
    geog_level = "county",
    geog_name = county,
    category = paste0(category, " energy"),
    source = source,
    data_source = "Individual natural gas utilities, NREL SLOPE (2021)",
    factor_source = "EPA GHG Emission Factors Hub (2021)",
    year = as.numeric(year)
  ) %>%
  select(names(transportation_emissions))

## propane and kerosene ----

propane_kerosene_emissions <- readRDS("_energy/data/fuel_use.RDS") %>%
  mutate(
    sector = "Building Fuel",
    geog_level = "county",
    geog_name = NAME,
    category = "Liquid stationary fuels",
    source = stringr::str_to_sentence(fuel_type),
    data_source = "EIA RECS (2020)",
    factor_source = "EPA GHG Emission Factors Hub (2021)"
  ) %>%
  select(names(transportation_emissions))

## agriculture ----

livestock_emissions <- readRDS("_agriculture/data/county_livestock_emissions_2005_2021.rds") %>% 
  mutate(
    sector = "Agriculture",
    geog_level = "county",
    geog_name = county_name,
    category = "Livestock",
    emissions_metric_tons_co2e = MT_co2e,
    source = stringr::str_to_sentence(source),
    data_source = "USDA Census",
    factor_source = "EPA State Inventory Tool"
  )  %>%
  select(names(transportation_emissions))

cropland_emissions <- readRDS("_agriculture/data/county_cropland_emissions_2005_2021.rds") %>% 
  ungroup() %>% 
  mutate(
    sector = "Agriculture",
    geog_level = "county",
    geog_name = county_name,
    category = "Cropland",
    emissions_metric_tons_co2e = MT_co2e,
    source = stringr::str_to_sentence(source),
    data_source = "USDA Census",
    factor_source = "EPA State Inventory Tool"
  )  %>%
  select(names(transportation_emissions))

## natural systems ----

# further work to be done reconciling ESA and NLCD data
natural_systems_sequestration_esa <- readRDS("_nature/data/county_landcover_sequestration_2021.RDS") %>%
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
    year = as.numeric(year)
  ) %>%
  ungroup() %>%
  select(names(transportation_emissions))

natural_systems_sequestration_nlcd <- readRDS("_nature/data/nlcd_county_landcover_sequestration_2001_2021.RDS") %>%
  mutate(
    sector = "Nature",
    geog_level = "county",
    geog_name = county,
    category = "Sequestration",
    source = stringr::str_to_sentence(str_replace_all(land_cover_type, "_", " ")),
    data_source = "NLCD 2021",
    factor_source = "Various primary literature",
    emissions_metric_tons_co2e = sequestration_potential,
    year = as.numeric(year)
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
  livestock_emissions,
  cropland_emissions,
  natural_systems_sequestration_nlcd,
  natural_systems_stock
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
        "Light-duty vehicles",
        "Heavy-duty vehicles",
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
        geog_id = COUNTYFP,
        population_year,
        county_total_population = population,
        population_data_source
      ),
    by = join_by(geog_id, year == population_year)
  ) %>%
  rowwise() %>%
  mutate(emissions_per_capita = round(emissions_metric_tons_co2e / county_total_population, digits = 2)) %>%
  select(year, geog_level, geog_id, geog_name, everything())


# splitting off carbon stock here as it is a capacity, not a rate
carbon_stock <- emissions_all %>% filter(category == "Stock")
emissions_all <- emissions_all %>% filter(category != "Stock")

mean(emissions_all$emissions_per_capita[!emissions_all$category == "Stock"])
sum(emissions_all$emissions_metric_tons_co2e[!emissions_all$category == "Stock"]) / sum(cprg_county_pop$population)

### break out desired years and data sources for RDG 90%
emissions_rdg_90_baseline <- emissions_all %>% 
  filter(year %in% c(2005,2021), !geog_name %in% c("Sherburne", "Chisago", "St. Croix", "Pierce")) %>% 
  group_by(year, sector) %>% 
  summarize(MT_CO2e = sum(emissions_metric_tons_co2e)) %>% 
  left_join(., cprg_county_pop %>% group_by(population_year) %>% summarize(population = sum(population)),
            by = c('year' = 'population_year')
            ) %>% 
  mutate(emissions_per_capita = MT_CO2e / population) %>%
  mutate(sector = factor(sector, levels = c("Electricity", "Transportation", "Building Fuel", "Waste", "Agriculture", "Nature")))


#### remove later

baseline_comparison <- ggplot(emissions_rdg_90_baseline %>% filter(year %in% c(2005, 2021)),
                                                     aes(x = sector, y = MT_CO2e, fill = as.factor(year))) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  labs(fill = "Year")

# Define custom colors for sectors and tones for years
sector_colors <- c("Electricity" = "#1f77b4", 
                   "Transportation" = "#8c564b", 
                   "Building Fuel" = "#9467bd", 
                   "Waste" = "#d62728", 
                   "Agriculture" = "#ff7f0e", 
                   "Nature" = "#2ca02c")

# Function to lighten colors for different years
lighten <- function(color, factor=1.4){
  col <- col2rgb(color)
  col <- col + (255 - col) * (factor - 1)
  col[col > 255] <- 255
  rgb(col[1], col[2], col[3], maxColorValue=255)
}

# Custom palette for years
custom_palette <- c(
  "Electricity.2005" = lighten(sector_colors["Electricity"], 1.4),
  "Electricity.2021" = unname(sector_colors["Electricity"]),
  "Transportation.2005" = lighten(sector_colors["Transportation"], 1.4),
  "Transportation.2021" = unname(sector_colors["Transportation"]),
  "Building Fuel.2005" = lighten(sector_colors["Building Fuel"], 1.4),
  "Building Fuel.2021" = unname(sector_colors["Building Fuel"]),
  "Waste.2005" = lighten(sector_colors["Waste"], 1.4),
  "Waste.2021" = unname(sector_colors["Waste"]),
  "Agriculture.2005" = lighten(sector_colors["Agriculture"], 1.4),
  "Agriculture.2021" = unname(sector_colors["Agriculture"]),
  "Nature.2005" = lighten(sector_colors["Nature"], 1.4),
  "Nature.2021" = unname(sector_colors["Nature"])
)

emissions_rdg_90_baseline <- emissions_rdg_90_baseline %>%
  mutate(sector_year = interaction(sector, as.factor(year), sep = "."))

# Plot with custom settings
baseline_comparison <- ggplot(emissions_rdg_90_baseline %>% filter(year %in% c(2005, 2021)),
                              aes(x = sector, y = MT_CO2e, fill = sector_year)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  labs(fill = "Sector and Year") +
  scale_fill_manual(values = custom_palette) +
  theme_minimal()


baseline_comparison

baseline_comparison_per_capita <- ggplot(emissions_rdg_90_baseline %>% filter(year %in% c(2005, 2021)),
                              aes(x = sector, y = emissions_per_capita, fill = as.factor(year))) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  labs(fill = "Year")

baseline_comparison_per_capita


#quick calcs
emissions_rdg_90_baseline %>% filter(year == 2021, sector == "Transportation") %>% pull(MT_CO2e) %>% sum() /
  emissions_rdg_90_baseline %>% filter(year == 2005, sector != "Nature") %>% pull(MT_CO2e) %>% sum()

emissions_rdg_90_baseline %>% filter(year == 2021, sector == "Waste") %>% pull(MT_CO2e) %>% sum() /
  emissions_rdg_90_baseline %>% filter(year == 2005, sector != "Nature") %>% pull(MT_CO2e) %>% sum()

emissions_rdg_90_baseline %>% filter(year == 2021, sector == "Nature") %>% pull(MT_CO2e) %>% sum() /
  emissions_rdg_90_baseline %>% filter(year == 2005, sector != "Nature") %>% pull(MT_CO2e) %>% sum()



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
write.csv(emissions_rdg_90_baseline, "_meta/data/baseline_emissions_rdg.csv", row.names = FALSE)


saveRDS(carbon_stock, "_meta/data/cprg_county_carbon_stock.RDS")
saveRDS(emissions_all_meta, "_meta/data/cprg_county_carbon_stock_meta.RDS")

# save emissions to shared drive location
# source("R/fetch_path.R")

# if (fs::dir_exists(fetch_path())) {
#   write.csv(emissions_all, paste0(fetch_path(), "/cprg_county_emissions.CSV"), row.names = FALSE)
# }

msa_subsector_inv <- emissions_all %>% 
  filter(year  == 2021, !geog_name %in% c("Sherburne", "Chisago", "St. Croix", "Pierce"), sector != "Nature") %>% 
  group_by(year,geog_name,sector, category,county_total_population ) %>%
  summarise(
    emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e),
    .groups = "keep"
  ) %>% 
  mutate(emissions_per_capita = emissions_metric_tons_co2e / county_total_population) %>% 
  ungroup() %>% 
  select(-county_total_population)

write.csv(msa_subsector_inv, "_meta/data/subsector_emissions_rdg.csv", row.names = FALSE)

msa_subsector_inv %>% filter(year == 2021, sector == "Transportation", category == "Light-duty vehicles") %>% pull(emissions_metric_tons_co2e) %>% sum() / 
msa_subsector_inv %>% filter(year == 2021, sector == "Transportation") %>% pull(emissions_metric_tons_co2e) %>% sum() 



### sequestration by area

cprg_area <- cprg_county %>%
  mutate(area_sq_mi = sf::st_area(cprg_county) %>% units::set_units("mi^2") %>%
           as.numeric()) %>% select(NAME, area_sq_mi) %>% st_drop_geometry()

msa_sequestration <- left_join(emissions_all %>% 
  filter(year  == 2021, !geog_name %in% c("Sherburne", "Chisago", "St. Croix", "Pierce"), sector == "Nature"),
  cprg_area, 
  by = c("geog_name" = "NAME")) %>% 
  group_by(year,geog_name,sector, source,area_sq_mi) %>%
  summarise(
    sequestration_metric_tons_co2e = sum(emissions_metric_tons_co2e),
    .groups = "keep"
  ) %>% 
  mutate(sequestration_per_sq_mi = sequestration_metric_tons_co2e / area_sq_mi) %>% 
  ungroup() %>% 
  select(-area_sq_mi)
  
county_seq <- ggplot(
  msa_sequestration,
  aes(x = geog_name, y = sequestration_per_sq_mi / 1000000, fill = source)
) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = color_palette_vector) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.45, size = 14),
    axis.title.y = element_text(size = 16)
  ) +
  ylab("Millions of metric tons of CO2e") +
  xlab("") +
  labs(fill = "Subsector")


write.csv(msa_sequestration, "_meta/data/natural_systems_sequestration_rdg.csv", row.names = FALSE)
