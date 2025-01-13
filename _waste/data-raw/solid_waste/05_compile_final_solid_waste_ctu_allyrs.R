source("R/_load_pkgs.R")

# pull in final RDS
solid_waste <- readRDS(file.path(here::here(), "_waste/data/final_solid_waste_allyrs.RDS"))
# pull in population proportion timeseries

ctu_population <- readRDS(file.path(here::here(), "_meta/data/ctu_population.RDS"))
# allocate to ctu by proportion

solid_waste_ctu <- solid_waste %>%
  right_join(
    ctu_population %>% filter(inventory_year > 2004),
    by = join_by(geoid, inventory_year),
    relationship = "many-to-many"
  ) %>%
  mutate(
    ctu_value_activity = value_activity * ctu_proportion_of_county_pop,
    ctu_value_emissions = value_emissions * ctu_proportion_of_county_pop
  ) %>%
  select(
    geoid,
    ctuid,
    inventory_year,
    sector,
    category,
    source,
    data_source,
    factor_source,
    value_activity = ctu_value_activity,
    units_activity,
    value_emissions = ctu_value_emissions,
    units_emissions
  )

solid_waste_ctu_meta <- tibble::tribble(
  ~"Column", ~"Class", ~"Description",
  "geoid", class(solid_waste_ctu$geoid), "5-digit FIPS county code",
  "ctuid", class(solid_waste_ctu$ctuid), "5-digit CTU code",
  "inventory_year", class(solid_waste_ctu$inventory_year), "Emissions estimation year",
  "sector", class(solid_waste_ctu$sector), "Emissions sector (e.g., Waste)",
  "category", class(solid_waste_ctu$category), "Category of emissions within given sector",
  "source", class(solid_waste_ctu$source), "Subcategory-specific source (e.g., Landfill)",
  "data_source", class(solid_waste_ctu$data_source), "Activity data source",
  "factor_source", class(solid_waste_ctu$factor_source), "Emissions factor data source",
  "value_activity", class(solid_waste_ctu$value_activity), "Activity data value (from SCORE)",
  "units_activity", class(solid_waste_ctu$units_activity), "Activity data units",
  "value_emissions", class(solid_waste_ctu$value_emissions), "Emissions value",
  "units_emissions", class(solid_waste_ctu$units_emissions), "Emissions units",
)

saveRDS(solid_waste_ctu, file.path(here::here(), "_waste/data/final_solid_waste_ctu_allyrs.RDS"))
saveRDS(solid_waste_ctu_meta, file.path(here::here(), "_waste/data/final_solid_waste_ctu_allyrs_meta.RDS"))
