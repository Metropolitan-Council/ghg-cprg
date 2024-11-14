source("R/_load_pkgs.R")
solid_waste_mn <- readRDS(file.path(here::here(), "_waste/data/solid_waste_MN_allyrs.RDS"))
solid_waste_wi <- readRDS(file.path(here::here(), "_waste/data/solid_waste_WI_allyrs.RDS"))

# combine data
final_solid_waste <- solid_waste_mn %>%
  bind_rows(solid_waste_wi)

final_solid_waste_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "geoid", class(final_solid_waste$geoid), "5-digit FIPS code",
    "inventory_year", class(final_solid_waste$inventory_year), "Emissions estimation year",
    "sector", class(final_solid_waste$sector), "Emissions sector (e.g., Waste)",
    "category", class(final_solid_waste$category), "Category of emissions within given sector",
    "source", class(final_solid_waste$source), "Subcategory-specific source (e.g., Landfill)",
    "data_source", class(final_solid_waste$data_source), "Activity data source",
    "factor_source", class(final_solid_waste$factor_source), "Emissions factor data source",
    "value_activity", class(final_solid_waste$value_activity), "Activity data value (from SCORE)",
    "units_activity", class(final_solid_waste$units_activity), "Activity data units",
    "value_emissions", class(final_solid_waste$value_emissions), "Emissions value",
    "units_emissions", class(final_solid_waste$units_emissions), "Emissions units",
  )

saveRDS(final_solid_waste, file.path(here::here(), "_waste/data/final_solid_waste_allyrs.RDS"))
saveRDS(final_solid_waste_meta, file.path(here::here(), "_waste/data/final_solid_waste_allyrs_meta.RDS"))
