source("R/_load_pkgs.R")
if (!exists("gwp")) {
  source(file.path(here::here(), "R/global_warming_potential.R"))
}

landfill <- readRDS(file.path(here::here(), "_waste/data/landfill_MN_allyrs.RDS"))
incineration <- readRDS(file.path(here::here(), "_waste/data/incineration_MN_allyrs.RDS"))
organics <- readRDS(file.path(here::here(), "_waste/data/organics_MN_allyrs.RDS"))

solid_waste_mn_by_gas <- landfill %>%
  bind_rows(incineration, organics)

# meta

solid_waste_mn_by_gas_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "geoid", class(solid_waste_mn_by_gas$geoid), "5-digit FIPS code",
    "source", class(solid_waste_mn_by_gas$source), "Subcategory-specific source (e.g., Landfill)",
    "inventory_year", class(solid_waste_mn_by_gas$inventory_year), "Emissions estimation year",
    "value_activity", class(solid_waste_mn_by_gas$value_activity), "Activity data value (from SCORE)",
    "units_activity", class(solid_waste_mn_by_gas$units_activity), "Activity data units",
    "value_emissions", class(solid_waste_mn_by_gas$value_emissions), "Emissions value",
    "units_emissions", class(solid_waste_mn_by_gas$units_emissions), "Emissions units",
  )
# save RDS
saveRDS(solid_waste_mn_by_gas, "_waste/data/solid_waste_MN_by_gas.RDS")
saveRDS(solid_waste_mn_by_gas_meta, "_waste/data/solid_waste_MN_by_gas_meta.RDS")

# multiply by gwp factors and sum to find total co2e emissions
solid_waste_mn <- solid_waste_mn_by_gas %>%
  pivot_wider(
    names_from = units_emissions,
    values_from = value_emissions
  ) %>%
  replace(is.na(.), 0) %>%
  mutate(
    ch4_co2e = `Metric tons CH4` * gwp$ch4,
    n2o_co2e = `Metric tons N2O` * gwp$n2o,
    sector = "Waste",
    category = "Solid waste",
    data_source = "MPCA SCORE Report",
    factor_source = "IPCC solid waste methodology"
  ) %>%
  mutate(
    value_emissions = ch4_co2e + n2o_co2e + `Metric tons CO2`,
    units_emissions = "Metric tons CO2e"
  ) %>%
  ungroup() %>%
  select(
    geoid,
    ctuid,
    inventory_year,
    sector,
    category,
    source,
    data_source,
    factor_source,
    value_activity,
    units_activity,
    value_emissions,
    units_emissions
  )

# write meta
solid_waste_mn_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "geoid", class(solid_waste_mn$geoid), "5-digit FIPS code",
    "inventory_year", class(solid_waste_mn$inventory_year), "Emissions estimation year",
    "sector", class(solid_waste_mn$sector), "Emissions sector (e.g., Waste)",
    "category", class(solid_waste_mn$category), "Category of emissions within given sector",
    "source", class(solid_waste_mn$source), "Subcategory-specific source (e.g., Landfill)",
    "data_source", class(solid_waste_mn$data_source), "Activity data source",
    "factor_source", class(solid_waste_mn$factor_source), "Emissions factor data source",
    "value_activity", class(solid_waste_mn$value_activity), "Activity data value (from SCORE)",
    "units_activity", class(solid_waste_mn$units_activity), "Activity data units",
    "value_emissions", class(solid_waste_mn$value_emissions), "Emissions value",
    "units_emissions", class(solid_waste_mn$units_emissions), "Emissions units",
  )

# save RDS
saveRDS(solid_waste_mn, "_waste/data/solid_waste_MN_allyrs.RDS")
saveRDS(solid_waste_mn_meta, "_waste/data/solid_waste_MN_allyrs_meta.RDS")
