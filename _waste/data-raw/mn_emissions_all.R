# source("R/_load_pkgs.R")
# score_data <- readRDS("_waste/data/mpca_score.RDS") #called in individual scripts
if (!exists("gwp")) {
  source("R/global_warming_potential.R")
}

# read in SCORE data, convert to metric tons, save as RDS
source("_waste/data-raw/mn_read_score_data.R")

# read in methane recovery data, save as RDS (UNFINISHED)
# source("_waste/data-raw/mn_methane_flaring.R")

# clean tables with mn waste composition data, save as RDS
source("_waste/data-raw/clean_tabula_tables.R")

# calculate landfill emissions (UNFINISHED), return landfill_emissions df
source("_waste/data-raw/mn_methane_commitment_model.R")

# calculate incineration emissions, return incineration_emissions df
# note that WTE must be listed as energy sector
source("_waste/data-raw/mn_incineration_emissions.R")

# calculate compost emissions, return compost_emissions df
source("_waste/data-raw/mn_compost_emissions.R")

solid_waste_emissions_metric_tons <- landfill_emissions %>% 
  bind_rows(incineration_emissions, compost_emissions) %>% 
  replace(is.na(.), 0)

# meta

solid_waste_emissions_mt_meta <- 
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "County", class(solid_waste_emissions_metric_tons$County), "Emissions estimation county",
    "Method", class(solid_waste_emissions_metric_tons$Method), "Solid waste subcategory (e.g., Landfill)",
    "Year", class(solid_waste_emissions_metric_tons$Year), "Emissions estimation year",
    "total_co2", class(solid_waste_emissions_metric_tons$total_co2), "Annual total metric tons of CO~2~  attributed to the given county",
    "total_ch4", class(solid_waste_emissions_metric_tons$total_ch4), "Annual total metric tons of CH~4~  attributed to the given county",
    "total_n2o", class(solid_waste_emissions_metric_tons$total_n2o), "Annual total metric tons of N~2~O  attributed to the given county",
  )
# save RDS
saveRDS(solid_waste_emissions_metric_tons, "_waste/data/mn_sw_emissions_by_gas.RDS")
saveRDS(solid_waste_emissions_mt_meta, "_waste/data/mn_sw_emissions_by_gas_meta.RDS")

# multiply by gwp factors and sum to find total co2e emissions
solid_waste_emissions_co2e <- solid_waste_emissions_metric_tons %>% 
  mutate(
    ch4_emissions_metric_tons_co2e = total_ch4 * gwp$ch4,
    n2o_emissions_metric_tons_co2e = total_n2o * gwp$n2o,
    sector = "Waste",
    category = "Solid waste",
    source = case_when(
      Method == "WTE" ~ "Waste to energy",
      TRUE ~ Method
    )
  ) %>% 
  mutate(
    emissions_metric_tons_co2e = ch4_emissions_metric_tons_co2e + n2o_emissions_metric_tons_co2e + total_co2
  ) %>% 
  select(
    year = Year,
    geog_name = County,
    sector,
    category,
    source,
    emissions_metric_tons_co2e
  )

# write meta
solid_waste_emissions_co2e_meta <- 
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "year", class(solid_waste_emissions_co2e$year), "Emissions estimation year",
    "geog_name", class(solid_waste_emissions_co2e$geog_name), "Name of geographic unit (city or county)",
    "sector", class(solid_waste_emissions_co2e$sector), "Emissions sector (Waste or Energy)",
    "category", class(solid_waste_emissions_co2e$category), "Sector subcategory (Solid waste or Waste to energy)",
    "source", class(solid_waste_emissions_co2e$source), "Subcategory-specific source (e.g., Landfill)",
    "emissions_metric_tons_co2e", class(solid_waste_emissions_co2e$emissions_metric_tons_co2e), "Annual total GHG emissions, in metric tons of CO~2~ equivalent, attributed to the given county"
  )

# save RDS
saveRDS(solid_waste_emissions_co2e, "_waste/data/mn_sw_emissions_co2e.RDS")
saveRDS(solid_waste_emissions_co2e_meta, "_waste/data/mn_sw_emissions_co2e_meta.RDS")

