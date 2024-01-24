# compile emissions from all sectors into a single data table
source("R/_load_pkgs.R")

# transportation -----
transportation_emissions <- readRDS("_transportation/data/county_vmt_emissions.RDS") %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(sector = "Transportation",
         geog_level = "county",
         geog_name = zone,
         category = stringr::str_to_sentence(vehicle_type),
         source = paste0(vehicle_weight_label, " vehicles"),
         data_source = "StreetLight Data",
         factor_source = paste0("EPA MOVES (", moves_year, ")")) %>% 
  select(year,
         geog_level,
         geog_name,
         sector,
         category,
         source,
         emissions_metric_tons_co2e,
         data_source,
         factor_source)

# waste -----
## wastewater ----
## solid waste -----
# energy -----
## electricity ----
## natural gas ----
## propane ----

# combine and write metadata----

emissions_all <- bind_rows(transportation_emissions) %>% 
  mutate(source = factor(source,
                         c(
                           # transportation levels
                           "Light-duty vehicles",
                           "Medium-duty vehicles",
                           "Heavy-duty vehicles"
                           # waste levels
                           # energy levels
                           ),
                         ordered = TRUE))

emissions_all_meta <- tibble::tribble(
  ~"Column", ~"Class", ~"Description",
  "year", class(emissions_all$year), "Emissions estimation year",
  "geog_level", class(emissions_all$geog_level), "Geography level; city or county",
  "geog_name", class(emissions_all$geog_name), "Name of geographic area",
  "sector", class(emissions_all$sector), paste0("Emissions sector. One of ", 
                                                paste0(unique(emissions_all$sector), collapse = ", ")),
  "category", class(emissions_all$category), "Category of emissions within given sector",
  "source", class(emissions_all$source), "Source of emissions. Most detailed sub-category in this table",
  "emissions_metric_tons_co2e", class(emissions_all$emissions_metric_tons_co2e), "Annual total metric tons CO~2~ and CO~2~ equivalent attributed to the given geography for given year",
  "data_source",  class(emissions_all$data_source), "Activity data source",
  "factor_source",  class(emissions_all$factor_source), "Emissions factor data source"
)

saveRDS(emissions_all, "R/data/cprg_county_emissions.RDS")
saveRDS(emissions_all_meta, "R/data/cprg_county_emissions_meta.RDS")

