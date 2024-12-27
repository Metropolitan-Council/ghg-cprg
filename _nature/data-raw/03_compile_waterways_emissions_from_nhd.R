rm(list=ls())
source("R/_load_pkgs.R")
source("R/global_warming_potential.R")

overwrite_RDS <- TRUE

nhd_county <- readRDS("./_nature/data/nhd_county_waterways_2001_2021.rds")
nhd_ctu <- readRDS("./_nature/data/nhd_ctu_waterways_2001_2021.rds")
waterways_c <- readRDS("./_nature/data/waterways_emissions_factors.rds")



# load the county and ctu boundaries layer
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_ctu <- readRDS("_meta/data/cprg_ctu.RDS")

# turn your county and ctu layers into dataframes
cprg_county_df <- cprg_county %>% as.data.frame() %>%
  select(geoid, county_name, state_name) %>% sf::st_drop_geometry()

cprg_ctu_df <- cprg_ctu %>% as.data.frame() %>%
  select(gnis, geoid_wis, ctu_name, ctu_class, county_name, state_name) %>% 
  sf::st_drop_geometry() %>%
  mutate(
    geoid_wis = as.numeric(geoid_wis),
    gnis = as.numeric(gnis))





# Compute methane emissions for different waterways types by county
nhd_county_c <- nhd_county %>%
  left_join(., waterways_c, by = join_by(waterway_type)) %>%
  mutate(
    mt_ch4 = area * ef_mt_ch4, # multiply area in km2 by CH4 emissions factor
    mt_co2e = mt_ch4 * gwp$ch4  # convert to CO2 equivalent
    ) %>%
  left_join(., cprg_county_df, by = join_by(county_name, state_name)) %>%
  mutate(sector = "Nature",
         category = "Waterways",
         data_source = "National Hydrography Dataset and MPCA Surface Water Emissions Factors") %>%
  dplyr::select(geoid, county_name, state_name, inventory_year = year, 
                sector, category, source = waterway_type, data_source, 
                factor_source, area, value_emissions = mt_ch4, units_emissions, mt_co2e) %>%
  arrange(inventory_year, county_name, source)



# Compute methane emissions for different waterways types by CTU
nhd_ctu_c <- nhd_ctu %>%
  left_join(., waterways_c, by = join_by(waterway_type)) %>%
  mutate(
    mt_ch4 = area * ef_mt_ch4,
    mt_co2e = mt_ch4 * gwp$ch4  
  ) %>%
left_join(., cprg_ctu_df, by = join_by(ctu_name, ctu_class, county_name, state_name)) %>%
  mutate(sector = "Nature",
         category = "Waterways",
         data_source = "National Hydrography Dataset and MPCA Surface Water Emissions Factors") %>%
  dplyr::select(gnis, geoid_wis, ctu_name, ctu_class, county_name, state_name,
                inventory_year = year, sector, category, source = waterway_type, data_source, 
                factor_source, area, value_emissions = mt_ch4, units_emissions, mt_co2e) %>%
  arrange(inventory_year, ctu_name, source)




# nhd_ctu_c %>%
#   filter(ctu_name %in% c("Minneapolis", "Saint Paul", "Edina")) %>%
#   ggplot() +
#   theme_minimal()+
#   geom_point(aes(x=year, y=emissions_mtCO2e, color=waterway_type)) +
#   facet_wrap(~ctu_name)


nhd_county_c_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "geoid", class(nhd_county_c$geoid), "County GEOID",
    "county_name", class(nhd_county_c$county_name), "County name",
    "state_name", class(nhd_county_c$state_name), "State name",
    "inventory_year", class(nhd_county_c$inventory_year), "Year of survey",
    "sector", class(nhd_county_c$sector), "Emissions sector. One of Transportation, Energy, Waste, Nature, Agriculture",
    "category", class(nhd_county_c$category), "Category of emissions within given sector",
    "source", class(nhd_county_c$source), "Source of emissions. Most detailed sub-category in this table",
    "data_source", class(nhd_county_c$data_source), "Activity data source",
    "factor_source", class(nhd_county_c$factor_source), "Emissions factor data source",
    "value_emissions", class(nhd_county_c$value_emissions), "Numerical value of emissions",
    "units_emissions", class(nhd_county_c$units_emissions), "Units and gas type of emissions",
    "mt_co2e", class(nhd_county_c$mt_co2e), "Metric tons of gas in CO2 equivalency"
  )



nhd_ctu_c_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "gnis", class(nhd_ctu_c$gnis), "Minnesota geographic identifier",
    "geoid_wis", class(nhd_ctu_c$geoid_wis), "Wisconsin geographic identifier",
    "ctu_name", class(nhd_ctu_c$ctu_name), "City, township, unorganized territory, or village name",
    "ctu_class", class(nhd_ctu_c$ctu_class), "City class (City, township, unorganized territory, or village)",
    "county_name", class(nhd_ctu_c$county_name), "County name",
    "state_name", class(nhd_ctu_c$state_name), "State name",
    "inventory_year", class(nhd_ctu_c$inventory_year), "Year of survey",
    "sector", class(nhd_ctu_c$sector), "Emissions sector. One of Transportation, Energy, Waste, Nature, Agriculture",
    "category", class(nhd_ctu_c$category), "Category of emissions within given sector",
    "source", class(nhd_ctu_c$source), "Source of emissions. Most detailed sub-category in this table",
    "data_source", class(nhd_ctu_c$data_source), "Activity data source",
    "factor_source", class(nhd_ctu_c$factor_source), "Emissions factor data source",
    "value_emissions", class(nhd_ctu_c$value_emissions), "Numerical value of emissions",
    "units_emissions", class(nhd_ctu_c$units_emissions), "Units and gas type of emissions",
    "mt_co2e", class(nhd_ctu_c$mt_co2e), "Metric tons of gas in CO2 equivalency"
  )




# User chooses whether to overwrite the rds files
if (overwrite_RDS) {
  saveRDS(nhd_county_c, paste0("./_nature/data/nhd_county_waterways_emissions_", head(sort(unique(nhd_county_c$inventory_year)), 1), "_", tail(sort(unique(nhd_county_c$inventory_year)), 1), ".rds"))
  saveRDS(nhd_county_c_meta, paste0("./_nature/data/nhd_county_waterways_emissions_", head(sort(unique(nhd_county_c$inventory_year)), 1), "_", tail(sort(unique(nhd_county_c$inventory_year)), 1), "_meta.rds"))
 
  saveRDS(nhd_ctu_c, paste0("./_nature/data/nhd_ctu_waterways_emissions_", head(sort(unique(nhd_ctu_c$inventory_year)), 1), "_", tail(sort(unique(nhd_ctu_c$inventory_year)), 1), ".rds"))
  saveRDS(nhd_ctu_c_meta, paste0("./_nature/data/nhd_ctu_waterways_emissions_", head(sort(unique(nhd_ctu_c$inventory_year)), 1), "_", tail(sort(unique(nhd_ctu_c$inventory_year)), 1), "_meta.rds"))
}





