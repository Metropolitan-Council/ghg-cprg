### compile all ctu electricity emissions

## ctu and county data
cprg_ctu <- read_rds("_meta/data/cprg_ctu.RDS") %>%
  filter(!county_name %in% c("Chisago", "Sherburne", "St. Croix", "Pierce")) 
cprg_county <- read_rds("_meta/data/cprg_county.RDS") %>%
  filter(!county_name %in% c("Chisago", "Sherburne", "St. Croix", "Pierce")) 
ctu_population <- read_rds("_meta/data/ctu_population.RDS") %>% 
  left_join(cprg_county %>% st_drop_geometry() %>% select(geoid, county_name)) %>%
  filter(!county_name %in% c("Chisago", "Sherburne", "St. Croix", "Pierce")) 

# assign cities to counties where majority of population is
ctu_county_unique <- ctu_population %>%
  group_by(ctu_name, ctu_class) %>%
  filter(ctu_population == max(ctu_population)) %>%
  ungroup() %>% 
  distinct(geoid, ctuid, ctu_name, ctu_class, county_name)

# county activity data
county_mwh <- readRDS("_energy/data/minnesota_county_elec_ActivityAndEmissions.rds")

## create storage frame of unique city and utility combos with all years
ctu_utility_year <- readRDS("_energy/data/ctu_utility_intersect.rds") %>% 
  cross_join(data.frame(inventory_year = c(2007:2023))) %>% 
  mutate(residential_mwh = NA,
         business_mwh = NA,
         total_mwh = NA)

## load formatted SQL utility data
sql_elec <- readRDS("_energy/data/ctu_electricity_emissions_2015_2018.rds") %>% 
  mutate(ctu_class = if_else(grepl("Twp.", ctu_name), "TOWNSHIP", "CITY"),
         ctu_name = str_replace_all(ctu_name, " Twp.", ""),
         ctu_name = str_replace_all(ctu_name, "St. ", "Saint "),
         ctu_class = if_else(ctu_name %in% c("Credit River", "Empire"),
                             "CITY",
                             ctu_class)) %>% 
  filter(units_emissions == "Metric tons CO2",
         !is.na(mwh_per_year)) %>%  # removes duplicates
 mutate(sector = if_else(customer_class == "Residential",
                         "Residential",
                         "Business")) %>%
  group_by(ctu_name, emissions_year, data_source, sector) %>%
  summarise(mwh_per_year = sum(mwh_per_year, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = sector, values_from = mwh_per_year, 
              names_glue = "{tolower(sector)}_mwh") %>% 
  mutate(total_mwh = replace_na(business_mwh, 0) + replace_na(residential_mwh, 0))

## load and format connexus data
connexus <- readRDS("_energy/data/connexus_activityData_2014_2023.rds") %>% 
  filter(!is.na(mwh_delivered)) %>% 
  mutate(mwh_delivered = mwh_delivered * 10e-3) %>%  # kwh listed instead of mwh
  mutate(sector_use = case_when(
    sector == "Residential" ~ "Residential",
    sector == "Residential/Commercial/Industrial" ~ "Total",
    TRUE ~ "Business"))
  
xcel <- readRDS("_energy/data/Xcel_activityData_2015_2023.rds")

rii <- readRDS("_energy/data/rii_electricity_2007_2023.rds")

#load in municipal utility data
munis <- readRDS("_energy/data/MNelecMunis_activityData_2014_2023.rds")
