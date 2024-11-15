##### read in MPCA data fuel combustion data

source("R/_load_pkgs.R")
source("R/_")

cprg_county <- readRDS("_meta/data/cprg_county.rds") %>% 
  st_drop_geometry()
cprg_ctu <- readRDS("_meta/data/cprg_ctu.rds")%>% 
  st_drop_geometry()
ctu_population <- readRDS("_meta/data/ctu_population.rds")
ghg_factor_hub <- readRDS("_meta/data/epa_ghg_factor_hub.rds")

mpca_fuel <- read_xlsx("_industrial/data-raw/MPCA_industrial_fuel_throughput.xlsx",
                       sheet = 1) %>% 
  clean_names()

### format data


mpca_fuel_formatted <- mpca_fuel %>% 
  ## bring in county and ctu IDs
  mutate(county_name = str_to_sentence(county_name)) %>% 
  filter(county_name %in% c(cprg_county$county_name),
         ### not calculating power plant landfill emissions here
         !sector %in% c("Electric Power", "Waste", "Transportation")) %>% 
  left_join(cprg_county %>% select(county_name, geoid),
            by = "county_name") %>% 
  left_join(ctu_population %>% 
              # going to assume most facilities are in cities not townships
              filter(ctu_class  != "TOWNSHIP") %>% 
              distinct(ctu_name, ctuid),
            by = c("geo_city_name" = "ctu_name")) %>% 
  ### clean variables
  mutate(inventory_year = as.numeric(gsub("EI ","", inventory_id)),
         value_activity = case_when(
           grepl("E3", activity_unit_code) ~ activity_amt * 1000,
           grepl("E6", activity_unit_code) ~ activity_amt * 1000000,
           TRUE ~ activity_amt
         ),
         unit_activity = case_when(
           grepl("FT3", activity_unit_code) ~ "cubic_feet",
           grepl("GAL", activity_unit_code) ~ "gallon",
           grepl("BBL", activity_unit_code) ~ "barrel",
           grepl("Ton", activity_unit_code) ~ "standard_ton"
         ),
         fuel_type = str_to_title(standard_material_code))# %>% 
  mutate(fuel_type = case_when(
    
  ))
  select(county_name, county_id = geoid, ctu_name = geo_city_name, ctuid,
         inventory_year, value_activity, unit_activity,standard_material_code,
         sector, naics_description)

  
