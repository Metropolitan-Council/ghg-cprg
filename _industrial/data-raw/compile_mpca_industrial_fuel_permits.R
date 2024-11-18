##### read in MPCA data fuel combustion data

source("R/_load_pkgs.R")


cprg_county <- readRDS("_meta/data/cprg_county.rds") %>% 
  st_drop_geometry()
cprg_ctu <- readRDS("_meta/data/cprg_ctu.rds")%>% 
  st_drop_geometry()
ctu_population <- readRDS("_meta/data/ctu_population.rds")
ghg_factor_hub <- readRDS("_meta/data/epa_ghg_factor_hub.rds")

mpca_fuel <- read_xlsx("_industrial/data-raw/MPCA_industrial_fuel_throughput.xlsx",
                       sheet = 1) %>% 
  clean_names()

### create factor hub with matching fuel types to MPCA
fuel_emission_factors <- ghg_factor_hub$industrial_combustion %>% 
  mutate(fuel_type = case_when(
    grepl("Distillate Fuel", `Fuel type`) ~ "Distillate Oil",
    `Fuel type` == "Sub-bituminous Coal" ~ "Coal,Subbit",
    `Fuel type` == "Anthracite Coal" ~ "Coal,Anth",
    `Fuel type` == "Natural Gasoline" ~ "Gasoline",
    `Fuel type` == "Wood and Wood Residuals" ~ "Wood",
    `Fuel type` == "Liquefied Petroleum Gases (LPG)" ~ "Lpg",
    grepl("Residual Fuel", `Fuel type`) ~ "Residual Oil",
    # this next one is my best guess - fuel gas is large source in FLIGHT
    `Fuel type` == "Fuel Gas" ~ "Process Gas",
    `Fuel type` == "Coal Coke" ~ "Coke",
    `Fuel type` == "Municipal Solid Waste" ~ "Waste,Solid",
    `Fuel type` == "Kerosene-Type Jet Fuel" ~ "Jet Fuel",
    `Fuel type` == "Used Oil" ~ "Waste Oil",
    # leftovers with no clear analogue being lumped into other
    `Fuel type` %in% "Other Oil (>401 deg F)" ~ "Other Oil",
    TRUE ~ `Fuel type`
  )) %>% 
  group_by(fuel_category, fuel_form, emission, per_unit, fuel_type) %>% 
  summarize(value = mean(value)) %>% 
  arrange(fuel_type) 
### add in transportation and waste sectors later
# %>% 
#   bind_rows(.,
#             ghg_factor_hub$mobile_combustion %>% 
#               mutate(fuel_type = case_when(
#                 grepl("Distillate Fuel", `Fuel type`) ~ "Distillate Oil",
#                 `Fuel type` == "Sub-bituminous Coal" ~ "Coal,Subbit",
#                 `Fuel type` == "Bituminous Coal" ~ "Coal,Bit",
#                 `Fuel type` == "Anthracite Coal" ~ "Coal,Anth",
#                 `Fuel type` == "Natural Gasoline" ~ "Gasoline",
#                 `Fuel type` == "Wood and Wood Residuals" ~ "Wood",
#                 `Fuel type` == "Liquefied Petroleum Gases (LPG)" ~ "Lpg",
#                 grepl("Residual Fuel", `Fuel type`) ~ "Residual Oil",
#                 # this next one is my best guess - fuel gas is large source in FLIGHT
#                 `Fuel type` == "Fuel Gas" ~ "Process Gas",
#                 `Fuel type` == "Coal Coke" ~ "Coke",
#                 `Fuel type` == "Municipal Solid Waste" ~ "Waste,Solid",
#                 `Fuel type` == "Kerosene-Type Jet Fuel" ~ "Jet Fuel",
#                 `Fuel type` == "Used Oil" ~ "Waste Oil",
#                 # leftovers with no clear analogue being lumped into other
#                 `Fuel type` %in% "Other Oil (>401 deg F)" ~ "Other Oil",
#                 TRUE ~ `Fuel type`
#               )) %>% 
#               group_by(fuel_category, fuel_form, emission, per_unit, fuel_type) %>% 
#               summarize(value = mean(value)) %>% 
#               arrange(fuel_type))

### format data

mpca_fuel_formatted <- mpca_fuel %>% 
  ## bring in county and ctu IDs
  mutate(county_name = str_to_sentence(county_name)) %>% 
  filter(county_name %in% c(cprg_county$county_name),
         !sector %in% c("Waste", "Transportation", "Electric Power")) %>% 
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
         fuel_type = str_to_title(standard_material_code)) %>% 
  mutate(fuel_type = if_else(fuel_type %in% fuel_emission_factors$fuel_type,
                             fuel_type,"Other Oil")) %>% 
  select(county_name, county_id = geoid, ctu_name = geo_city_name, ctuid,
         inventory_year, value_activity, unit_activity,fuel_type,
         sector, naics_description)

  
mpca_fuel_emissions <- mpca_fuel_formatted %>% 
  left_join(.,fuel_emission_factors,
            by="fuel_type",
            relationship = "many-to-many")
