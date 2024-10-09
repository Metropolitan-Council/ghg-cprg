#### Script to read in and process EPA GHG FLIGHT data
source("R/_load_pkgs.R")

cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_ctu <- readRDS("_meta/data/cprg_ctu.RDS") %>% 
  mutate(city_name = str_to_title(gsub("ST.", "Saint", ctu_name)))
industrial_hub <- readRDS("_meta/data/epa_ghg_factor_hub.RDS") %>% 
  extract2("industrial_combustion")


### download emissions by fuel type data: https://www.epa.gov/ghgreporting/data-sets
ind_unit_data <-  
  read_excel(file.path(here::here(), 
                       "_industrial/data-raw/emissions_by_unit_and_fuel_type_c_d_aa_09_2023.xlsx"),
                                       sheet = "UNIT_DATA",
                             skip = 6) %>% 
  clean_names() %>% 
  filter(state %in% c("MN","WI"),
         #remove power plants and municipal waste (doublecounting)
         !grepl("D", industry_type_subparts),
         !grepl("HH", industry_type_subparts),
         !industry_type_sectors == "Power Plants")%>% 
  mutate(city_name = str_to_title(gsub("ST.", "Saint", city, ignore.case = TRUE))) %>% 
  inner_join(.,cprg_ctu %>% select(state_abb, city_name, county_name)%>% 
               st_drop_geometry(),
             by= c("state" = "state_abb", "city_name"))

ind_fuel_data <-  read_excel(file.path(here::here(), "_industrial/data-raw/emissions_by_unit_and_fuel_type_c_d_aa_09_2023.xlsx"),
                             sheet = "FUEL_DATA",
                             skip = 5) %>% 
  clean_names() %>% 
  filter(state %in% c("MN","WI"),
         #remove power plants and municipal waste (doublecounting)
         !grepl("D", industry_type_subparts),
         !grepl("HH", industry_type_subparts),
         !industry_type_sectors == "Power Plants") %>% 
  mutate(city_name = str_to_title(gsub("ST.", "Saint", city, ignore.case = TRUE))) %>% 
  inner_join(.,cprg_ctu %>% select(state_abb, city_name, county_name) %>% 
               st_drop_geometry(),
             by= c("state" = "state_abb", "city_name"))

#unique fuel types and matching to ghg factor hub

unique(ind_fuel_data$specific_fuel_type)
unique(industrial_hub$`Fuel type`)

ind_fuel_data %>% filter(!specific_fuel_type %in% industrial_hub$`Fuel type`) %>% 
  distinct(specific_fuel_type)

#only 5, manually changing them to match hub
ind_fuel_data <- ind_fuel_data %>% 
  mutate(corrected_fuel_type = case_when(
    specific_fuel_type == "Natural Gas (Weighted U.S. Average)" ~ "Natural Gas",
    specific_fuel_type == "Wood and Wood Residuals (dry basis)" ~ "Wood and Wood Residuals",
    specific_fuel_type == "Coke" ~ "Coal Coke",
    specific_fuel_type == "Liquefied petroleum gases (LPG)" ~ "Liquefied Petroleum Gases (LPG)",
    specific_fuel_type == "Subbituminous" ~ "Sub-bituminous Coal",
    TRUE ~ specific_fuel_type
  ))

### create conversion factors to back-translate CO2e to activity data



### are all fuel data facilities present in unit data?

ind_fuel_data %>% 
  filter(!frs_id %in% ind_unit_data$frs_id) %>% 
  nrow() # 0 - Yes

### are all fuel data units in unit data units?

ind_fuel_data %>% 
  filter(!unit_name %in% ind_unit_data$unit_name) %>% 
  nrow() # 0 - Yes

# are units repeated or unique?
ind_unit_data %>% filter(reporting_year == 2021) %>% 
  distinct(unit_name) # 72

ind_fuel_data %>% 
  filter(duplicated(paste0(reporting_year, unit_name))) %>% 
  distinct(facility_name, unit_name, general_fuel_type) %>% 
  print(n = 100)
# there are 35 units that have multiple fuel type emissions (roughly half)
# Because total CO2e is only reported in unit_type data, creativity is required
## One option is to use methane or n2o proportion between fuel types to 
## further partition total emissions. This is non-ideal as different fuels
## will have different proportionality of co2/ch4/n2o emissions

ind_fuel_data <- ind_fuel_data %>% 
  group_by(reporting_year, unit_name) %>% 
  mutate(total_ch4 = sum(fuel_methane_ch4_emissions_mt_co2e),
         total_n2o = sum(fuel_nitrous_oxide_n2o_emissions_mt_co2e)) %>% 
  ungroup() %>% 
  mutate(percent_ch4 = fuel_methane_ch4_emissions_mt_co2e / total_ch4,
         percent_n2o = fuel_nitrous_oxide_n2o_emissions_mt_co2e / total_n2o)


