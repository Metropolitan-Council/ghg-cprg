# The LGGIT tool has a template for adding data in bulk to the Excel workbook. 
# Lets try to construct that from the VMT data we calculated

source("R/_load_pkgs.R")

vmt_emissions <- readRDS(file.path(here::here(), "_transportation/data/county_vmt_emissions.RDS"))
tbi_vehicle_fuel_age <- readRDS("_transportation/data-raw/tbi/tbi_vehicle_fuel_age.RDS") %>% 
  mutate(`Fuel Type` = case_when(fuel == "Diesel" ~ "Diesel",
                                 TRUE ~ "Gasoline"),
         `Vehicle Type` = "Passenger Car",
         `Unit Description` = paste0(`Vehicle Type`, " ", `Fuel Type`),
         Sector = "Residential",
         `Vehicle Year` = year_median,
         `Vehicle Model (optional)` = "")

vmt_emissions_weight <- vmt_emissions %>%
  group_by(vehicle_type, vehicle_weight, vehicle_weight_label) %>%
  summarise(
    # vmt_same = sum(vmt_same),
    vmt_total = sum(vmt_total),
    emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e),
    .groups = "keep"
  ) %>% 
  ungroup()

lggit_structure <- tribble(
  ~`ID`, # Enter unique ascending integers for each ID#. Gaps between numbers are permitted. An ID is required for entry into the Module.
  ~`Unit Description`, # Use this field to describe or name the vehicle or vehicle group that you are entering. This field is for the user's reference only and does not impact the calculations. It can represent any number of vehicles.
  ~`Sector`, # c("Residential", "Commercial/Institutional", "Industrial", "Energy Generation")
  ~`Vehicle Year`,  # If you have chosen to enter data by model year, enter the model year of the vehicle or group of vehicles. If not known, leave blank. Model year data enables a more accurate emissions calculation.
  ~`Vehicle Type`, # c("Passenger Car", "Light Truck (Vans, Pickup Trucks, SUVs)",  "Heavy Duty Vehicle", "Motorcycle", "Agricultural Equipment", "Construction Equipment", "Utility and Recreational Equipment", "Aircraft", "Ship or Boat", "Locomotive")
  ~`Vehicle Model (optional)`, 
  ~`Fuel Type`, # c("Gasoline", "Diesel", "Biodiesel (B5)", "Biodiesel (B20)", "Ethanol (E5)", "CNG", "LPG")
  ~`Fuel Consumption`, # Enter the amount of that fuel the vehicle or vehicle group used during the inventory year. Amounts for all fuel types must be reported in gallons, except for CNG which should be reported in GGE.
  ~`VMT` # Enter the number of miles traveled for the unit or group during the inventory time period for Passenger Cars, Light Trucks, Heavy Duty Vehicles, and Motorcycles. This is required for calculating emissions of non-CO2 GHGs. If VMT is unknown, you can multiply the fuel consumption in Column I by the MPG of the vehicle/vehicle group. If MPG is unknown, see Columns P-R for average MPGs.
)


lggit_avg_mpg <- tibble::tribble(
  ~ "Vehicle Type", ~"Gasoline & Other Fuels", ~"Diesel & Biodiesel",
  "Passenger Car",        24.1,  32.4,
  "Light Truck",          18.5,  22.1, 
  "Heavy-Duty Vehicle",   10.13, 12.96,
  "Motorcycle",           50,    NA
) %>% 
  # make new vehicle_weight column 
  # we are going to assume light trucks are medium duty vehicles
  mutate(vehicle_weight = case_when(`Vehicle Type` == "Passenger Car" ~ "Passenger",
                                    `Vehicle Type` == "Light Truck" ~ "Medium",
                                    `Vehicle Type` == "Heavy-Duty Vehicle" ~ "Heavy"),
         # we assume that most passenger vehicles are gasoline
         # most medium and heavy are diesel
         # `Fuel Type` = case_when(vehicle_weight == "Passenger" ~ "Gasoline",
         #                         TRUE ~ "Diesel"),
         avg_mpg = case_when(vehicle_weight == "Passenger" ~ `Gasoline & Other Fuels`,
                             TRUE ~ `Diesel & Biodiesel`),
         # all passenger cars are residential
         # all others are commercial 
         Sector = case_when(`Vehicle Type` == "Passenger Car" ~ "Residential",
                            TRUE ~ "Commercial/Institutional"))


vmt_entry <- vmt_emissions_weight %>% 
  left_join(lggit_avg_mpg) %>% 
  arrange(vehicle_weight_label) %>% 
  mutate(VMT = vmt_total,
         # `Fuel Consumption` = VMT * avg_mpg,
         `Unit Description` = vehicle_weight_label,
         # ID = row_number(),
         `Vehicle Year` = NA,
         `Vehicle Model (optional)` = "")



# get passenger car VMT
passenger_vmt <- vmt_entry %>% 
  filter(`Vehicle Type` == "Passenger Car") %>% 
  magrittr::extract2("VMT")


lggit_vmt_entries <- tbi_vehicle_fuel_age %>%
  # split passenger car VMT between gasoline and diesel
  # based on regional distribution (est_pct)
  mutate(VMT = passenger_vmt * est_pct) %>% 
  # select only needed cols
  select(`Fuel Type`, `Unit Description`, `Vehicle Type`,
         `Vehicle Model (optional)`,
         `Vehicle Year`, VMT, Sector) %>% 
  # bind medium and heavy duty table
  bind_rows(vmt_entry %>% 
              filter(vehicle_type != "passenger")) %>% 
  # create ID
  mutate(ID = row_number()) %>% 
  # select only needed columns
  select(c("ID", "Unit Description", "Sector", "Vehicle Year", "Vehicle Type", 
           "Vehicle Model (optional)", "Fuel Type",
           "VMT")) %>% 
  left_join(lggit_avg_mpg)  %>% 
  rowwise() %>% 
  mutate(
    # fix fuel type
    `Fuel Type` = case_when(`Vehicle Type` != "Passenger Car" ~ "Diesel",
                            TRUE ~ `Fuel Type`),
    # discern which MPG to use from fuel type
    avg_mpg = case_when(`Fuel Type` == "Gasoline" ~ `Gasoline & Other Fuels`,
                        TRUE ~ `Diesel & Biodiesel`),
    # gallons = miles * (miles/gallon)
    `Fuel Consumption` = VMT * avg_mpg
  ) %>%  
  select(names(lggit_structure))

write.csv(lggit_vmt_entries,
          "_transportation/data-raw/epa/community_ghg_inventorytool_11.28.23/lggit_vmt_entries.CSV",
          row.names = FALSE)
