# Confirm that the values listed in the 2021 GHG emission factors
# match those used in the LGGIT
source("R/_load_pkgs.R")

# Federal Register EPA; 40 CFR Part 98; e-CFR, (see link below). Table C-1 (as amended at 81 FR 89252, Dec. 9, 2016).
kg_co2_per_unit <- readxl::read_xlsx("_energy/data-raw/ghg-emission-factors-hub-2021.xlsx",
  range = "C97:E107"
)


# Source: EPA (2020) Inventory of U.S. Greenhouse Gas Emissions and Sinks: 1990-2018.
#  All values are calculated from Tables A-106 through A-110.
gasoline_factor <- readxl::read_xlsx("_energy/data-raw/ghg-emission-factors-hub-2021.xlsx",
  range = "C115:F217"
) %>%
  mutate(`Fuel Type` = "Gasoline")


# Source: EPA (2020) Inventory of U.S. Greenhouse Gas Emissions and Sinks: 1990-2018.
# All values are calculated from Tables A-113 through A-114.
alt_fuel_factor <- readxl::read_xlsx("_energy/data-raw/ghg-emission-factors-hub-2021.xlsx",
  range = "C222:G258"
) %>%
  mutate(`Vehicle Type` = case_when(
    `Vehicle Type` == "Light Truck (Vans, Pickup Trucks, SUVs)" ~ "Light Truck",
    TRUE ~ `Vehicle Type`
  ))



# visually inspected and matched values
gasoline_factor %>% View()

alt_fuel_factor

lggit_kg_emissions_per_mile
