# 10 tables
source("R/_load_pkgs.R")
# Table 1: Stationary Combustion -----
#
# Source:
# Federal Register EPA; 40 CFR Part 98; e-CFR, (see link below). Table C-1, Table C-2 (as amended at 81 FR 89252, Dec. 9, 2016), Table AA-1 (78 FR 71965, Nov. 29, 2013).
# https://www.ecfr.gov/cgi-bin/text-idx?SID=ae265d7d6f98ec86fcd8640b9793a3f6&mc=true&node=pt40.23.98&rgn=div5#ap40.23.98_19.1
# Note: Emission factors are per unit of heat content using higher heating values (HHV).
# If heat content is available from the fuel supplier, it is preferable to use that value.
# If not, default heat contents are provided.
# solid

coal_coke <- readxl::read_xlsx("_meta/data-raw/ghg-emission-factors-hub-2021.xlsx",
  range = "C17:J25",
  col_names = c(
    "Fuel type",
    "mmBtu per short ton",
    "kg CO2 per mmBtu",
    "g CH4 per mmBtu",
    "g N2O per mmBtu",
    "kg CO2 per short ton",
    "g CH4 per short ton",
    "g N2O per short ton"
  )
) %>%
  mutate(fuel_category = "Coal and Coke")

other_solid <- readxl::read_xlsx("_meta/data-raw/ghg-emission-factors-hub-2021.xlsx",
  range = "C27:J30",
  col_names = c(
    "Fuel type",
    "mmBtu per short ton",
    "kg CO2 per mmBtu",
    "g CH4 per mmBtu",
    "g N2O per mmBtu",
    "kg CO2 per short ton",
    "g CH4 per short ton",
    "g N2O per short ton"
  )
) %>%
  mutate(fuel_category = "Other Fuels - Solid")

biomass_solid <- readxl::read_xlsx("_meta/data-raw/ghg-emission-factors-hub-2021.xlsx",
  range = "C32:J35",
  col_names = c(
    "Fuel type",
    "mmBtu per short ton",
    "kg CO2 per mmBtu",
    "g CH4 per mmBtu",
    "g N2O per mmBtu",
    "kg CO2 per short ton",
    "g CH4 per short ton",
    "g N2O per short ton"
  )
) %>%
  mutate(fuel_category = "Biomass Fuels - Solid")

# gases
natural_gas <- readxl::read_xlsx("_meta/data-raw/ghg-emission-factors-hub-2021.xlsx",
  range = "C38:J38",
  col_names = c(
    "Fuel type",
    "mmBtu per scf",
    "kg CO2 per mmBtu",
    "g CH4 per mmBtu",
    "g N2O per mmBtu",
    "kg CO2 per scf",
    "g CH4 per scf",
    "g N2O per scf"
  )
) %>%
  mutate(fuel_category = "Natural Gas")

other_fuel_gas <- readxl::read_xlsx("_meta/data-raw/ghg-emission-factors-hub-2021.xlsx",
  range = "C40:J43",
  col_names = c(
    "Fuel type",
    "mmBtu per scf",
    "kg CO2 per mmBtu",
    "g CH4 per mmBtu",
    "g N2O per mmBtu",
    "kg CO2 per scf",
    "g CH4 per scf",
    "g N2O per scf"
  )
) %>%
  mutate(fuel_category = "Other Fuels - Gaseous")

biomass_gas <- readxl::read_xlsx("_meta/data-raw/ghg-emission-factors-hub-2021.xlsx",
  range = "C45:J46",
  col_names = c(
    "Fuel type",
    "mmBtu per scf",
    "kg CO2 per mmBtu",
    "g CH4 per mmBtu",
    "g N2O per mmBtu",
    "kg CO2 per scf",
    "g CH4 per scf",
    "g N2O per scf"
  )
) %>%
  mutate(fuel_category = "Biomass Fuels - Gaseous")


# liquid
petroleum <- readxl::read_xlsx("_meta/data-raw/ghg-emission-factors-hub-2021.xlsx",
  range = "C49:J78",
  col_names = c(
    "Fuel type",
    "mmBtu per gallon",
    "kg CO2 per mmBtu",
    "g CH4 per mmBtu",
    "g N2O per mmBtu",
    "kg CO2 per gallon",
    "g CH4 per gallon",
    "g N2O per gallon"
  )
) %>%
  mutate(fuel_category = "Petroleum Products")


biomass_liquid <- readxl::read_xlsx("_meta/data-raw/ghg-emission-factors-hub-2021.xlsx",
  range = "C80:J83",
  col_names = c(
    "Fuel type",
    "mmBtu per gallon",
    "kg CO2 per mmBtu",
    "g CH4 per mmBtu",
    "g N2O per mmBtu",
    "kg CO2 per gallon",
    "g CH4 per gallon",
    "g N2O per gallon"
  )
) %>%
  mutate(fuel_category = "Biomass Fuels - Liquid")


biomass_fuels_pulp <- readxl::read_xlsx("_meta/data-raw/ghg-emission-factors-hub-2021.xlsx",
  range = "C85:J89",
  col_names = c(
    "Fuel type",
    "mmBtu per gallon",
    "kg CO2 per mmBtu",
    "g CH4 per mmBtu",
    "g N2O per mmBtu",
    "kg CO2 per gallon",
    "g CH4 per gallon",
    "g N2O per gallon"
  )
) %>%
  mutate(fuel_category = "Biomass Fuels - Kraft Pulping Liquor, by Wood Furnish")


stationary_combustion <- bind_rows(
  bind_rows(
    coal_coke,
    other_solid,
    biomass_solid
  ) %>%
    mutate(fuel_form = "Solid") %>%
    pivot_longer(2:8,
      names_to = "fuel_factor"
    ),
  bind_rows(
    natural_gas,
    other_fuel_gas,
    biomass_gas
  ) %>%
    mutate(fuel_form = "Gas") %>%
    pivot_longer(2:8,
      names_to = "fuel_factor"
    ),
  bind_rows(
    petroleum,
    biomass_liquid,
    biomass_fuels_pulp
  ) %>%
    mutate(fuel_form = "Liquid") %>%
    pivot_longer(2:8,
      names_to = "fuel_factor"
    )
) %>%
  tidyr::separate_wider_delim(fuel_factor,
    delim = "per",
    names = c(
      "emission",
      "per_unit"
    )
  ) %>%
  mutate(
    emission = stringr::str_trim(emission),
    per_unit = stringr::str_trim(per_unit),
    Source = "Federal Register EPA; 40 CFR Part 98"
  )



# Table 2: Mobile Combustion CO2 -----
# Source:
# Federal Register EPA; 40 CFR Part 98; e-CFR, (see link below). Table C-1 (as amended at 81 FR 89252, Dec. 9, 2016).
# https://www.ecfr.gov/cgi-bin/text-idx?SID=ae265d7d6f98ec86fcd8640b9793a3f6&mc=true&node=pt40.23.98&rgn=div5#ap40.23.98_19.1
# LNG:  The factor was developed based on the CO2 factor for Natural Gas factor and LNG fuel density from GREET1_2020.xlsx Model, Argonne National Laboratory.


kg_co2_per_unit <- readxl::read_xlsx("_meta/data-raw/ghg-emission-factors-hub-2021.xlsx",
  range = "C97:E107"
)

# Table 3: Mobile Combustion CH4 and N2O for On-Road Gasoline Vehicles -----
# Source: EPA (2020) Inventory of U.S. Greenhouse Gas Emissions and Sinks: 1990-2018. All values are calculated from Tables A-106 through A-110.

passenger_gas <- read_xlsx("_meta/data-raw/ghg-emission-factors-hub-2021.xlsx",
  range = "D116:F149",
  col_names = c(
    "Vehicle Year",
    "g CH4 per mile",
    "g N2O per mile"
  )
) %>%
  mutate(
    vehicle_type = "Passenger Car",
    fuel_type = "Gasoline"
  )

light_duty_gas <- read_xlsx("_meta/data-raw/ghg-emission-factors-hub-2021.xlsx",
  range = "D150:F186",
  col_names = c(
    "Vehicle Year",
    "g CH4 per mile",
    "g N2O per mile"
  )
) %>%
  mutate(
    vehicle_type = "Light-Duty Truck",
    fuel_type = "Gasoline"
  )


heavy_duty_gas <- read_xlsx("_meta/data-raw/ghg-emission-factors-hub-2021.xlsx",
  range = "D187:F215",
  col_names = c(
    "Vehicle Year",
    "g CH4 per mile",
    "g N2O per mile"
  )
) %>%
  mutate(
    vehicle_type = "Heavy-Duty Vehicle",
    fuel_type = "Gasoline"
  )

moto_gas <- read_xlsx("_meta/data-raw/ghg-emission-factors-hub-2021.xlsx",
  range = "D216:F217",
  col_names = c(
    "Vehicle Year",
    "g CH4 per mile",
    "g N2O per mile"
  )
) %>%
  mutate(
    vehicle_type = "Motorcycle",
    fuel_type = "Gasoline"
  )


onroad_gas <- bind_rows(
  passenger_gas,
  light_duty_gas,
  heavy_duty_gas,
  moto_gas
)

# Table 4: Mobile Combustion CH4 and N2O for On-Road Diesel and Alternative Fuel Vehicles -----
# Source: EPA (2020) Inventory of U.S. Greenhouse Gas Emissions and Sinks: 1990-2018. All values are calculated from Tables A-109 through A-112.

onroad_alt <- read_xlsx("_meta/data-raw/ghg-emission-factors-hub-2021.xlsx",
  range = "E223:G232",
  col_names = c(
    "Vehicle Year",
    "g CH4 per mile",
    "g N2O per mile"
  )
) %>%
  mutate(
    vehicle_type = c(
      rep("Passenger Car", 4),
      rep("Light-Duty Truck", 4),
      rep("Medium and Heavy-Duty Vehciles", 2)
    ),
    fuel_type = "Diesel"
  ) %>%
  bind_rows(
    read_xlsx("_meta/data-raw/ghg-emission-factors-hub-2021.xlsx",
      range = "D233:G258",
      col_names = c(
        "fuel_type",
        "Vehicle Year",
        "g CH4 per mile",
        "g N2O per mile"
      )
    ) %>%
      mutate(vehicle_type = c(
        rep("Light-Duty Car", 5),
        rep("Light-Duty Truck", 5),
        rep("Medium-Duty Truck", 4),
        rep("Heavy-Duty Truck", 6),
        rep("Bus", 6)
      ))
  )


# Table 5: Mobile Combustion CH4 and N2O for Non-Road Vehicles-----
# Source: EPA (2020) Inventory of U.S. Greenhouse Gas Emissions and Sinks: 1990-2018. All values are calculated from Tables A-113 through A-114.
# "Notes:
# A Includes equipment, such as tractors and combines, as well as fuel consumption from trucks that are used off-road in agriculture.
# B Includes equipment, such as cranes, dumpers, and excavators, as well as fuel consumption from trucks that are used off-road in construction. "


nonroad_alt <- read_xlsx("_meta/data-raw/ghg-emission-factors-hub-2021.xlsx",
  range = "D264:F303",
  col_names = c(
    "fuel_type",
    "g CH4 per mile",
    "g N2O per mile"
  )
) %>%
  mutate(
    vehicle_type =
      c(
        rep("Ships and Boats", 4),
        rep("Locomotives", 1),
        rep("Aircraft", 2),
        rep("Agricultural Equipment", 4),
        rep("Agricultural Offroad Trucks", 2),
        rep("Construction/Mining Equipment", 4),
        rep("Construction/Mining Offroad Trucks", 2),
        rep("Lawn and Garden Equipment", 4),
        rep("Airport Equipment", 3),
        rep("Industrial/Commercial Equipment", 4),
        rep("Logging Equipment", 3),
        rep("Railroad Equipment", 3),
        rep("Recreational Equipment", 4)
      )
  ) %>%
  mutate(`g CH4 per mile` = as.numeric(`g CH4 per mile`))


# compile transportation tables -----

transportation_tbl345 <- onroad_gas %>%
  mutate(category = "On-Road") %>%
  bind_rows(
    onroad_alt %>%
      mutate(category = "On-Road"),
    nonroad_alt %>%
      mutate(category = "Non-Road")
  ) %>%
  select(
    category,
    vehicle_type,
    fuel_type,
    `Vehicle Year`,
    everything()
  ) %>%
  pivot_longer(cols = 5:6) %>%
  tidyr::separate_wider_delim(name,
    delim = "per",
    names = c(
      "emission",
      "per_unit"
    )
  ) %>%
  mutate(across(where(is.character), str_trim))


# Table 6: Electricity ----
# Source: EPA eGRID2019, February 2021
# Note: Total output emission factors can be used as default factors for estimating GHG emissions
# from electricity use when developing a carbon footprint or emissions inventory.
# Annual non-baseload output emission factors should not be used for those purposes,
# but can be used to estimate GHG emissions reductions from reductions in electricity use.


egrid <- readxl::read_xlsx("_meta/data-raw/ghg-emission-factors-hub-2021.xlsx",
  range = "C312:I339",
  col_names = c(
    "eGrid Subregion",
    "Total output:lb CO2 per MWh",
    "Total output:lb CH4 per MWh",
    "Total output:lb N2O per MWh",
    "Non-baseload:lb CO2 per MWh",
    "Non-baseload:lb CH4 per MWh",
    "Non-baseload:lb N2O per MWh"
  )
) %>%
  pivot_longer(2:7) %>%
  tidyr::separate_wider_delim(name,
    delim = ":",
    names = c(
      "factor_type",
      "unit"
    )
  ) %>%
  tidyr::separate_wider_delim(unit,
    delim = "per",
    names = c(
      "emission",
      "per_unit"
    )
  ) %>%
  mutate(across(1:4, stringr::str_trim)) %>%
  mutate(Source = "EPA eGRID2019, February 2021")



# Table: 7	Steam and Heat ----
# Note: Emission factors are per mmBtu of steam or heat purchased.
# These factors assume natural gas fuel is used to generate steam or heat at 80 percent thermal efficiency.
steam_heat <- readxl::read_xlsx("_meta/data-raw/ghg-emission-factors-hub-2021.xlsx",
  range = "C377:F377",
  col_names = c(
    "steam_heat",
    "kg CO2 per mmBtu",
    "g CH4 per mmBtu",
    "g N2O per mmBtu"
  )
) %>%
  pivot_longer(2:4) %>%
  tidyr::separate_wider_delim(name,
    delim = "per",
    names = c(
      "emission",
      "per_unit"
    )
  ) %>%
  mutate(across(1:3, stringr::str_trim))


# Scope 3 ----
# Scope 3 emission factors provided below are aligned with the Greenhouse Gas Protocol Technical Guidance for Calculating Scope 3 Emissions, version 1.0 (Scope 3 Calculation Guidance).
# Where applicable, the specific calculation method is referenced.
# Refer to the Scope 3 Calculation Guidance for more information (http://www.ghgprotocol.org/scope-3-technical-calculation-guidance).

## Table 8: Scope 3 Category 4: Upstream Transportation and Distribution and Category 9: Downstream Transportation and Distribution----

# These factors are intended for use in the distance-based method defined in the Scope 3 Calculation Guidance.
# If fuel data are available, then the fuel-based method should be used, with factors from Tables 2 through 5.
# "Source:
# CO2, CH4, and N2O emissions data for road vehicles are from Table 2-13 of the EPA (2020) Inventory of U.S. Greenhouse Gas Emissions and Sinks: 1990-2018.
# Vehicle-miles and passenger-miles data for road vehicles are from Table VM-1 of the Federal Highway Administration Highway Statistics 2018.
# CO2e emissions data for non-road vehicles are based on Table A-124 of the EPA (2020)
# Inventory of U.S. Greenhouse Gas Emissions and Sinks: 1990-2018, which are distributed into CO2, CH4, and N2O emissions based on fuel/vehicle emission factors.
# Freight ton-mile data are from Table 1-50 of the Bureau of Transportation Statistics, National Transportation Statistics for 2020 (Data based on 2018)."
# "Notes:
# Vehicle-mile factors are appropriate to use when the entire vehicle is dedicated to transporting the reporting company's product.
#  Ton-mile factors are appropriate when the vehicle is shared with products from other companies.
# A Passenger car: includes passenger cars, minivans, SUVs, and small pickup trucks (vehicles with wheelbase less than 121 inches).
# B Light-duty truck: includes full-size pickup trucks, full-size vans, and extended-length SUVs (vehicles with wheelbase greater than 121 inches).
# C Aircraft: updates due to a methodology change."


scope3_cat4_transportation <- readxl::read_xlsx("_meta/data-raw/ghg-emission-factors-hub-2021.xlsx",
  range = "D386:G392",
  col_names = c(
    "kg CO2 per unit",
    "g CH4 per unit",
    "g N2O per unit",
    "Units"
  )
) %>%
  mutate(vehicle_type = c(
    "Medium and Heavy-Duty Truck",
    "Passenger Car",
    "Light-Duty Truck",
    "Medium and Heavy-Duty Truck",
    "Rail",
    "Waterborne Craft",
    "Aircraft"
  )) %>%
  pivot_longer(1:3) %>%
  tidyr::separate_wider_delim(name,
    delim = "per",
    names = c(
      "emission",
      "per_unit"
    )
  ) %>%
  mutate(across(1:3, stringr::str_trim)) %>%
  select(
    vehicle_type,
    emission,
    per_unit,
    Units,
    value
  )


## Table 9: Scope 3 Category 5: Waste Generated in Operations and Category 12: End-of-Life Treatment of Sold Products----
# Source: EPA, Office of Resource Conservation and Recovery (February 2016) Documentation for Greenhouse Gas Emission and Energy Factors used in the Waste Reduction Model (WARM). Factors from tables provided in the Management Practices Chapters and Background Chapters. WARM Version 15, November 2020 Update. Additional data provided  by EPA, WARM-15 Background Data.
# "Notes: These factors do not include any avoided emissions impact from any of the disposal methods. All the factors presented here include transportation emissions, which are optional in the Scope 3 Calculation Guidance, with an assumed average distance traveled to the processing facility. AR4 GWPs are used to convert all waste emission factors into CO2e.

# A Recycling emissions include transport to recycling facility and sorting of recycled materials at material recovery facility.
# B Landfilling emissions include transport to landfill, equipment use at landfill and fugitive landfill CH4 emissions.  Landfill CH4 is based on typical landfill gas collection practices and average landfill moisture conditions.
# C Combustion emissions include transport to combustion facility and combustion-related non-biogenic CO2 and N2O
# D Composting emissions include transport to composting facility, equipment use at composting facility and CH4 and N2O emissions during composting. "

scope3_cat5_cat12_waste <- readxl::read_xlsx("_meta/data-raw/ghg-emission-factors-hub-2021.xlsx",
  range = "C400:I459",
  col_names = c(
    "Material",
    "Recycled",
    "Landfilled",
    "Combusted",
    "Composted",
    "Anaerobically Digested (Dry Digestate with Curing)",
    "Anaerobically Digested (Wet  Digestate with Curing)"
  )
) %>%
  mutate(across(2:7, as.numeric)) %>%
  pivot_longer(2:7) %>%
  mutate(
    units = "metric tons CO2e per short ton material",
    Source = "EPA WARM version 15, November 2020"
  )

## Table 10: Scope 3 Category 6: Business Travel and Category 7: Employee Commuting 				----
# "Source:
# CO2, CH4, and N2O emissions data for highway vehicles are from Table 2-13 of the EPA (2020) Inventory of U.S. Greenhouse Gas Emissions and Sinks: 1990–2018.
# Vehicle-miles and passenger-miles data for highway vehicles are from Table VM-1 of the Federal Highway Administration Highway Statistics 2018.
# Fuel consumption data and passenger-miles data for rail are from Tables A.14 to A.16 and C.9 to C.11 of the Transportation Energy Data Book: Edition 39. Fuel consumption was converted to emissions by using fuel and electricity emission factors presented in the tables above.
# Intercity Rail factors from personal communication with Amtrak (Laura Fotiou), March 2020.  These are based on 2019 values.
# Air Travel factors from 2020 Guidelines to Defra / DECC's GHG Conversion Factors for Company Reporting.  Version 1.0 July 2020."
# "Notes:
# A Passenger car: includes passenger cars, minivans, SUVs, and small pickup trucks (vehicles with wheelbase less than 121 inches).
# B Light-duty truck: includes full-size pickup trucks, full-size vans, and extended-length SUVs (vehicles with wheelbase greater than 121 inches).
# C Intercity rail: Amtrak long-distance rail between major cities. Northeast Corridor extends from Boston to Washington D.C. Other Routes are all routes outside the Northeast Corridor.
# D Commuter rail: rail service between a central city and adjacent suburbs (also called regional rail or suburban rail).
# E Transit rail: rail typically within an urban center, such as subways, elevated railways, metropolitan railways (metro), streetcars, trolley cars, and tramways. "

scope3_cat6_commuting <- readxl::read_xlsx("_meta/data-raw/ghg-emission-factors-hub-2021.xlsx",
  range = "D466:G477",
  col_names = c(
    "kg CO2 per unit",
    "g CH4 per unit",
    "g N2O per unit",
    "Units"
  )
) %>%
  mutate(
    vehicle_type =
      c(
        "Passenger Car",
        "Light-Duty Truck",
        "Motorcycle",
        "Intercity Rail - Northeast Corridor",
        "Intercity Rail - Other Routes",
        "Intercity Rail - National Average",
        "Commuter Rail",
        "Transit Rail (i.e. Subway, Tram)",
        "Bus",
        "Air Travel - Short Haul (< 300 miles)",
        "Air Travel - Medium Haul (>= 300 miles, < 2300 miles)",
        "Air Travel - Long Haul (>= 2300 miles)"
      )
  ) %>%
  mutate(across(1:3, as.numeric)) %>%
  pivot_longer(1:3) %>%
  tidyr::separate_wider_delim(name,
    delim = "per",
    names = c(
      "emission",
      "per_unit"
    )
  ) %>%
  mutate(across(1:4, stringr::str_trim)) %>%
  select(
    vehicle_type,
    emission,
    per_unit,
    Units,
    value
  )


# Table 11-12: Global Warming Potentials (GWPs) -----
# !IMPORTANT: we are not using these values
# "Source:
# 100-year GWPs from IPCC Fourth Assessment Report (AR4), 2007.  IPCC AR4 was published in 2007 and is among the most current and comprehensive peer-reviewed assessments of climate change. AR4 provides revised GWPs of several GHGs relative to the values provided in previous assessment reports, following advances in scientific knowledge on the radiative efficiencies and atmospheric lifetimes of these GHGs and of CO2. Because the GWPs provided in AR4 reflect an improved scientific understanding of the radiative effects of these gases in the atmosphere, the values provided are more appropriate for supporting the overall goal of organizational GHG reporting than the Second Assessment Report (SAR) GWP values previously used in the Emission Factors Hub.
# While EPA recognizes that Fifth Assessment Report (AR5) GWPs have been published, in an effort to ensure consistency and comparability of GHG data between EPA’s voluntary and non-voluntary GHG reporting programs (e.g. GHG Reporting Program and National Inventory), EPA recommends the use of AR4 GWPs. The United States and other developed countries to the UNFCCC have agreed to submit annual inventories in 2015 and future years to the UNFCCC using GWP values from AR4, which will replace the current use of SAR GWP values.  Utilizing AR4 GWPs improves EPA’s ability to analyze corporate, national, and sub-national GHG data consistently, enhances communication of GHG information between programs, and gives outside stakeholders a consistent, predictable set of GWPs to avoid confusion and additional burden."

# glob_warm_pot <- readxl::read_xlsx("_meta/data-raw/ghg-emission-factors-hub-2021.xlsx",
#                   range = "C485:D517")

# Table 12: Global Warming Potentials (GWPs) for Blended Refrigerants
# "Source:
# 100-year GWPs from IPCC Fourth Assessment Report (AR4), 2007.  See the source note to Table 11 for further explanation.
# GWPs of blended refrigerants are based on their HFC and PFC constituents, which are based on data
#  from http://www.epa.gov/ozone/snap/refrigerants/refblend.html."

# glob_warm_pot_refrig <- readxl::read_xlsx("_meta/data-raw/ghg-emission-factors-hub-2021.xlsx",
#                                    range = "C522:E558")


# compile all -----


epa_ghg_factor_hub <- list(
  "egrid" = egrid %>%
    filter(
      `eGrid Subregion` == "MROW (MRO West)",
      factor_type == "Total output"
    ),
  "stationary_combustion" = stationary_combustion %>%
    filter(`Fuel type` %in% c(
      "Propane",
      "Kerosene",
      "Natural Gas",
      "Kerosene-Type Jet Fuel"
    )),
  "industrial_combustion" = stationary_combustion,
  "mobile_combustion" = kg_co2_per_unit,
  "mobile_combustion_other" = transportation_tbl345,
  "waste" = scope3_cat5_cat12_waste %>%
    filter(
      name %in% c(
        "Landfilled",
        "Composted",
        "Combusted",
        "Recycled"
      ),
      Material %in% c(
        "Mixed MSW",
        "Mixed Organics",
        "Mixed Recyclables"
      )
    )
)

# manual adjustment of eGRID MROW values -- 2021 Factor Hub used 2019 eGRID; add year to reference
# need to adjust emissions code that applies these factors to 2021 activity data (and 2005!)
new_values_2021 <- c(995.8, 0.107, 0.015)
epa_ghg_factor_hub[[1]] <- epa_ghg_factor_hub[[1]] %>%
  mutate(
    across(
      value,
      ~ replace(., 1:3, new_values_2021)
    ),
    year = 2021,
    Source = "EPA eGRID2021, January 2023"
  )


# manually add 2005 eGRID information (CH4 and N2O output emissions rates reported as 28 and 30.71 lb/GWh in 2005, which translate to .028 and .03071 lb/MWh respectively)
eGRID_values_2005 <- c(1821.84, 0.028, 0.03071)

# Extract/copy the three rows of 2021 data to use as a template for new values
template_rows <- epa_ghg_factor_hub[[1]][1:3, ]

# Create new rows for 2005 by updating year, source, and value; other columns remain the same as the 2021 values
new_rows_2005 <- template_rows %>%
  mutate(
    value = c(1821.84, 0.028, 0.03071),
    year = 2005,
    Source = "EPA eGRID2005, December 2008"
  )

# Bind the new rows to the original data
epa_ghg_factor_hub[[1]] <- bind_rows(epa_ghg_factor_hub[[1]], new_rows_2005)

saveRDS(epa_ghg_factor_hub, "_meta/data/epa_ghg_factor_hub.RDS")

