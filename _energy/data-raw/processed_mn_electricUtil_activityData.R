# Process all Minnesota electric utility activity data

source("R/_load_pkgs.R")
source("_energy/data-raw/_energy_emissions_factors.R")

# Directory containing Excel files with utility annual reports -- 
# all reports were manually downloaded from 
# https://mn.gov/commerce/energy/industry-government/utilities/annual-reporting.jsp
# based on the contents of MNutilities_in_scope$utility_name

# NOTE: Great River Energy, which supplies energy to many MN electric co-ops,
#  reports their sales to the state and their reporting stands in for.... FILL IN
dir_mn_electricity <- here("_energy", "data-raw", "mn_utility_reporting")

# Get list of Excel files in the directory
file_list <- list.files(path = dir_mn_electricity, pattern = "\\.xlsx$", full.names = TRUE)

# Function to process each file and read the electricity delivered to each county by each utility
process_file <- function(file_path) {
  utility_name <- tools::file_path_sans_ext(basename(file_path))

  # Read specific ranges from the file
  data_A_C <- read_excel(file_path, sheet = "ElectricityByCounty", range = "A12:C56")
  data_E_G <- read_excel(file_path, sheet = "ElectricityByCounty", range = "E12:G53")

  # Rename columns
  colnames(data_A_C) <- c("countyCode", "county", "mWh_delivered")
  colnames(data_E_G) <- c("countyCode", "county", "mWh_delivered")

  # Combine the data from both ranges
  combined_data <- rbind(data_A_C, data_E_G)

  # Filter for specific counties
  combined_data <- combined_data %>%
    filter(county %in% c(
      "Anoka", "Carver", "Dakota", "Hennepin", "Ramsey",
      "Scott", "Sherburne", "Chisago", "Washington"
    ))

  # Add utility name
  combined_data$utility <- utility_name

  return(combined_data)
}

# Process all files and combine the data
# NOTE -- North Branch data is from 2022, no 2021 data available
combined_MNelectUtil_activityData <- do.call(rbind, lapply(file_list, process_file))


# manually add data from municipal utility financial reports
# Elk River -- 341,047.71 mWh delivered to customers in 2021, all goes to Sherburne county
# source: pg 54 https://www.ermumn.com/application/files/3316/5668/9846/2021_Annual_Financial_Report.pdf
sherburneElkRiverMuni_mWh <- 341047.71

combined_MNelectUtil_activityData <- combined_MNelectUtil_activityData %>%
  add_row(
    countyCode = 71,
    county = "Sherburne",
    mWh_delivered = sherburneElkRiverMuni_mWh,
    utility = "ElkRiverMunicipalUtilities"
  )



# New Prague Utilities -- 69,291.725 mWh delivered to customers in 2021,
# source: pg 18 https://www.ci.new-prague.mn.us/vertical/sites/%7BAD7ECB62-2C5E-4BA0-8F19-1426026AFA3E%7D/uploads/01-24-2022_Utilities_Commission_Meeting_Packet.pdf
# New Prague (Scott County portion) at 2020 census: 4706 (source: https://metrocouncil.org/Data-and-Maps/Publications-And-Resources/Files-and-reports/2022-Final-Population-Estimates-(PDF).aspx); total New Prague, MN pop is 8,162, per 2020 decennial Census
scottProp <- 4706 / 8162
scottNewPragueMuni_mWh <- scottProp * 69291.725
combined_MNelectUtil_activityData <- combined_MNelectUtil_activityData %>%
  add_row(
    countyCode = 70,
    county = "Scott",
    mWh_delivered = scottNewPragueMuni_mWh,
    utility = "NewPragueUtilitiesCommission"
  )

# Assuming each row in mn_electricity_data represents a utility's electricity delivery in a county, process and merge data -- this will be a separate data colelction process spanning excel reports submitted to state
processed_mn_elecUtil_activityData <- combined_MNelectUtil_activityData %>%
  mutate(
    CO2_emissions = mWh_delivered * eGRID_MROW_emissionsFactor_CO2,
    CH4_emissions = mWh_delivered * eGRID_MROW_emissionsFactor_CH4,
    N2O_emissions = mWh_delivered * eGRID_MROW_emissionsFactor_N2O
  )

MNcounty_level_electricity_emissions <- processed_mn_elecUtil_activityData %>%
  group_by(county) %>%
  summarise(
    total_CO2_emissions_lbs = sum(CO2_emissions, na.rm = TRUE),
    total_CO2_emissions_tons = total_CO2_emissions_lbs / 2000,
    total_CH4_emissions_lbs = sum(CH4_emissions, na.rm = TRUE),
    total_CH4_emissions_tons = total_CH4_emissions_lbs / 2000,
    total_N2O_emissions_lbs = sum(N2O_emissions, na.rm = TRUE),
    total_N2O_emissions_tons = total_N2O_emissions_lbs / 2000,
    total_CO2e_emissions_lbs = sum(
      CO2_emissions +
        CH4_emissions +
        N2O_emissions,
      na.rm = TRUE
    ),
    total_CO2e_emissions_tons = total_CO2e_emissions_lbs / 2000,
    emissions_metric_tons_co2e = total_CO2e_emissions_lbs %>% 
      units::as_units("pound") %>% 
      units::set_units("metric_ton") %>% 
      as.numeric()
  ) %>%
  mutate(
    state = "MN",
    sector = "Electricity",
    year = 2021
  )


write_rds(processed_mn_elecUtil_activityData, here("_energy", "data", "minnesota_elecUtils_ActivityAndEmissions.RDS"))
write_rds(MNcounty_level_electricity_emissions, here("_energy", "data", "minnesota_county_ElecEmissions.RDS"))
