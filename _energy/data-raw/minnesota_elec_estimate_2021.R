# Process all Minnesota electric utility activity data

source("R/_load_pkgs.R")
source("_energy/data-raw/_energy_emissions_factors.R")

# Directory containing Excel files with utility annual reports --
# all reports were manually downloaded from
# https://mn.gov/commerce/energy/industry-government/utilities/annual-reporting.jsp
# based on the contents of MNutilities_in_scope$utility_name

# root directory with folders for each utility in scope (with each folder containing subfolders for all years which reporting to the state is available)
dir_mn_electricity_state <- here("_energy", "data-raw", "mn_elec_utility_reporting_state")


# Function to get file paths, utility names, and years of utility reports in root directory
get_files <- function(root_dir) {
  file_info <- list()
  
  # Loop through each utility folder
  utility_folders <- list.dirs(root_dir, recursive = FALSE)
  for (utility_folder in utility_folders) {
    utility_name <- basename(utility_folder)
    
    # Loop through each year sub-folder within each utility folder
    year_folders <- list.dirs(utility_folder, recursive = FALSE)
    for (year_folder in year_folders) {
      year <- basename(year_folder)
      
      # Get list of Excel files in the year folder
      files <- list.files(path = year_folder, pattern = "\\.xlsx$", full.names = TRUE)
      
      # Append each file path with utility and year information
      for (file in files) {
        file_info <- append(file_info, list(list(file_path = file,
                                                 utility_name = utility_name,
                                                 year = year)))
      }
    }
  }
  return(file_info)
}

# function to process the file associatedf with each utility-year combo and extract activity (mWh) at the utility-year-county granularity electricity data
process_file <- function(file_info) {
  # Extract file path, utility name, and year from file_info (output nested list structure from get_files)
  file_path <- file_info$file_path
  utility_name <- file_info$utility_name
  year <- file_info$year
  
  # Read specific ranges from each file 
  data_A_C <- read_excel(file_path, sheet = "ElectricityByCounty", range = "A12:C56")
  data_E_G <- read_excel(file_path, sheet = "ElectricityByCounty", range = "E12:G53")
  
  # Rename columns
  colnames(data_A_C) <- c("countyCode", "county", "mWh_delivered")
  colnames(data_E_G) <- c("countyCode", "county", "mWh_delivered")
  
  # Combine the data from both ranges
  combined_data <- rbind(data_A_C, data_E_G)
  
  # Filter for specific counties, and exclude county rows a given utility didn't operate in that year
  combined_data <- combined_data %>%
    filter(county %in% c(
      "Anoka", "Carver", "Dakota", "Hennepin", "Ramsey",
      "Scott", "Sherburne", "Chisago", "Washington"
    )) %>%
    filter(!is.na(mWh_delivered))
  
  # Add utility name and year columns
  combined_data$utility <- utility_name
  combined_data$year <- as.numeric(year)  # Ensure year is numeric if needed
  
  return(combined_data)
}

# Apply process_file to each file identified in get_files() in the nested structure and combine the results
file_list <- get_files(dir_mn_electricity_state)
combined_MNelectUtil_activityData <- do.call(rbind, lapply(file_list, process_file))



# manual data collection to fill in gaps for 2021 as needed

# Elk River -- 341,047.71 mWh delivered to customers in 2021, all goes to Sherburne county (marginal amounts to Hennepin in other years)
# source: pg 54 https://www.ermumn.com/application/files/3316/5668/9846/2021_Annual_Financial_Report.pdf

sherburneElkRiverMuni_mWh_2021 <- 341047.71
combined_MNelectUtil_activityData <- combined_MNelectUtil_activityData %>%
  add_row(
    countyCode = 71,
    county = "Sherburne",
    mWh_delivered = sherburneElkRiverMuni_mWh_2021,
    utility = "Elk River Municipal Utilities",
    year = 2021
  )



# New Prague Utilities -- 69,291.725 mWh delivered to customers in 2021,
# source: pg 18 https://www.ci.new-prague.mn.us/vertical/sites/%7BAD7ECB62-2C5E-4BA0-8F19-1426026AFA3E%7D/uploads/01-24-2022_Utilities_Commission_Meeting_Packet.pdf

scottProp <- 45972 / 65674 
scottNewPragueMuni_mWh_2021 <- scottProp * 69291.725
combined_MNelectUtil_activityData <- combined_MNelectUtil_activityData %>%
  add_row(
    countyCode = 70,
    county = "Scott",
    mWh_delivered = scottNewPragueMuni_mWh_2021,
    utility = "New Prague Utilities Commission",
    year = 2021
  )

# Assuming each row in mn_electricity_data represents a utility's electricity delivery in a county,
# process and merge data -- this will be a separate data collection process spanning excel reports submitted to state
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
        (CH4_emissions * gwp$ch4) +
        (N2O_emissions * gwp$n2o),
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


write_rds(processed_mn_elecUtil_activityData, here("_energy", "data", "minnesota_elecUtils_ActivityAndEmissions_2021.RDS"))
write_rds(MNcounty_level_electricity_emissions, here("_energy", "data", "minnesota_county_ElecEmissions_2021.RDS"))

# compare numbers we obtained to downscaled EIA numbers
# read in EIA state estimate (mWh) for MN -- https://www.eia.gov/electricity/state/archive/2021/minnesota/

EIA_MN_elecRetailEst_mWh <- 66589168


MN_currentCounty_deliveries <- read_rds(here(
  "_energy",
  "data",
  "Minnesota_county_ElecEmissions.RDS"
)) %>%
  select(county, OURS_total_CO2e_emissions_lbs = total_CO2e_emissions_lbs)

downscaleEIA_MN_electricRetail <- read_rds(here(
  "_meta",
  "data",
  "cprg_county_proportions.RDS"
)) %>%
  filter(state_name == "Minnesota" &
    population_data_source == "Decennial Census PL 94-171 Redistricting Data Summary File") %>%
  select(geoid, county_name, county_proportion_of_state_pop) %>%
  mutate(
    downscaled_EIA_total_CO2e_emissions_lbs =
      EIA_MN_elecRetailEst_mWh * county_proportion_of_state_pop * 1003.1,
    state = "MN"
  ) %>%
  left_join(MN_currentCounty_deliveries,
    by = join_by("county_name" == "county")
  )

write_rds(downscaleEIA_MN_electricRetail, here(
  "_energy",
  "data",
  "minnesota_QA_versusEIAStateProfile.RDS"
))
