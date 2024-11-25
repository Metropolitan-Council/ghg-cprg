# Process Xcel Community Reports data
source("R/_load_pkgs.R")
source("_energy/data-raw/_energy_emissions_factors.R")


#read in time series of eGRID emissions factor data and pivot wider to make one row per year with all three emission types
egridTimeSeries <- epa_ghg_factor_hub$egridTimeSeries %>%
  pivot_wider(names_from = emission, values_from = value)

# root directory with folders for each utility in scope (with each folder containing subfolders for all years which reporting to the state is available)
dir_xcel_communityReports <- here("_energy", "data-raw", "xcel_community_reports")


# Function to get file paths, utility names, and years of utility reports in root directory
get_files <- function(root_dir) {
  file_info <- list()
  
  # Loop through each utility folder
  utility_folders <- list.dirs(root_dir, recursive = FALSE)
  for (year in years) {
    year <- basename(year)
    
    # Loop through each year sub-folder within each utility folder
    year_folders <- list.dirs(utility_folder, recursive = FALSE)
    for (year_folder in year_folders) {
      year <- basename(year_folder)
      
      # Get list of Excel files in the year folder
      files <- list.files(path = year_folder, pattern = "\\.xlsx$", full.names = TRUE)
      
      # Append each file path with utility and year information
      for (file in files) {
        file_info <- append(file_info, list(list(file_path = file,
                                                 city_name = utility_name,
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
