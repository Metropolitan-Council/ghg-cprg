# Process Xcel Community Reports data
source("R/_load_pkgs.R")
source("_energy/data-raw/_energy_emissions_factors.R")


#read in time series of eGRID emissions factor data and pivot wider to make one row per year with all three emission types
egridTimeSeries <- epa_ghg_factor_hub$egridTimeSeries %>%
  pivot_wider(names_from = emission, values_from = value)

# root directory with folders for each utility in scope (with each folder containing subfolders for all years which reporting to the state is available)
dir_xcel_communityReports <- here("_energy", "data-raw", "xcel_community_reports")


extract_city_name <- function(file_path) {
  # Read the first 3 rows of the Excel file
  data <- read_excel(file_path, range = cell_rows(1:3), col_names = FALSE)
  
  # Convert the data to a character matrix for easier searching
  data_char <- as.matrix(data)
  
  # Process row 2 (Excel row 3)
  row2 <- data_char[2, ] # Index 2 since R indexes rows starting at 1
  
  # Loop through each cell in row 2 to find 'Community'
  for (i in seq_along(row2)) {
    cell_content <- row2[i]
    if (!is.na(cell_content) && grepl('Community', cell_content, ignore.case = TRUE)) {
      # Case 1: City name is in the same cell
      if (grepl('Community[:]?\\s*.+', cell_content, ignore.case = TRUE)) {
        city_name <- sub('.*Community[:]?\\s*', '', cell_content, ignore.case = TRUE)
        city_name <- trimws(city_name)
        if (city_name != "") return(city_name)
      }
      
      # Case 2: City name is in the next cell(s)
      for (j in (i + 1):length(row2)) {
        next_cell <- row2[j]
        if (!is.na(next_cell) && next_cell != "") {
          city_name <- trimws(next_cell)
          if (city_name != "") return(city_name)
        }
      }
    }
  }
  
  # Return NA if 'Community' not found or city name is empty
  return(NA)
}

# Function to get file paths, city names, and years of Xcel utility reports in root directory
get_files <- function(root_dir) {
  file_info <- list()
  
  # Get list of year folders in root_dir (assuming folders are named as years)
  year_folders <- list.dirs(root_dir, recursive = FALSE)
  
  # Loop through each year folder
  for (year_folder in year_folders) {
    year <- basename(year_folder)
    
    # Get list of .xls files in the year folder
    files <- list.files(path = year_folder, pattern = "\\.xls$", full.names = TRUE)
    
    # Loop through each file in the year folder
    for (file in files) {
      # Extract city name from the file
      city_name <- extract_city_name(file)
      
      # Append the file information to the list
      file_info <- append(file_info, list(list(
        file_path = file,
        city_name = unname(city_name),
        year = as.numeric(year),
        utility = 'Xcel Energy'
      )))
    }
  }
  return(file_info)
}

# Apply process_file to each file identified in get_files() in the nested structure and combine the results
file_list <- get_files(dir_xcel_communityReports)


# test that number of distinct city year combos = number of files
# highlight which files if any have NA value for city_name or year 


# function to dynamically read in0ut from files until a stopping value is found
read_until_value <- function(file_path, sheet, start_cell, stop_value, columns) {
  # Step 1: Read a broad range starting from the specified cell
  start_row <- as.numeric(gsub("[A-Z]", "", start_cell)) # Extract the row number from start_cell
  start_col <- gsub("[0-9]", "", start_cell)            # Extract the column letter from start_cell
  broad_range <- paste0(start_col, start_row, ":", columns, start_row + 10) # Read 10 rows initially
  
  # Read the broad range
  data_broad <- read_excel(file_path, sheet = sheet, range = broad_range, col_names = FALSE)
  
  # Step 2: Locate the stopping value
  stop_row_offset <- which(data_broad[[1]] == stop_value) # Check the first column for stop_value
  
  if (length(stop_row_offset) == 0) {
    stop("Stopping value not found in the specified range.")
  }
  
  # Step 3: Define the dynamic range
  stop_row <- start_row + stop_row_offset - 1 # Adjust for Excel indexing
  refined_range <- paste0(start_col, start_row, ":", columns, stop_row)
  
  # Step 4: Read the refined range
  data <- read_excel(file_path, sheet = sheet, range = refined_range, col_names = TRUE)
  
  return(data)
}

# Example Usage:
sector_test <- read_until_value(
  file_path = "C:/Users/LimeriSA/Documents/Projects/ghg-cprg/_energy/data-raw/xcel_community_reports/2015/MN-City-Edina-2015.xls",
  sheet = "Standard Community Report",
  start_cell = "A20",
  stop_value = "Total:", # Replace this with the actual stopping value you're looking for
  columns = "H"       # Read columns A to D
)


combined_XcelCity_activityData <- do.call(rbind, lapply(file_list, process_file))


# function to process the file associated with each utility-year combo and extract activity (mWh) at the utility-year-county granularity electricity data
# years 2015 to 2019 have constant format -- 2020 adds more info about renewables and clean energy
process_file_2015_2019 <- function(file_info) {
  # Extract file path, utility name, and year from file_info (output nested list structure from get_files)
  file_path <- file_info$file_path
  city_name <- file_info$city_name
  utility <- file_info$utility
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
