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
testthat::test_that("Correct number of distinct city-year combos", {
  
  combinations <- lapply(file_list, function(x) {
    c(city_name = x$city_name, year = x$year)
  })
  
  # Convert to a data frame for easier processing
  combinations_df <- do.call(rbind, combinations)
  
  # Count unique combinations
  unique_combinations <- nrow(unique(combinations_df))
  
  
  testthat::expect_equal(
    length(file_list),
    unique_combinations
  )
})

# function to dynamically read input from files until a stopping value is found
read_until_value <- function(file_path, sheet, start_cell, stop_value, columns) {
  print(paste("Reading file:", file_path))
  
  # Step 1: Read a broad range starting from the specified cell
  start_row <- as.numeric(gsub("[A-Z]", "", start_cell)) # Extract the row number from start_cell
  start_col <- gsub("[0-9]", "", start_cell)            # Extract the column letter from start_cell
  broad_range <- paste0(start_col, start_row, ":", columns, start_row + 10) # Read 10 rows initially
  print(paste("Broad range:", broad_range))
  
  # Read the broad range
  data_broad <- read_excel(file_path, sheet = sheet, range = broad_range, col_names = FALSE)
  print(data_broad)
  
  # Step 2: Locate the stopping value
  stop_row_offset <- which(data_broad[[1]] == stop_value)[1] # Check the first column for stop_value
  print(paste("Stop row offset:", stop_row_offset))
  
  if (length(stop_row_offset) == 0) {
    stop("Stopping value not found in the specified range.")
  }
  
  # Step 3: Define the dynamic range
  stop_row <- start_row + stop_row_offset - 2 # Adjust for Excel indexing and remove the total row
  refined_range <- paste0(start_col, start_row, ":", columns, stop_row)
  print(refined_range)
  
  # Step 4: Read the refined range
  data <- read_excel(file_path, sheet = sheet, range = refined_range, col_names = TRUE)
  print(data)
  return(data)
}

# function to process the file associated with each utility-year combo and extract activity (mWh) at the utility-year-county granularity electricity data
# years 2015 to 2019 have constant format -- 2020 adds more info about renewables and clean energy
process_file <- function(file_info, start_cell) {
  # Extract file path, utility name, and year from file_info (output nested list structure from get_files)
  file_path <- file_info$file_path
  city_name <- file_info$city_name
  utility <- file_info$utility
  year <- file_info$year
  
  # Read in provided electricity sector data from each file and add file reference info
  city_data <- read_until_value(
    file_path = file_path,
    sheet = "Standard Community Report",
    start_cell = start_cell,
    stop_value = "Total:", 
    columns = "H"       # Read columns A to G
  ) %>%
  # clean up raw data -- rename columns as needed and drop unnecessary columns
  select(
    sector = Electricity,
    kwh_delivered = `Energy Consumption (kWh)`,
    util_reported_co2e = `Carbon Emissions (metric tons CO2) [6]`
  ) %>%
  mutate(
    mWh_delivered = kwh_delivered / 1000,
    city_name = city_name,
    utility_name = utility,
    year = year,
    source = 'Electricity'
  )
  
  return(city_data)
}


# Apply process_file_2015_2019 to each file for years 2015-19 identified in get_files() in the nested structure and combine the results
file_list_2015_2019 <- Filter(function(x) x$year < 2020, file_list)
file_list_2020_2023 <- Filter(function(x) x$year > 2019, file_list)

combined_Xcel_activityData_2015_2019 <- do.call(
  rbind, 
  lapply(file_list_2015_2019, function(file_info) {
    process_file(file_info, start_cell = "A20")
  })
)

combined_Xcel_activityData_2020_2023 <- do.call(
  rbind, 
  lapply(file_list_2020_2023, function(file_info) {
    process_file(file_info, start_cell = "A39")
  })
)


#row bind two sets


#address incongruent sectors


#join with CTU-county reference


