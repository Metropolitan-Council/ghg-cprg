# Process Xcel Community Reports data
source("R/_load_pkgs.R")
source("_energy/data-raw/_energy_emissions_factors.R")


# root directory with folders for each utility in scope (with each folder containing subfolders for all years which reporting to the state is available)
dir_xcel_communityReports <- here("_energy", "data-raw", "xcel_community_reports")

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

      # Handle cases where city_name or year is missing
      if (is.na(city_name) || is.na(year)) {
        warning(paste("Missing city name or year in file:", file))
      }

      # Append the file information to the list
      file_info <- append(file_info, list(list(
        file_path = file,
        city_name = unname(city_name),
        year = as.numeric(year),
        utility = "Xcel Energy"
      )))
    }
  }
  return(file_info)
}

# Isolate the community referenced in each file
extract_city_name <- function(file_path) {
  # Read the first 3 rows of the Excel file
  data <- read_excel(file_path, range = cell_rows(1:3), col_names = FALSE)
  
  # Convert the data to a character matrix for easier searching
  data_char <- as.matrix(data)
  
  # Process row 2 (Excel row 3)
  row2 <- data_char[2, ] 
  
  # Loop through each cell in row 2 to find 'Community'
  for (i in seq_along(row2)) {
    cell_content <- row2[i]
    if (!is.na(cell_content) && grepl("Community", cell_content, ignore.case = TRUE)) {
      # Case 1: City name is in the same cell
      if (grepl("Community[:]?\\s*.+", cell_content, ignore.case = TRUE)) {
        city_name <- sub(".*Community[:]?\\s*", "", cell_content, ignore.case = TRUE)
        city_name <- trimws(city_name)
        if (city_name != "") {
          return(city_name)
        }
      }
      
      # Case 2: City name is in the next cell(s)
      for (j in (i + 1):length(row2)) {
        next_cell <- row2[j]
        if (!is.na(next_cell) && next_cell != "") {
          city_name <- trimws(next_cell)
          if (city_name != "") {
            return(city_name)
          }
        }
      }
    }
  }
  
  # Return NA if 'Community' not found or city name is empty
  return(NA)
}


# Find the row number of a given pattern (i.e. 'Electricity' or 'Natural Gas') in a limited Excel range to determine where to start data reads
find_row_of_text <- function(file_path, sheet, pattern, search_range = "A1:H60") {
  # Read some portion of the sheet to find 'Electricity' or 'Natural Gas'
  df <- read_excel(
    file_path,
    sheet = sheet,
    range = search_range,
    col_names = FALSE
  )
  mat <- as.matrix(df)
  
  match_idx <- which(grepl(pattern, mat, ignore.case = TRUE))
  if (length(match_idx) == 0) {
    return(NA)
  } else {
    # Convert matrix index to row index (1-based)
    row_idx <- (match_idx - 1) %% nrow(mat) + 1
    return(row_idx[1])  # If multiple matches, take the first
  }
}


# Dynamically read input from file until a stopping value (e.g., "Total:") is found
read_until_value <- function(file_path, sheet, start_cell, stop_value, columns) {
  print(paste("Reading file:", file_path))

  # Read a broad range starting from the specified cell
  start_row <- as.numeric(gsub("[A-Z]", "", start_cell)) # Extract the row number from start_cell
  start_col <- gsub("[0-9]", "", start_cell) # Extract the column letter from start_cell
  broad_range <- paste0(start_col, start_row, ":", columns, start_row + 10) # Read 10 rows initially
  print(paste("Broad range:", broad_range))

  # Read the broad range
  data_broad <- read_excel(file_path, sheet = sheet, range = broad_range, col_names = FALSE)
  print(data_broad)

  # Locate the stopping value
  stop_row_offset <- which(data_broad[[1]] == stop_value)[1] # Check the first column for stop_value
  print(paste("Stop row offset:", stop_row_offset))

  if (length(stop_row_offset) == 0) {
    stop("Stopping value not found in the specified range.")
  }

  # Define the dynamic range
  stop_row <- start_row + stop_row_offset - 1 # Adjust for Excel indexing and remove the total row
  refined_range <- paste0(start_col, start_row, ":", columns, stop_row)
  print(refined_range)

  # Read the refined range
  data <- read_excel(file_path, sheet = sheet, range = refined_range, col_names = TRUE)
  print(data)
  return(data)
}

# Fetch a single section (Electricity or Natural Gas) using other helper fucntions in ensemble
fetch_section <- function(file_path, sheet, pattern, stop_value, columns = "H") {
  start_row <- find_row_of_text(file_path, sheet, pattern)
  
  # If pattern not found, return NULL
  if (is.na(start_row)) {
    message("'", pattern, "' not found in this file. Skipping.")
    return(NULL)
  }
  
  # Build start_cell like "A39" if row is 39
  start_cell <- paste0("A", start_row)
  
  # Use read_until_value() to capture the block of interest
  df <- read_until_value(
    file_path   = file_path,
    sheet       = sheet,
    start_cell  = start_cell,
    stop_value  = stop_value,
    columns     = columns
  )
  
  return(df)
}

# Process the file associated with each utility-year combo and extract activity (mwh/therms) at the utility-year granularity
process_file <- function(file_info) {
  # Unpack city-year metadata
  file_path <- file_info$file_path
  city_name <- file_info$city_name
  utility   <- file_info$utility
  year      <- file_info$year
  
  # Electricity
  electricity_raw <- fetch_section(
    file_path = file_path,
    sheet     = "Standard Community Report",
    pattern   = "Electricity",
    stop_value= "Total:"
  )
  
  # if data is retrieved, clean it up
  if (!is.null(electricity_raw)) {
    electricity_clean <- electricity_raw %>%
      select(
        sector = Electricity,
        kwh_delivered = `Energy Consumption (kWh)`,
        util_reported_co2e = `Carbon Emissions (metric tons CO2) [6]`
      ) %>%
      mutate(
        mwh_delivered = kwh_delivered / 1000,
        city_name = city_name,
        utility = utility,
        year = year,
        source = "Electricity"
      )
  } else {
    electricity_clean <- NULL
  }
  
  # Natural Gas
  gas_raw <- fetch_section(
    file_path = file_path,
    sheet     = "Standard Community Report",
    pattern   = "Natural Gas",
    stop_value= "Total:"
  )
  
  # if data is retrieved, clean it up
  if (!is.null(gas_raw)) {
    gas_clean <- gas_raw %>%
      select(
        sector = `Natural Gas`,
        therms_consumed = `Energy Consumption (therms)`,
        util_reported_co2e = `Carbon Emissions (metric tons CO2) [9]`
      ) %>%
      mutate(
        city_name = city_name,
        utility = utility,
        year = year,
        source = "Natural Gas"
      )
  } else {
    gas_clean <- NULL
  }
  
  # Combine cleaned electricity and natural gas data from a given utility-city into one df and return it
  combined <- dplyr::bind_rows(electricity_clean, gas_clean)
  return(combined)
}


# prepare for CTU-county reference joins to enable disaggregation 
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_ctu <- readRDS("_meta/data/cprg_ctu.RDS")
ctu_population <- readRDS("_meta/data/ctu_population.RDS") %>%
  filter(inventory_year > 2014) %>%
  left_join(cprg_county %>% select(geoid, county_name, state_abb), by = "geoid") %>%
  filter(state_abb == "MN") %>%
  rename(year = inventory_year)

# Calculate unique total population by city-year-county (multi-county case)
city_total_population <- ctu_population %>%
  distinct(ctu_name, ctu_class, year, county_name, ctu_population) %>% # Ensure unique rows per city-county-year
  group_by(ctu_name, ctu_class, year) %>%
  mutate(
    total_ctu_population = sum(ctu_population, na.rm = TRUE), # Sum populations across counties for each city-year
    multi_county = n_distinct(county_name) > 1
  ) %>%
  ungroup()

# takes about an hour to process all of nthe Xcel data -- only run if needed (i.e., the activity file doesn't exist)
if (file.exists("_energy/data/Xcel_elecNG_activityData_2015_2023.RDS") == FALSE) {
  
  file_list <- get_files(dir_xcel_communityReports)
  results_all <- purrr::map_dfr(file_list, process_file)
  
  therms_to_mcf <- 1 / 10.38
  
  Xcel_activityData_2015_2023 <- results_all %>%
    # Map Xcel-provided sectors to simplified CPRG sectors and "Business" for subsequent disaggregation
    mutate(
      sector_mapped = case_when(
        sector %in% c("Residential") ~ "residential",
        sector %in% c(
          "Street Lighting - Non-Metered/Customer Owned",
          "Street Lighting - Non-Metered/Xcel-Owned",
          "Street Lighting - Metered"
        ) ~ "commercial", # Map street lighting to Commercial
        sector == "Industrial" ~ "industrial",
        sector == "Commercial" ~ "commercial",
        sector == "Business *" ~ "Business",
        sector == "Business" ~ "Business", # Handle as disaggregation
        TRUE ~ NA_character_
      ),
      # Align ctu_name and ctu_class to CPRG format based on Xcel inputs
      ctu_class = case_when(
        grepl("^City of", city_name, ignore.case = TRUE) ~ "CITY",
        grepl("^Town of", city_name, ignore.case = TRUE) ~ "TOWNSHIP",
        grepl("(Twp)", city_name, ignore.case = TRUE) ~ "TOWNSHIP",
        city_name == "Village of Birchwood" ~ "CITY",
        TRUE ~ NA_character_
      ),
      ctu_name = case_when(
        grepl("^City of", city_name, ignore.case = TRUE) ~ sub("^City of\\s+", "", city_name),
        grepl("^Town of", city_name, ignore.case = TRUE) ~ sub("^Town of\\s+", "", city_name),
        city_name == "Village of Birchwood" ~ "Birchwood Village",
        TRUE ~ city_name
      ),
      mcf_delivered = therms_consumed * therms_to_mcf
    ) %>%
    # Remove records with no or unusable data
    filter(!is.na(sector_mapped)) %>%
    # Join city_total_population back to main dataset
    left_join(city_total_population,
              by = c("ctu_name", "ctu_class", "year"),
              relationship = "many-to-many"
    ) %>%
    # Calculate proportions and disaggregated values
    group_by(ctu_name, ctu_class, year, county_name) %>%
    mutate(
      ctu_population_proportion = ctu_population / total_ctu_population, # Calculate proportions
      disagg_util_reported_co2e = ifelse(
        multi_county,
        util_reported_co2e * ctu_population_proportion,
        NA
      ),
      disagg_mwh_delivered = ifelse(
        multi_county,
        mwh_delivered * ctu_population_proportion,
        NA
      ),
      disagg_mcf_delivered = ifelse(
        multi_county,
        mcf_delivered * ctu_population_proportion,
        NA
      )
    ) %>%
    ungroup() %>%
    # Filter to core metro counties while keeping `county_name` intact
    filter(county_name %in% c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington")) %>%
    # exclude cities statutorily not in METC area despite presence in core counties
    filter(!ctu_name %in% c("Northfield", "Hanover", "New Prague", "Cannon Falls", "Rockford")) %>%
    arrange(ctu_name, county_name, sector, year) %>%
    mutate(
      mwh_delivered = coalesce(disagg_mwh_delivered, mwh_delivered),
      mcf_delivered = coalesce(disagg_mcf_delivered, mcf_delivered),
      util_reported_co2e = coalesce(disagg_util_reported_co2e, util_reported_co2e)
    ) %>%
    select(1, 3:4, 6:8, 10:14) # exclude interstitial calculation columns
  
  write_rds(Xcel_activityData_2015_2023, "_energy/data/Xcel_elecNG_activityData_2015_2023.RDS")
}


# test that number of distinct city year combos = number of files
# highlight which files if any have NA value for city_name or year
# testthat::test_that("Correct number of distinct city-year combos and identify missing ones", {
#   # Extract combinations of city_name and year from file_list
#   combinations <- lapply(file_list, function(x) {
#     data.frame(city_name = x$city_name, year = x$year, stringsAsFactors = FALSE)
#   })
#   
#   # Convert to a single data frame
#   combinations_df <- do.call(rbind, combinations)
#   
#   # Count unique combinations
#   unique_combinations <- combinations_df %>%
#     distinct(city_name, year)
#   
#   # Identify duplicate or missing rows
#   duplicate_combinations <- combinations_df %>%
#     group_by(city_name, year) %>%
#     filter(n() > 1)
#   
#   # Compare length of file list to unique combinations and find missing
#   missing_combinations <- setdiff(
#     combinations_df %>% distinct(city_name, year),
#     unique_combinations
#   )
#   
#   # Print missing combinations for inspection
#   print("Missing combinations:")
#   print(missing_combinations)
#   
#   # Assert equality of unique combinations to file list length
#   testthat::expect_equal(
#     nrow(unique_combinations),
#     length(file_list) - 1,
#     info = paste("Missing or duplicate city-year combinations found. Check missing_combinations and duplicate_combinations.")
#   )
# })
