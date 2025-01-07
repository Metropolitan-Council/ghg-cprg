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
  stop_row <- start_row + stop_row_offset - 1 # Adjust for Excel indexing and remove the total row
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


#join with CTU-county reference
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_ctu <- readRDS("_meta/data/cprg_ctu.RDS")
ctu_population <- readRDS("_meta/data/ctu_population.RDS") %>%
  filter(inventory_year > 2014) %>%
  left_join(cprg_county %>% select(geoid, county_name, state_abb), by = 'geoid') %>%
  filter(state_abb == 'MN') %>%
  rename(year = inventory_year) 



if (file.exists("_energy/data/Xcel_activityData_2015_2023.RDS") == FALSE) {

# Apply process_file to each file identified in get_files() in the nested structure and combine the results
file_list <- get_files(dir_xcel_communityReports)
  

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


# COCTU population proportion reference to parcel out Xcel CTU-level reporting to COCTUs based on population.
# Assume sector breakouts are the same in all COCTU units tied to CTU reporting.

# Calculate unique total population by city-year-county
city_total_population <- ctu_population %>%
  distinct(ctu_name, ctu_class, year, county_name, ctu_population) %>% # Ensure unique rows per city-county-year
  group_by(ctu_name, ctu_class, year) %>% 
  mutate(
    total_ctu_population = sum(ctu_population, na.rm = TRUE), # Sum populations across counties for each city-year
    multi_county = n_distinct(county_name) > 1 
  ) %>%
  ungroup()


Xcel_activityData_2015_2023 <- rbind(combined_Xcel_activityData_2015_2019,
                                     combined_Xcel_activityData_2020_2023) %>%
  # Map Xcel-provided sectors to simplified CPRG sectors and "Business" for subsequent disaggregation
  mutate(
    sector_mapped = case_when(
      sector %in% c("Residential") ~ "residential",
      sector %in% c("Street Lighting - Non-Metered/Customer Owned",
                    "Street Lighting - Non-Metered/Xcel-Owned",
                    "Street Lighting - Metered") ~ "commercial", # Map street lighting to Commercial
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
      city_name == "Village of Birchwood" ~ "CITY",
      TRUE ~ NA_character_
    ),
    ctu_name = case_when(
      grepl("^City of", city_name, ignore.case = TRUE) ~ sub("^City of\\s+", "", city_name),
      grepl("^Town of", city_name, ignore.case = TRUE) ~ sub("^Town of\\s+", "", city_name),
      city_name == "Village of Birchwood" ~ "Birchwood Village",
      TRUE ~ city_name
    )
  ) %>%
  # Remove records with no or unusable data
  filter(!is.na(sector_mapped)) %>%
  # Join city_total_population back to main dataset
  left_join(city_total_population, 
            by = c("ctu_name", "ctu_class", "year"),
            relationship = 'many-to-many') %>%
  # Calculate proportions and disaggregated values
  group_by(ctu_name, ctu_class, year, county_name) %>%
  mutate(
    ctu_population_proportion = ctu_population / total_ctu_population, # Calculate proportions
    disagg_util_reported_co2e = ifelse(
      multi_county,
      util_reported_co2e * ctu_population_proportion,
      NA
    ),
    disagg_mWh_delivered = ifelse(
      multi_county,
      mWh_delivered * ctu_population_proportion,
      NA
    )
  ) %>%
  ungroup() %>%
  
  # Filter to core metro counties while keeping `county_name` intact
  filter(county_name %in% c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington"))

write_rds(Xcel_activityData_2015_2023, "_energy/data/Xcel_activityData_2015_2023.RDS")

}

Xcel_activityData_2015_2023 <- readRDS("_energy/data/Xcel_activityData_2015_2023.RDS")

#preprocess the NREL proportions dataset -- make duplicates of 2017 records for 2015 and 2016 to enable join to Xcel data
nrel_slope_city_emission_proportions_adjusted <-  readRDS("_energy/data-raw/nrel_slope/nrel_slope_city_emission_proportions.RDS") %>%
  filter(source == 'Electricity') %>%
  mutate(nrel_year = year) %>%
  # Duplicate rows for 2015 and 2016, mapping them to 2017
  bind_rows(
    filter(., year == 2017) %>% mutate(year = 2015),
    filter(., year == 2017) %>% mutate(year = 2016)
  )

xcel_activityData_NREL_QA_2015_2022 <- Xcel_activityData_2015_2023 %>%
  left_join(
    nrel_slope_city_emission_proportions_adjusted,
    # implicitly applies same proportions to COCTU splits
    by = join_by(
      ctu_name == city_name,
      ctu_class == ctu_class,
      year == year
    )
  ) %>%
  # temporarily exclude 2023 since no eGRID data yet
  filter(year < 2023) %>%
  select(-source.x,
         -source.y,
         -kwh_delivered,
         -city_name)

xcel_activityData_NREL_2015_2022 <-  bind_rows(
  # Keep original rows that are not 'Business'
  xcel_activityData_NREL_QA_2015_2022 %>%
    filter(sector_mapped != "Business"),
  
  # Expand `Business` rows into 'commercial*'
  xcel_activityData_NREL_QA_2015_2022 %>%
    filter(sector_mapped == "Business") %>%
    mutate(sector_mapped = "commercial*"),
  
  # Expand `Business` rows into 'industrial*'
  xcel_activityData_NREL_QA_2015_2022 %>%
    filter(sector_mapped == "Business") %>%
    mutate(sector_mapped = "industrial*")
) %>%
  # Step 2: Mutate disaggregated or original values based on the value of `sector_mapped`
  mutate(
    util_co2e = coalesce(disagg_util_reported_co2e, util_reported_co2e),
    util_mWh = coalesce(disagg_mWh_delivered, mWh_delivered),
    
    # Adjust values for 'commercial*' and 'industrial*' rows
    # breaks out the Business records into two pieces based oon the proportional breakdown of NREL-modeled commercial/industrial
    util_co2e = case_when(
      sector_mapped == "commercial*" ~ util_co2e * coalesce((commercial_city / (commercial_city + industrial_city)),
                                                            (commercial_downscale / (commercial_downscale + industrial_downscale))
                                                            ),
      sector_mapped == "industrial*" ~ util_co2e * coalesce((industrial_city / (commercial_city + industrial_city)),
                                                            (industrial_downscale / (commercial_downscale + industrial_downscale))
      ),
      TRUE ~ util_co2e
    ),
    util_mWh = case_when(
      sector_mapped == "commercial*" ~ util_mWh * coalesce((commercial_city / (commercial_city + industrial_city)),
                                                            (commercial_downscale / (commercial_downscale + industrial_downscale))
      ),
      sector_mapped == "industrial*" ~ util_mWh * coalesce((industrial_city / (commercial_city + industrial_city)),
                                                            (industrial_downscale / (commercial_downscale + industrial_downscale))
      ),
      TRUE ~ util_mWh
    ),
    
    # Create `nrel_source` column to identify which NREL data set was used to disaggregate BUSINESS records (if such disagg was done)
    nrel_source = case_when(
      sector_mapped == "commercial*" & !is.na(commercial_city) ~ "CITY",
      sector_mapped == "industrial*" & !is.na(industrial_city) ~ "CITY",
      sector_mapped == "commercial*" & !is.na(commercial_downscale) ~ "COUNTY",
      sector_mapped == "industrial*" & !is.na(industrial_downscale) ~ "COUNTY",
      TRUE ~ NA_character_
    )
  )

write_rds(xcel_activityData_NREL_2015_2022, "_energy/data/xcel_activityData_NREL_2015_2022_process.RDS")

# enable comparison of NREL breakdowns to actual breakdowns from Xcel

#identify cities with a mix of business and commercial/industrial?

# Step 1: Calculate city-level average proportions for Commercial and Industrial, with a default value of 1 for commercial to ensure street lighting is fully allocated there if no commercial or industrial data




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

