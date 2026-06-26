source("R/_load_pkgs.R")
source("_energy/data-raw/_energy_emissions_factors.R")

# root directory with folders for each utility in scope (with each folder containing subfolders for all years which reporting to the state is available)
dir_mn_natGas_state <- here("_energy", "data-raw", "mn_ng_utility_reporting_state")

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

      # Get list of Excel files in the year folder -- captures both .xls and .xlsx
      files <- list.files(path = year_folder, pattern = "\\.xls(x)?$", full.names = TRUE)

      # Append each file path with utility and year information
      for (file in files) {
        file_info <- append(file_info, list(list(
          file_path = file,
          utility_name = utility_name,
          year = year
        )))
      }
    }
  }
  return(file_info)
}

# Function to process each file and extract county-level utility activity data
process_file <- function(file_info) {
  # Extract file path, utility name, and year from file_info (output nested list structure from get_files)
  file_path <- file_info$file_path
  utility_name <- file_info$utility_name
  year <- file_info$year

  # Read specific ranges from the file
  data_A_C <- read_excel(file_path, sheet = "GasByCounty", range = "A15:C59")
  data_E_G <- read_excel(file_path, sheet = "GasByCounty", range = "E15:G60")

  # Rename columns to reflect the data that is actually read
  colnames(data_A_C) <- c("countyCode", "county", "mcf_delivered")
  colnames(data_E_G) <- c("countyCode", "county", "mcf_delivered")

  # Combine the data from both ranges
  combined_data <- rbind(data_A_C, data_E_G)

  # Filter for specific study area counties
  combined_data <- combined_data %>%
    filter(county %in% c(
      "Anoka", "Carver", "Dakota", "Hennepin", "Ramsey",
      "Scott", "Sherburne", "Chisago", "Washington",
      "GRAND TOTAL (Calculated)"
    ))

  # Add utility name and year columns
  combined_data$utility <- utility_name
  combined_data$year <- as.numeric(year) # Ensure year is numeric if needed

  return(combined_data)
}

# Apply process_file to each file identified in get_files() in the nested structure and combine the results
file_list <- get_files(dir_mn_natGas_state)
combined_MNgasUtil_activityData <- do.call(rbind, lapply(file_list, process_file))


# CALCULATE COUNTY PROPORTIONS OF EACH UTILITY'S TOTAL BY YEAR

utility_county_proportions <- combined_MNgasUtil_activityData %>%
  filter(!is.na(mcf_delivered)) %>%
  # Separate grand totals from county rows
  mutate(is_total = county == "GRAND TOTAL (Calculated)") %>%
  # Get each utility's grand total per year
  group_by(utility, year) %>%
  mutate(
    utility_grand_total = mcf_delivered[is_total]
  ) %>%
  ungroup() %>%
  # Keep only county rows (not the total row)
  filter(!is_total) %>%
  # Calculate county share of each utility's total
  mutate(
    county_proportion = mcf_delivered / utility_grand_total
  ) %>%
  select(utility, year, county, mcf_delivered, utility_grand_total, county_proportion)

# QA: check proportions sum to something reasonable per utility-year
# (won't sum to 1.0 because we only have study area counties, not all MN counties)
utility_county_proportions %>%
  group_by(utility, year) %>%
  summarise(
    sum_proportion = sum(county_proportion, na.rm = TRUE),
    n_counties = n(),
    .groups = "drop"
  ) %>%
  print(n = Inf)

# Save the county proportions object for use in handbook back-estimation
write_rds(utility_county_proportions, here("_energy", "data", "county_natgas_7610_activity.RDS"))
