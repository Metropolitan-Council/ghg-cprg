# Process all Minnesota electric utility activity data
source("R/_load_pkgs.R")
source("_energy/data-raw/_energy_emissions_factors.R")

# Directory containing Excel files with utility annual reports --
# all reports were manually downloaded from
# https://mn.gov/commerce/energy/industry-government/utilities/annual-reporting.jsp
# based on the contents of MNutilities_in_scope$utility_name

# read in time series of eGRID emissions factor data and pivot wider to make one row per year with all three emission types
egridTimeSeries <- epa_ghg_factor_hub$egridTimeSeries %>%
  pivot_wider(names_from = emission, values_from = value)

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

# function to process the file associated with each utility-year combo and extract activity (mWh) at the utility-year-county granularity electricity data
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
  combined_data$year <- as.numeric(year) # Ensure year is numeric if needed

  return(combined_data)
}

# function designed to extract "electricity by class" (read: sector) for municipal utilities to derive additional city-sector level observations for modeling
process_municipal_elecByClass <- function(file_info) {
  # Extract file path, utility name, and year from file_info (output nested list structure from get_files)
  file_path <- file_info$file_path
  utility_name <- file_info$utility_name
  year <- file_info$year

  # Read specific ranges from each file
  data_A_C <- read_excel(file_path, sheet = "ElectricityByClass", range = "A10:C16")

  # Rename columns
  colnames(data_A_C) <- c("sector", "customer_count", "mwh_delivered")

  # Add utility name and year columns
  data_A_C$utility <- utility_name
  data_A_C$year <- as.numeric(year) # Ensure year is numeric if needed

  return(data_A_C)
}


# Apply process_file to each file identified in get_files() in the nested structure and combine the results
file_list <- get_files(dir_mn_electricity_state)
combined_MNelectUtil_activityData <- do.call(rbind, lapply(file_list, process_file))

#### GRE and Connexus split up in 2022, but everything prior to then in Anoka County is a doublecount
#### checking and then zeroing out GRE in Anoka
combined_MNelectUtil_activityData %>%
  filter(
    county == "Anoka",
    utility %in% c(
      "Connexus Energy",
      "Great River Energy"
    )
  ) %>%
  distinct(utility, year, mWh_delivered) %>%
  arrange(desc(year)) %>%
  print(n = 50)
# GRE is variable in reporting year to year, Connexus is fairly consistent

combined_MNelectUtil_activityData <- combined_MNelectUtil_activityData %>%
  filter(!(utility == "Great River Energy" & county == "Anoka"))

# identify subset of files attached to municipal utilities, then create a table with elec by class for each utility-year and export it
MN_elecMunis <- readRDS(here("_energy", "data", "distinct_electricity_util_type_MN.RDS")) %>%
  filter(utility_type == "Municipal")
muni_files <- keep(file_list, ~ .x$utility_name %in% unique(MN_elecMunis$mpuc_name))
combined_MNelecMunis_elecByClass <- do.call(rbind, lapply(muni_files, process_municipal_elecByClass))

# raw 7610 data for elec is processed and enriched with city level geo data in muniElectrics_7610_elecByClass_2013_2023.R
write_rds(combined_MNelecMunis_elecByClass, here("_energy", "data", "combined_MNelecMunis_elecByClass_raw7610.RDS"))

# manual data collection to fill in gaps for 2021 as needed -- need to check if 2022 is available

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

scottProp <- 45972 / 65674 # proportion of utility operation in Scott County
scottNewPragueMuni_mWh_2021 <- scottProp * 72086.211
scottNewPragueMuni_mWh_2020 <- scottProp * 67435.726
combined_MNelectUtil_activityData <- combined_MNelectUtil_activityData %>%
  add_row(
    countyCode = 70,
    county = "Scott",
    mWh_delivered = scottNewPragueMuni_mWh_2021,
    utility = "New Prague Utilities Commission",
    year = 2021
  ) %>%
  add_row(
    countyCode = 70,
    county = "Scott",
    mWh_delivered = scottNewPragueMuni_mWh_2020,
    utility = "New Prague Utilities Commission",
    year = 2020
  )


## interpolate missing county-utility combinations
full_grid <- expand.grid(
  year = 2013:2023,
  county = unique(combined_MNelectUtil_activityData$county),
  utility = unique(combined_MNelectUtil_activityData$utility)
) %>%
  inner_join(combined_MNelectUtil_activityData %>% distinct(county, utility), by = c("county", "utility"))

# Merge with original data
interpolated_mn_utility_activity <- full_grid %>%
  left_join(combined_MNelectUtil_activityData, by = c("year", "county", "utility")) %>%
  arrange(county, utility, year) %>%
  group_by(county, utility) %>%
  mutate(
    data_source = if_else(is.na(mWh_delivered), "Interpolated", "Utility report"),
    mWh_delivered = na_kalman(mWh_delivered)
  ) %>%
  ungroup()


processed_mn_elecUtil_activityData <- interpolated_mn_utility_activity %>%
  left_join(egridTimeSeries,
    by = join_by(year == Year)
  ) %>%
  mutate(
    CO2_emissions_mt = mWh_delivered * `lb CO2` %>%
      units::as_units("pound") %>%
      units::set_units("metric_ton") %>%
      as.numeric(),
    CH4_emissions_mt = mWh_delivered * `lb CH4` %>%
      units::as_units("pound") %>%
      units::set_units("metric_ton") %>%
      as.numeric(),
    N2O_emissions_mt = mWh_delivered * `lb N2O` %>%
      units::as_units("pound") %>%
      units::set_units("metric_ton") %>%
      as.numeric()
  ) %>%
  # get rid of unnecessary columns from eGRID factor tables
  select(-eGrid_Subregion,
    -factor_type,
    -per_unit,
    factor_source = Source,
    -`lb CO2`,
    -`lb CH4`,
    -`lb N2O`
  )

MNcounty_level_electricity_emissions <- processed_mn_elecUtil_activityData %>%
  group_by(year, county) %>%
  summarise(
    total_mWh_delivered = sum(mWh_delivered, na.rm = TRUE),
    total_CO2_emissions_mt = sum(CO2_emissions_mt, na.rm = TRUE),
    total_CH4_emissions_mt = sum(CH4_emissions_mt, na.rm = TRUE),
    total_N2O_emissions_mt = sum(N2O_emissions_mt, na.rm = TRUE),
    emissions_metric_tons_co2e = sum(
      CO2_emissions_mt +
        (CH4_emissions_mt * gwp$ch4) +
        (N2O_emissions_mt * gwp$n2o),
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  mutate(
    state = "MN",
    sector = "Electricity"
  )


# parcel out municipal utility reporting to be leveraged as QA against the sector-level numbers pulled separately
muni_activity_separated <- processed_mn_elecUtil_activityData %>%
  right_join(MN_elecMunis,
    by = join_by(utility == mpuc_name)
  ) %>%
  # remove out of scope utilities
  filter(!utility %in% c("Delano Municipal Utilities,", "Elk River Municipal Utilities")) %>%
  select(1:3, 5) %>%
  mutate(
    sector = "All",
    source = "Electricity"
  )


write_rds(processed_mn_elecUtil_activityData, here("_energy", "data", "minnesota_elecUtils_ActivityAndEmissions.RDS"))
write_rds(MNcounty_level_electricity_emissions, here("_energy", "data", "minnesota_county_elec_ActivityAndEmissions.RDS"))
write_rds(muni_activity_separated, here("_energy", "data-raw", "minnesota_municipalElectric_activity_2013_2023_QA.RDS"))


# OUT OF DATE -- written for just 2021
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
