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
  data_E_G <- read_excel(file_path, sheet = "GasByCounty", range = "E15:G56")

  # Rename columns to reflect the data that is actually read
  colnames(data_A_C) <- c("countyCode", "county", "mcf_delivered")
  colnames(data_E_G) <- c("countyCode", "county", "mcf_delivered")

  # Combine the data from both ranges
  combined_data <- rbind(data_A_C, data_E_G)

  # Filter for specific study area counties
  combined_data <- combined_data %>%
    filter(county %in% c(
      "Anoka", "Carver", "Dakota", "Hennepin", "Ramsey",
      "Scott", "Sherburne", "Chisago", "Washington"
    ))

  # Add utility name and year columns
  combined_data$utility <- utility_name
  combined_data$year <- as.numeric(year) # Ensure year is numeric if needed

  return(combined_data)
}

# Apply process_file to each file identified in get_files() in the nested structure and combine the results
file_list <- get_files(dir_mn_natGas_state)
combined_MNgasUtil_activityData <- do.call(rbind, lapply(file_list, process_file))

# Assuming each row in mn_electricity_data represents a utility's electricity delivery in a county,
# process and merge data -- this will be a separate data collection process spanning excel reports submitted to state
processed_mn_gasUtil_activityData <- combined_MNgasUtil_activityData %>%
  # remove utility-county records with no data
  filter(!is.na(mcf_delivered)) %>%
  mutate(
    CO2_emissions_mt = mcf_delivered * epa_emissionsHub_naturalGas_factor_lbsCO2_perMCF %>%
      units::as_units("pound") %>%
      units::set_units("metric_ton") %>%
      as.numeric(),
    CH4_emissions_mt = mcf_delivered * epa_emissionsHub_naturalGas_factor_lbsCH4_perMCF * GWP_CH4 %>%
      units::as_units("pound") %>%
      units::set_units("metric_ton") %>%
      as.numeric(),
    N2O_emissions_mt = mcf_delivered * epa_emissionsHub_naturalGas_factor_lbsN2O_perMCF * GWP_N2O %>%
      units::as_units("pound") %>%
      units::set_units("metric_ton") %>%
      as.numeric(),
    CO2e_emissions_mt = CO2_emissions + CH4_emissions + N2O_emissions
  )

# Aggregate data by county, add identifiers for state and sector
MNcounty_level_gas_emissions <- processed_mn_gasUtil_activityData %>%
  group_by(county, year) %>%
  summarise(
    total_mcf = sum(mcf_delivered, na.rm = TRUE),
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
    sector = "Natural gas"
  ) %>%
  rename(
    county_name = county
  )


# incorporates totals numbers for Residential, Commercial, Industrial, internal use, Unaccounted For gas, and Deliveries to Transportation (i.e. deregulated competitive providers)
# Source: 2005 MN Utility Data Book

MN_state2005_natGasMCFTotal <- 367825000 # EIA State Energy Profile; 328955000 reported in Utility Data Book
MN_state2021_natGasMCFTotal <- 495126000 # EIA State Energy Profile

# create downscaled county-level data for 2005 and 2021 -- we have utility reports for 2021, so 2021 data is QA only; 2005 data is to be used as actuals
downscaleMN_gas_basedOnPopProps <- read_rds(here(
  "_meta",
  "data",
  "cprg_county_proportions.RDS"
)) %>%
  filter(state_name == "Minnesota" &
    population_year %in% c(2005, 2021)) %>%
  select(year = population_year, county_name, county_proportion_of_state_pop) %>%
  mutate(
    total_mcf = case_when(
      year == 2005 ~ MN_state2005_natGasMCFTotal * county_proportion_of_state_pop,
      year == 2021 ~ MN_state2021_natGasMCFTotal * county_proportion_of_state_pop
    )
  ) %>%
  mutate(
    total_CO2_emissions_lbs = total_mcf * epa_emissionsHub_naturalGas_factor_lbsCO2_perMCF,
    total_CH4_emissions_lbs = total_mcf * epa_emissionsHub_naturalGas_factor_lbsCH4_perMCF * GWP_CH4,
    total_N2O_emissions_lbs = total_mcf * epa_emissionsHub_naturalGas_factor_lbsN2O_perMCF * GWP_N2O,
  ) %>%
  mutate(
    total_CO2e_emissions_lbs = total_CO2_emissions_lbs + total_CH4_emissions_lbs + total_N2O_emissions_lbs,
    total_CO2_emissions_tons = total_CO2_emissions_lbs / 2000,
    total_CH4_emissions_tons = total_CH4_emissions_lbs / 2000,
    total_N2O_emissions_tons = total_N2O_emissions_lbs / 2000
  ) %>%
  mutate(
    total_CO2e_emissions_tons = total_CO2e_emissions_lbs / 2000,
    emissions_metric_tons_co2e = total_CO2e_emissions_lbs %>%
      units::as_units("pound") %>%
      units::set_units("metric_ton") %>%
      as.numeric()
  ) %>%
  mutate(
    state = "MN",
    sector = "Natural gas"
  ) %>%
  select(-county_proportion_of_state_pop)

downscaleMN_gas_basedOnPopProps_2005 <- downscaleMN_gas_basedOnPopProps %>%
  filter(year == 2005)

downscaleMN_gas_basedOnPopProps_2021 <- downscaleMN_gas_basedOnPopProps %>%
  filter(year == 2021)

cprg_county_pops <- read_rds(here(
  "_meta",
  "data",
  "cprg_county_proportions.RDS"
)) %>%
  select(county_name, year = population_year, county_population) %>%
  filter(year %in% c(2005, 2021))

MNcounty_level_gas_emissions2005_2013to2023 <- rbind(downscaleMN_gas_basedOnPopProps_2005, MNcounty_level_gas_emissions) %>%
  left_join(cprg_county_pops,
    by = join_by(county_name, year)
  ) %>%
  mutate(
    CO2eEmissions_PerCap_Tons = emissions_metric_tons_co2e / county_population
  ) %>%
  left_join(downscaleMN_gas_basedOnPopProps_2021 %>% select(county_name, year, EST_emissions_metric_tons_co2e = emissions_metric_tons_co2e),
    by = join_by(county_name, year)
  ) %>%
  mutate(
    EST_perCap_CO2e = EST_emissions_metric_tons_co2e / county_population
  )

MNcounty_level_gas_emissions <- MNcounty_level_gas_emissions2005_2013to2023 %>%
  select(-county_population, -CO2eEmissions_PerCap_Tons, -EST_emissions_metric_tons_co2e, -EST_perCap_CO2e) %>%
  mutate(
    year = as.numeric(year)
  )

# since we have more detailed info for 2021, save off as a separate .RDS for safekeeping
write_rds(processed_mn_gasUtil_activityData, here("_energy", "data", "minnesota_gasUtils_ActivityAndEmissions.RDS"))

# write combined 2005 (downscaled) and 2021 (derived from utility reports) activity/emissions data to .RDS
write_rds(MNcounty_level_gas_emissions, here("_energy", "data", "minnesota_county_GasEmissions.RDS"))
