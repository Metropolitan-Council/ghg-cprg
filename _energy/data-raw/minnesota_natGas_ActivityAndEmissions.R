source("R/_load_pkgs.R")
source("_energy/data-raw/_energy_emissions_factors.R")

# Directory where Excel files of utility reports should be placed
dir_mn_gas <- here("_energy", "data-raw", "mn_natGas_utility_reporting")

# Get list of all Excel files in the directory
file_list <- list.files(path = dir_mn_gas, pattern = "\\.xlsx$", full.names = TRUE)

# Function to process each file and extract county-level utility activity data
process_file <- function(file_path) {
  # assumes you gave the file a nice name -- basically, the utility's name
  utility_name <- tools::file_path_sans_ext(basename(file_path))

  # Read specific ranges from the file
  data_A_C <- read_excel(file_path, sheet = "GasByCounty", range = "A15:C59")
  data_E_G <- read_excel(file_path, sheet = "GasByCounty", range = "E15:G56")

  # Rename columns to reflect the data that is actuall read
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

  # Add utility name derived from excel file name
  combined_data$utility <- utility_name

  return(combined_data)
}

# Process all files and combine the data
combined_MNgasUtil_activityData <- do.call(rbind, lapply(file_list, process_file))

# Assuming each row in mn_electricity_data represents a utility's electricity delivery in a county,
# process and merge data -- this will be a separate data collection process spanning excel reports submitted to state
processed_mn_gasUtil_activityData <- combined_MNgasUtil_activityData %>%
  mutate(
    CO2_emissions = mcf_delivered * epa_emissionsHub_naturalGas_factor_lbsCO2_perMCF,
    CH4_emissions = mcf_delivered * epa_emissionsHub_naturalGas_factor_lbsCH4_perMCF * GWP_CH4,
    N2O_emissions = mcf_delivered * epa_emissionsHub_naturalGas_factor_lbsN2O_perMCF * GWP_N2O,
    CO2e_emissions = CO2_emissions + CH4_emissions + N2O_emissions
  )

# Aggregate data by county, add identifiers for state and sector
MNcounty_level_gas_emissions <- processed_mn_gasUtil_activityData %>%
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
    sector = "Natural gas",
    year = 2021
  )


write_rds(processed_mn_gasUtil_activityData, here("_energy", "data", "minnesota_gasUtils_ActivityAndEmissions.RDS"))
write_rds(MNcounty_level_gas_emissions, here("_energy", "data", "minnesota_county_GasEmissions.RDS"))
