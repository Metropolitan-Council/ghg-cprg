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
MNcounty_level_gas_emissions_2021 <- processed_mn_gasUtil_activityData %>%
  group_by(county) %>%
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
    sector = "Natural gas",
    year = 2021
  )


#incorporates totals numbers for Residential, Commercial, Industrial, internal use, Unaccounted For gas, and Deliveries to Transportation (i.e. deregulated competitive providers)
# Source: 2005 MN Utility Data Book

MN_state2005_natGasMCFTotal <- 367825000 # EIA State Energy Profile; 328955000 reported in Utility Data Book
MN_state2021_natGasMCFTotal <- 495126000 # EIA State Energy Profile

# create downscaled county-level data for 2005 and 2021 -- we have utility reports for 2021, so 2021 data is QA only; 2005 data is to be used as actuals
downscaleMN_gas_basedOnPopProps <- read_rds(here(
  "_meta",
  "data",
  "cprg_county_proportions.RDS"
)) %>%
  filter(STATEFP == 27 &
           year %in% c(2005,2021)) %>%
  select(year, county = NAME, county_proportion_of_state_pop) %>%
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

cprg_county_pops <- read_rds(here("_meta",
                                         "data",
                                         "cprg_county_proportions.RDS")
) %>%
  select(county = NAME, year, county_population) %>%
  filter(year %in% c(2005,2021))
                                    
MNcounty_level_gas_emissionsQA = rbind(downscaleMN_gas_basedOnPopProps_2005, MNcounty_level_gas_emissions_2021) %>%
  left_join(cprg_county_pops,
            by = join_by(county, year)
  ) %>%
  mutate(
    CO2eEmissions_PerCap_Tons = emissions_metric_tons_co2e / county_population
  ) %>%
  left_join(downscaleMN_gas_basedOnPopProps_2021 %>% select(county, year, EST_emissions_metric_tons_co2e = emissions_metric_tons_co2e),
            by = join_by(county, year)
            ) %>%
  mutate(
    EST_perCap_CO2e = EST_emissions_metric_tons_co2e / county_population
  )

MNcounty_level_gas_emissions <- MNcounty_level_gas_emissionsQA %>%
  select(-county_population, -CO2eEmissions_PerCap_Tons, EST_emissions_metric_tons_co2e, EST_perCap_CO2e)

#since we have more detailed info for 2021, save off as a separate .RDS for safekeeping
write_rds(processed_mn_gasUtil_activityData, here("_energy", "data", "minnesota_gasUtils_ActivityAndEmissions2021.RDS"))

# write combined 2005 (downscaled) and 2021 (derived from utility reports) activity/emissions data to .RDS
write_rds(MNcounty_level_gas_emissions, here("_energy", "data", "minnesota_county_GasEmissions.RDS"))
