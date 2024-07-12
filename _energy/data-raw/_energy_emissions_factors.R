if (exists("eGRID_MROW_emissionsFactor_CO2") == FALSE) {
  if (!exists("gwp")) {
    source("R/global_warming_potential.R")
  }

  epa_ghg_factor_hub <- readRDS("_meta/data/epa_ghg_factor_hub.RDS")
  mrow_factors <- epa_ghg_factor_hub$egrid


  # Load eGRID Total Output Emission Rates (lb/MWh) for the MROW subregion (
  # which covers our study area)
  # figures in lbs./mWh

  eGRID_MROW_emissionsFactor_CO2 <- mrow_factors %>%
    filter(emission == "lb CO2") %>%
    magrittr::extract2("value")
  eGRID_MROW_emissionsFactor_CH4 <- mrow_factors %>%
    filter(emission == "lb CH4") %>%
    magrittr::extract2("value")
  eGRID_MROW_emissionsFactor_N2O <- mrow_factors %>%
    filter(emission == "lb N2O") %>%
    magrittr::extract2("value")

  # Natural Gas emissions factor from https://www.epa.gov/system/files/documents/2023-04/emission-factors_sept2021.pdf
  # CO2
  # Emissions factor value provided by EPA (0.05444) is in terms of kg CO2 per scf
  # 1,000 scf = 1 mcf --> utilities report gas delivered to customers in Minnesota in mcf --> 54.44 kg CO2 per mcf
  # 1 kg = 2.20462262 lbs -->
  # EPA-provided emissions factor = 120.019655 lbs CO2 per mcf natural gas in 2021
  epa_emissionsHub_naturalGas_factor_lbsCO2_perMCF <- epa_ghg_factor_hub$stationary_combustion %>%
    filter(
      `Fuel type` == "Natural Gas",
      emission == "kg CO2",
      per_unit == "scf"
    ) %>%
    magrittr::extract2("value") * 1000 %>% # scf to mcf
      units::as_units("kg") %>%
      units::set_units("lb") %>%
      as.numeric()



  # CH4
  # 0.00103 g CH4 per scf --> 0.00000103 kg per scf ---> 0.00103 kg per mcf
  # 1 kg = 2.20462262 lbs --> 0.00227 lbs CH4 per mcf natural gas in 2021
  epa_emissionsHub_naturalGas_factor_lbsCH4_perMCF <- epa_ghg_factor_hub$stationary_combustion %>%
    filter(
      `Fuel type` == "Natural Gas",
      emission == "g CH4",
      per_unit == "scf"
    ) %>%
    magrittr::extract2("value") * 1000 %>% # scf to mcf
      units::as_units("gram") %>%
      units::set_units("lb") %>%
      as.numeric()

  # Global Warming Potential (GWP) is the multiplier from AR5 that converts emissions factors to CO2 equivalent
  GWP_CH4 <- gwp$ch4

  # N2O
  # 0.0001 g N2O per scf --> 0.0000001 kg per scf ---> 0.0001 kg per mcf
  # 1 kg = 2.20462262 lbs --> 0.0002 lbs N2O per mcf natural gas in 2021
  epa_emissionsHub_naturalGas_factor_lbsN2O_perMCF <- epa_ghg_factor_hub$stationary_combustion %>%
    filter(
      `Fuel type` == "Natural Gas",
      emission == "g N2O",
      per_unit == "scf"
    ) %>%
    magrittr::extract2("value") * 1000 %>% # scf to mcf
      units::as_units("gram") %>%
      units::set_units("lb") %>%
      as.numeric()

  # Global Warming Potential (GWP) is the multiplier from AR5 that converts emissions factors to CO2 equivalent
  GWP_N2O <- gwp$n2o
} else {
  cli::cli_inform(
    c("v" = "Energy emission factors\n"),
    .frequency = "once",
    .frequency_id = "load_packages"
  )
}
