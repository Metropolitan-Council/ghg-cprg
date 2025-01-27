source("R/_load_pkgs.R")
overwrite_rds <- TRUE

# Let's turn this into a function where the user can input the constants, or use the default values
# This function will be used in the data-raw script to create the constants data frame
create_ww_constants <- function(
    Per_capita_BOD5 = 0.09,
    Fraction_BOD5_anaerobically_digested = 12.65 / 100,
    Emission_Factor_CH4_BOD5 = 0.6,
    Factor_non_consumption_nitrogen = 1.75,
    Fraction_population_not_on_septic = 83 / 100,
    Direct_wwtp_emissions = 4.0,
    Emission_Factor_N2O_N = 0.005,
    Fraction_nitrogen_in_protein = 16 / 100,
    Wastewater_Outflow_Fruits_Vegetables = 9.11,
    COD_Fruits_Vegetables = 5,
    Fraction_COD_anaerobically_degraded_Fruits_Vegetables = 0 / 100,
    Emission_factor_CH4_COD_Fruits_Vegetables = 0.25,
    Wastewater_Outflow_Red_Meat = 5.3,
    COD_Red_Meat = 4.1,
    Fraction_COD_anaerobically_degraded_Red_Meat = 33 / 100,
    Emission_factor_CH4_COD_Red_Meat = 0.25,
    Wastewater_Outflow_Poultry = 12.5,
    COD_Poultry = 4.1,
    Fraction_COD_anaerobically_degraded_Poultry = 25 / 100,
    Emission_factor_CH4_COD_Poultry = 0.25,
    Wastewater_Outflow_Pulp_Paper = 39,
    BOD_Pulp_Paper = 0.3,
    Fraction_BOD_anaerobically_degraded_Pulp_Paper = 5.2 / 100,
    Emission_factor_CH4_BOD_Pulp_Paper = 0.6) {
  base_constants <- data.frame(
    Variable = c(
      "N2O_N_MWR",
      "CO2_C_MWR",
      "CH4_GWP",
      "N2O_GWP",
      "n_days_per_year",
      "MMT_per_MT",
      "MT_per_kg",
      "L_per_m3",
      "g_per_Tg",
      "g_per_MT"
    ),
    Description = c(
      "N2O/N2 (molecular weight ratio)",
      "C/CO2 (molecular weight ratio)",
      "Methane GWP",
      "Nitrous Oxide GWP",
      "Number of Days per Year",
      "Million Metric Tons / Metric Tons (MMT/MT)",
      "Metric Tons / Kg",
      "Liters / Cubic Meter",
      "Grams / Teragram",
      "Grams / Metric Ton"
    ),
    Default = c(
      1.57, # "N2O/N2 (molecular weight ratio)",
      0.27, # "C/CO2 (molecular weight ratio)",
      28, # "Methane GWP",
      265, # "Nitrous Oxide GWP",
      365, # "Number of Days per Year",
      0.000001, # "Million Metric Tons / Metric Tons (MMT/MT)",
      0.001, # "Metric Tons / Kg",
      1000, # "Liters / Cubic Meter",
      1E-12, # "Grams / Teragram",
      1E-6 # "Grams / Metric Ton",
    ),
    Value = c(
      1.57, # "N2O/N2 (molecular weight ratio)",
      0.27, # "C/CO2 (molecular weight ratio)",
      28, # "Methane GWP",
      265, # "Nitrous Oxide GWP",
      365, # "Number of Days per Year",
      0.000001, # "Million Metric Tons / Metric Tons (MMT/MT)",
      0.001, # "Metric Tons / Kg",
      1000, # "Liters / Cubic Meter",
      1E-12, # "Grams / Teragram",
      1E-6 # "Grams / Metric Ton",
    )
  )

  # Municipal Wastewater CH4 Emissions --------------------------------------
  # Per capita 5-day Biochemical Oxygen Demand (BOD5) (kg/day)
  def_Per_capita_BOD5 <- 0.09
  # Fraction of wastewater BOD5 anaerobically digested
  def_Fraction_BOD5_anaerobically_digested <- 12.65 / 100
  # Emission Factor (Gg CH4/Gg BOD5)
  def_Emission_Factor_CH4_BOD5 <- 0.6


  # Municipal Wastewater Direct N2O Emissions -------------------------------
  # Factor non-consumption nitrogen
  def_Factor_non_consumption_nitrogen <- 1.75
  # Fraction of population not on septic
  def_Fraction_population_not_on_septic <- 83 / 100
  # Direct wastewater treatment plant emissions (g N2O/person/year)
  def_Direct_wwtp_emissions <- 4.0


  # Municipal Wastewater N2O Emissions from Biosolids ------------------------
  # Emission Factor (kg N2O-N/kg sewage N-produced)
  def_Emission_Factor_N2O_N <- 0.005
  # Fraction of nitrogen in protein (FracNPR)
  def_Fraction_nitrogen_in_protein <- 16 / 100


  # Industrial Wastewater CH4 Emissions - Fruits and Vegetables --------------
  # Wastewater Outflow (m3/metric ton)
  def_Wastewater_Outflow_Fruits_Vegetables <- 9.11
  # WW Organic Content - Chemical Oxygen Demand (COD) (g/l)
  def_COD_Fruits_Vegetables <- 5
  # Fraction of COD anaerobically degraded
  def_Fraction_COD_anaerobically_degraded_Fruits_Vegetables <- 0 / 100
  # Emission factor (g CH4/g COD)
  def_Emission_factor_CH4_COD_Fruits_Vegetables <- 0.25


  # Industrial Wastewater CH4 Emissions - Red Meat --------------------------
  # Wastewater Outflow (m3/metric ton)
  def_Wastewater_Outflow_Red_Meat <- 5.3
  # WW Organic Content - Chemical Oxygen Demand (COD) (g/l)
  def_COD_Red_Meat <- 4.1
  # Fraction of COD anaerobically degraded
  def_Fraction_COD_anaerobically_degraded_Red_Meat <- 33 / 100
  # Emission factor (g CH4/g COD)
  def_Emission_factor_CH4_COD_Red_Meat <- 0.25

  # Industrial Wastewater CH4 Emissions - Poultry ---------------------------
  # Wastewater Outflow (m3/metric ton)
  def_Wastewater_Outflow_Poultry <- 12.5
  # WW Organic Content - Chemical Oxygen Demand (COD) (g/l)
  def_COD_Poultry <- 4.1
  # Fraction of COD anaerobically degraded
  def_Fraction_COD_anaerobically_degraded_Poultry <- 25 / 100
  # Emission factor (g CH4/g COD)
  def_Emission_factor_CH4_COD_Poultry <- 0.25


  # Industrial Wastewater CH4 Emissions - Pulp and Paper --------------------
  # Wastewater Outflow (m3/metric ton)
  def_Wastewater_Outflow_Pulp_Paper <- 39
  # WW Organic Content - Chemical Oxygen Demand (COD) (g/l)
  def_BOD_Pulp_Paper <- 0.3
  # Fraction of COD anaerobically degraded
  def_Fraction_BOD_anaerobically_degraded_Pulp_Paper <- 5.2 / 100
  # Emission factor (g CH4/g COD)
  def_Emission_factor_CH4_BOD_Pulp_Paper <- 0.6



  default_constants <- data.frame(
    Variable = c(
      "Per_capita_BOD5",
      "Fraction_BOD5_anaerobically_digested",
      "Emission_Factor_CH4_BOD5",
      "Factor_non_consumption_nitrogen",
      "Fraction_population_not_on_septic",
      "Direct_wwtp_emissions",
      "Emission_Factor_N2O_N",
      "Fraction_nitrogen_in_protein",
      "Wastewater_Outflow_Fruits_Vegetables",
      "COD_Fruits_Vegetables",
      "Fraction_COD_anaerobically_degraded_Fruits_Vegetables",
      "Emission_factor_CH4_COD_Fruits_Vegetables",
      "Wastewater_Outflow_Red_Meat",
      "COD_Red_Meat",
      "Fraction_COD_anaerobically_degraded_Red_Meat",
      "Emission_factor_CH4_COD_Red_Meat",
      "Wastewater_Outflow_Poultry",
      "COD_Poultry",
      "Fraction_COD_anaerobically_degraded_Poultry",
      "Emission_factor_CH4_COD_Poultry",
      "Wastewater_Outflow_Pulp_Paper",
      "BOD_Pulp_Paper",
      "Fraction_BOD_anaerobically_degraded_Pulp_Paper",
      "Emission_factor_CH4_BOD_Pulp_Paper"
    ),
    Description = c(
      "Per capita 5-day Biochemical Oxygen Demand (BOD5) (kg/day)",
      "Fraction of wastewater BOD5 anaerobically digested",
      "Emission Factor (Gg CH4/Gg BOD5)",
      "Factor non-consumption nitrogen",
      "Fraction of population not on septic",
      "Direct wastewater treatment plant emissions (g N2O/person/year)",
      "Emission Factor (kg N2O-N/kg sewage N-produced)",
      "Fraction of nitrogen in protein (FracNPR)",
      "Wastewater Outflow (m3/metric ton) - Fruits and Vegetables",
      "WW Organic Content - Chemical Oxygen Demand (COD) (g/l) - Fruits and Vegetables",
      "Fraction of COD anaerobically degraded - Fruits and Vegetables",
      "Emission factor (g CH4/g COD) - Fruits and Vegetables",
      "Wastewater Outflow (m3/metric ton) - Red Meat",
      "WW Organic Content - Chemical Oxygen Demand (COD) (g/l) - Red Meat",
      "Fraction of COD anaerobically degraded - Red Meat",
      "Emission factor (g CH4/g COD) - Red Meat",
      "Wastewater Outflow (m3/metric ton) - Poultry",
      "WW Organic Content - Chemical Oxygen Demand (COD) (g/l) - Poultry",
      "Fraction of COD anaerobically degraded - Poultry",
      "Emission factor (g CH4/g COD) - Poultry",
      "Wastewater Outflow (m3/metric ton) - Pulp and Paper",
      "WW Organic Content - Biochemical Oxygen Demand (BOD) (g/l) - Pulp and Paper",
      "Fraction of BOD anaerobically degraded - Pulp and Paper",
      "Emission factor (g CH4/g BOD) - Pulp and Paper"
    ),
    Default = c(
      def_Per_capita_BOD5,
      def_Fraction_BOD5_anaerobically_digested,
      def_Emission_Factor_CH4_BOD5,
      def_Factor_non_consumption_nitrogen,
      def_Fraction_population_not_on_septic,
      def_Direct_wwtp_emissions,
      def_Emission_Factor_N2O_N,
      def_Fraction_nitrogen_in_protein,
      def_Wastewater_Outflow_Fruits_Vegetables,
      def_COD_Fruits_Vegetables,
      def_Fraction_COD_anaerobically_degraded_Fruits_Vegetables,
      def_Emission_factor_CH4_COD_Fruits_Vegetables,
      def_Wastewater_Outflow_Red_Meat,
      def_COD_Red_Meat,
      def_Fraction_COD_anaerobically_degraded_Red_Meat,
      def_Emission_factor_CH4_COD_Red_Meat,
      def_Wastewater_Outflow_Poultry,
      def_COD_Poultry,
      def_Fraction_COD_anaerobically_degraded_Poultry,
      def_Emission_factor_CH4_COD_Poultry,
      def_Wastewater_Outflow_Pulp_Paper,
      def_BOD_Pulp_Paper,
      def_Fraction_BOD_anaerobically_degraded_Pulp_Paper,
      def_Emission_factor_CH4_BOD_Pulp_Paper
    ),
    Value = c(
      Per_capita_BOD5,
      Fraction_BOD5_anaerobically_digested,
      Emission_Factor_CH4_BOD5,
      Factor_non_consumption_nitrogen,
      Fraction_population_not_on_septic,
      Direct_wwtp_emissions,
      Emission_Factor_N2O_N,
      Fraction_nitrogen_in_protein,
      Wastewater_Outflow_Fruits_Vegetables,
      COD_Fruits_Vegetables,
      Fraction_COD_anaerobically_degraded_Fruits_Vegetables,
      Emission_factor_CH4_COD_Fruits_Vegetables,
      Wastewater_Outflow_Red_Meat,
      COD_Red_Meat,
      Fraction_COD_anaerobically_degraded_Red_Meat,
      Emission_factor_CH4_COD_Red_Meat,
      Wastewater_Outflow_Poultry,
      COD_Poultry,
      Fraction_COD_anaerobically_degraded_Poultry,
      Emission_factor_CH4_COD_Poultry,
      Wastewater_Outflow_Pulp_Paper,
      BOD_Pulp_Paper,
      Fraction_BOD_anaerobically_degraded_Pulp_Paper,
      Emission_factor_CH4_BOD_Pulp_Paper
    )
  )


  constants <- rbind(base_constants, default_constants) # %>% as_tibble()
  return(constants)
}





## MINNESOTA DEFAULTS -------------------------------------------------
# Municipal Wastewater CH4 Emissions
# Per capita 5-day Biochemical Oxygen Demand (BOD5) (kg/day)
Per_capita_BOD5 <- 0.09
# Fraction of wastewater BOD5 anaerobically digested
Fraction_BOD5_anaerobically_digested <- 12.65 / 100
# Emission Factor (Gg CH4/Gg BOD5)
Emission_Factor_CH4_BOD5 <- 0.6


# Municipal Wastewater Direct N2O Emissions
# Factor non-consumption nitrogen
Factor_non_consumption_nitrogen <- 1.75
# Fraction of population not on septic
Fraction_population_not_on_septic <- 83 / 100
# Direct wastewater treatment plant emissions (g N2O/person/year)
Direct_wwtp_emissions <- 4.0


# Municipal Wastewater N2O Emissions from Biosolids
# Emission Factor (kg N2O-N/kg sewage N-produced)
Emission_Factor_N2O_N <- 0.005
# Fraction of nitrogen in protein (FracNPR)
Fraction_nitrogen_in_protein <- 16 / 100


# Industrial Wastewater CH4 Emissions - Fruits and Vegetables
# Wastewater Outflow (m3/metric ton)
Wastewater_Outflow_Fruits_Vegetables <- 9.11
# WW Organic Content - Chemical Oxygen Demand (COD) (g/l)
COD_Fruits_Vegetables <- 5
# Fraction of COD anaerobically degraded
Fraction_COD_anaerobically_degraded_Fruits_Vegetables <- 0 / 100
# Emission factor (g CH4/g COD)
Emission_factor_CH4_COD_Fruits_Vegetables <- 0.25


# Industrial Wastewater CH4 Emissions - Red Meat
# Wastewater Outflow (m3/metric ton)
Wastewater_Outflow_Red_Meat <- 5.3
# WW Organic Content - Chemical Oxygen Demand (COD) (g/l)
COD_Red_Meat <- 4.1
# Fraction of COD anaerobically degraded
Fraction_COD_anaerobically_degraded_Red_Meat <- 33 / 100
# Emission factor (g CH4/g COD)
Emission_factor_CH4_COD_Red_Meat <- 0.25

# Industrial Wastewater CH4 Emissions - Poultry
# Wastewater Outflow (m3/metric ton)
Wastewater_Outflow_Poultry <- 12.5
# WW Organic Content - Chemical Oxygen Demand (COD) (g/l)
COD_Poultry <- 4.1
# Fraction of COD anaerobically degraded
Fraction_COD_anaerobically_degraded_Poultry <- 25 / 100
# Emission factor (g CH4/g COD)
Emission_factor_CH4_COD_Poultry <- 0.25


# Industrial Wastewater CH4 Emissions - Pulp and Paper
# Wastewater Outflow (m3/metric ton)
Wastewater_Outflow_Pulp_Paper <- 39
# WW Organic Content - Chemical Oxygen Demand (COD) (g/l)
BOD_Pulp_Paper <- 0.3
# Fraction of COD anaerobically degraded
Fraction_BOD_anaerobically_degraded_Pulp_Paper <- 5.2 / 100
# Emission factor (g CH4/g COD)
Emission_factor_CH4_BOD_Pulp_Paper <- 0.6



epa_wastewater_mn_constants <-
  create_ww_constants(
    Per_capita_BOD5,
    Fraction_BOD5_anaerobically_digested,
    Emission_Factor_CH4_BOD5,
    Factor_non_consumption_nitrogen,
    Fraction_population_not_on_septic,
    Direct_wwtp_emissions,
    Emission_Factor_N2O_N,
    Fraction_nitrogen_in_protein,
    Wastewater_Outflow_Fruits_Vegetables,
    COD_Fruits_Vegetables,
    Fraction_COD_anaerobically_degraded_Fruits_Vegetables,
    Emission_factor_CH4_COD_Fruits_Vegetables,
    Wastewater_Outflow_Red_Meat,
    COD_Red_Meat,
    Fraction_COD_anaerobically_degraded_Red_Meat,
    Emission_factor_CH4_COD_Red_Meat,
    Wastewater_Outflow_Poultry,
    COD_Poultry,
    Fraction_COD_anaerobically_degraded_Poultry,
    Emission_factor_CH4_COD_Poultry,
    Wastewater_Outflow_Pulp_Paper,
    BOD_Pulp_Paper,
    Fraction_BOD_anaerobically_degraded_Pulp_Paper,
    Emission_factor_CH4_BOD_Pulp_Paper
  ) %>%
  dplyr::select(-Default) %>%
  mutate(STATE = "MN") %>%
  relocate(STATE, Value, Variable, Description) %>%
  as_tibble()





## WISCONSIN DEFAULTS -------------------------------------------------
# Municipal Wastewater CH4 Emissions
# Per capita 5-day Biochemical Oxygen Demand (BOD5) (kg/day)
Per_capita_BOD5 <- 0.09
# Fraction of wastewater BOD5 anaerobically digested
Fraction_BOD5_anaerobically_digested <- 12.65 / 100
# Emission Factor (Gg CH4/Gg BOD5)
Emission_Factor_CH4_BOD5 <- 0.6


# Municipal Wastewater Direct N2O Emissions
# Factor non-consumption nitrogen
Factor_non_consumption_nitrogen <- 1.75
# Fraction of population not on septic
Fraction_population_not_on_septic <- 83 / 100
# Direct wastewater treatment plant emissions (g N2O/person/year)
Direct_wwtp_emissions <- 4.0


# Municipal Wastewater N2O Emissions from Biosolids
# Emission Factor (kg N2O-N/kg sewage N-produced)
Emission_Factor_N2O_N <- 0.005
# Fraction of nitrogen in protein (FracNPR)
Fraction_nitrogen_in_protein <- 16 / 100


# Industrial Wastewater CH4 Emissions - Fruits and Vegetables
# Wastewater Outflow (m3/metric ton)
Wastewater_Outflow_Fruits_Vegetables <- 9.11
# WW Organic Content - Chemical Oxygen Demand (COD) (g/l)
COD_Fruits_Vegetables <- 5
# Fraction of COD anaerobically degraded
Fraction_COD_anaerobically_degraded_Fruits_Vegetables <- 0 / 100
# Emission factor (g CH4/g COD)
Emission_factor_CH4_COD_Fruits_Vegetables <- 0.25


# Industrial Wastewater CH4 Emissions - Red Meat
# Wastewater Outflow (m3/metric ton)
Wastewater_Outflow_Red_Meat <- 5.3
# WW Organic Content - Chemical Oxygen Demand (COD) (g/l)
COD_Red_Meat <- 4.1
# Fraction of COD anaerobically degraded
Fraction_COD_anaerobically_degraded_Red_Meat <- 33 / 100
# Emission factor (g CH4/g COD)
Emission_factor_CH4_COD_Red_Meat <- 0.25

# Industrial Wastewater CH4 Emissions - Poultry
# Wastewater Outflow (m3/metric ton)
Wastewater_Outflow_Poultry <- 12.5
# WW Organic Content - Chemical Oxygen Demand (COD) (g/l)
COD_Poultry <- 4.1
# Fraction of COD anaerobically degraded
Fraction_COD_anaerobically_degraded_Poultry <- 25 / 100
# Emission factor (g CH4/g COD)
Emission_factor_CH4_COD_Poultry <- 0.25


# Industrial Wastewater CH4 Emissions - Pulp and Paper
# Wastewater Outflow (m3/metric ton)
Wastewater_Outflow_Pulp_Paper <- 39
# WW Organic Content - Chemical Oxygen Demand (COD) (g/l)
BOD_Pulp_Paper <- 0.3
# Fraction of COD anaerobically degraded
Fraction_BOD_anaerobically_degraded_Pulp_Paper <- 5.2 / 100
# Emission factor (g CH4/g COD)
Emission_factor_CH4_BOD_Pulp_Paper <- 0.6



epa_wastewater_wi_constants <-
  create_ww_constants(
    Per_capita_BOD5,
    Fraction_BOD5_anaerobically_digested,
    Emission_Factor_CH4_BOD5,
    Factor_non_consumption_nitrogen,
    Fraction_population_not_on_septic,
    Direct_wwtp_emissions,
    Emission_Factor_N2O_N,
    Fraction_nitrogen_in_protein,
    Wastewater_Outflow_Fruits_Vegetables,
    COD_Fruits_Vegetables,
    Fraction_COD_anaerobically_degraded_Fruits_Vegetables,
    Emission_factor_CH4_COD_Fruits_Vegetables,
    Wastewater_Outflow_Red_Meat,
    COD_Red_Meat,
    Fraction_COD_anaerobically_degraded_Red_Meat,
    Emission_factor_CH4_COD_Red_Meat,
    Wastewater_Outflow_Poultry,
    COD_Poultry,
    Fraction_COD_anaerobically_degraded_Poultry,
    Emission_factor_CH4_COD_Poultry,
    Wastewater_Outflow_Pulp_Paper,
    BOD_Pulp_Paper,
    Fraction_BOD_anaerobically_degraded_Pulp_Paper,
    Emission_factor_CH4_BOD_Pulp_Paper
  ) %>%
  dplyr::select(-Default) %>%
  mutate(STATE = "WI") %>%
  relocate(STATE, Value, Variable, Description) %>%
  as_tibble()



epa_wastewater_constants <- rbind(epa_wastewater_mn_constants, epa_wastewater_wi_constants)

if (overwrite_rds) {
  # Export the updated data frame as an RDS file
  saveRDS(epa_wastewater_constants, "_waste/data-raw/wastewater_v2/data-raw/epa_wastewater_constants.RDS")
}
