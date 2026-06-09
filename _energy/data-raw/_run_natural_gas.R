rm(list = ls())
# =============================================================================
# Natural Gas & Propane Pipeline — Compilation Script
# =============================================================================
# Builds county- and CTU-level natural gas and propane/fuel oil consumption
# and emissions estimates for the nine-county Twin Cities metro, 2005–2023.

# 01 — DATA INGESTION ---------------------------------------------------------
# These scripts compile utility and ACS data. Running them requires downloading
# the indicated files within each scripts from the mirrored Teams folders, except
# ACS data used for propane analysis which requires ACS API

# 01_compile_minnesota_7610_reporting_natgas.R
# Processes MN PUC 7610 utility filings (Xcel, CenterPoint, MER, others)
# into county-level MCF totals. Computes county proportions using GRAND TOTAL rows as
# denominators. REQUIRES DATA ON MIRRORED TEAMS FOLDER.
source("_energy/data-raw/01_compile_minnesota_7610_reporting_natgas.R")

# 01_compile_xcel_ctu.R
# Reads CTU-level electricity and natural gas data from Xcel Community Reports.
# Produces residential_mcf and business_mcf at CTU-year level (2015–2023).
# REQUIRES DATA ON MIRRORED TEAMS FOLDER. - LONG RUN TIME.
source("_energy/data-raw/01_compile_xcel_ctu.R")

# 01_compile_centerpoint_natgas.R
# Reads CTU-level natural gas data from CenterPoint.
# Produces residential_mcf and business_mcf at CTU-year level (2015–2023).
# Divvies same named city and township based on population splits
# REQUIRES DATA ON MIRRORED TEAMS FOLDER.
source("_energy/data-raw/01_compile_centerpoint_natgas.R")

#01_compile_sql_utility_records.R
# Reads CTU-level electricity and natural gas data from previous Met Council inventory.
# Reads in data on MS SQL server.
source("_energy/data-raw/01_compile_sql_utility_records.R")

# 01_compile_rii_ctu_elec_natgas.R
# Reads CTU-level electricity and natural gas data from RII inventory.
# REQUIRES DATA ON MIRRORED TEAMS FOLDER.
source("_energy/data-raw/01_compile_rii_ctu_elec_natgas.R")


# 01_compile_propane_kerosene_hh.R
# Queries ACS B25040 (house heating fuel) at CTU and county scales.
# Forces propane/kerosene households to 0 for urban-class CTUs.
# Kalman-extrapolates back to 2005. Saves both CTU and county household files.
# Outputs: propane_kerosene_hh_ctu.RDS, propane_kerosene_hh_county.RDS
source("_energy/data-raw/01_compile_propane_kerosene_hh.R")


# 02 — DATA COMPILATION -------------------------------------------------------
# These scripts mostly compile the above sources, though there is some additional
# data ingestion that relies on the above data

# 02_compile_ctu_natgas.R
# compile natural gas records from data read-ins. Prioritizes recent utility
# records sent to MC over past MC requests and RII data.
# output: ctu_utility_mcf.RDS
source("_energy/data-raw/02_compile_ctu_natgas.R")

# # 02_compile_minnesota_utility_handbook_natgas.R
# Compiles MN Utility Data Handbook Tables 12 (statewide annual), 13 (2006
# company-level), and 14 (2012 company-level) into a crosswalk of utility
# shares used for pre-2010 county proportional allocation.
# Uses the county proportions of utilities from 7610 reporting to
# partition total utility data.
# Output: county_natgas_activity.RDS
source("_energy/data-raw/02_compile_minnesota_utility_handbook_natgas.R")

# 02_calculate_fueloil_energy.R
# Downloads EIA SEDS residential consumption (HLRCB, DFRCB, KSRCB) for MN/WI.
# Disaggregates state totals to CTU and county using ACS household shares.
# pulls ACS data, requiring API
# Outputs: ctu_propane_fueloil_use.RDS, county_propane_fueloil_use.RDS
# (propane_mmBtu, fueloil_other_mmBtu)
source("_energy/data-raw/02_calculate_fueloil_energy.R")

# 03 — MODELING ----------------------------------------------------------------

# 03_model_ctu_natgas_residential.R
# Random forest model predicting CTU residential natural gas MCF where no utility data exists.
# Requires _meta objects: UrbanSim housing units, NOAA HDD, thrive designation, parcel data.
# Anchors RF to last known utility data year.
# Limited to 2010 and onward by UrbanSim data
# Splits cities across counties as needed
# Outputs: predicted_coctu_residential_mcf.rds
source("_energy/data-raw/03_predict_ctu_residential_natgas.R")

# 03_predict_ctu_business_natgas.R
# Random forest model predicting CTU non-residential natural gas MCF where no utility data exists.
# Requires _meta objects: UrbanSim jobs, NOAA HDD, thrive designation
# Anchors RF to last known utility data year.
# Limited to 2010 and onward by UrbanSim data
# Splits cities across counties as needed
# Outputs: predicted_coctu_business_mcf.rds
source("_energy/data-raw/03_predict_ctu_business_natgas.R")

# 04 — CTU coalescing -------------------------------------------------

# 04_combine_ctu_natgas_predictions.R
# Stitches residential and business CTU natural gas data: known utility actuals, RF predictions.
# Joins propane/fuel oil for total residential mmBtu calculation.
# Sanity checks make sure that previously unused utility data (i.e. no sectoral split, incomplete CTU)
# is appropriately considered.
# output: ctu_ng_combined.rds
source("_energy/data-raw/04_combine_ctu_natgas_predictions.R")


# 05 — County sectoral partitioning and emissions -------------------------------------------------

# 05_calculate_ctu_fuel_emissions.R
# Backcasts pre-2010 nat gas estimates using pre-2010 county data.
# holds 2010 CTU fueloil activity constant back to 2005
# calculates emissions for both based on EPA factor hub
# output: _ctu_natgas_emissions.RDS; _ctu_liquid_emissions.RDS
source("_energy/data-raw/05_calculate_ctu_fuel_emissions.R")

# 05_compile_temporal_county_mc_proportion.R
# Split county 7610 reports into four sectors/components: residential, commercial, industrial, powerplant
# Residential is summed total of ctu residential analysis
# industrial is summed total of non-powerplant/refinery natural gas combustion form _industrial GHGRP analysis
# powerplants are removed from 7610 on case-by-case basis as several are clearly off local utility pipeline. Uncertainty remains here
# commercial is residual of 7610 minus the above. There is certainly some small industrial included here and this should be revisited
# Outputs: county_natgas_emissions_by_sector.RDS
source("_energy/data-raw/05_compile_temporal_county_mc_proportion.R")

# 05_calculate_county_fueloil_emissions.R
# Applies EPA factor hub to fueloil energy and outputs activity and emssions
# output: county_propane_fueloil_activity_emissions.RDS
source("_energy/data-raw/05_calculate_county_fueloil_emissions.R")
