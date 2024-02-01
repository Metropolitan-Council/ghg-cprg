source("R/_load_pkgs.R")
source("_energy/data-raw/_energy_emissions_factors.R")
# WI data is reported in therms delivered. to match MN and the EPA conversion factor, which are reported in mcf, we use the conversion factor supplied by the EIA -- One thousand cubic feet (Mcf) of natural gas equals 1.038 MMBtu, or 10.38 therms. https://www.eia.gov/tools/faqs/faq.php?id=45&t=8

# maybe make an appendix with ALL report links of relevant utilities across NG and elec
# Midwest Natural Gas Incorporated
# https://apps.psc.wi.gov/PDFfiles/Annual%20Reports/IOU/IOU_2021_3670.pdf

# Northern States Power Company - Wisconsin
# https://apps.psc.wi.gov/PDFfiles/Annual%20Reports/IOU/IOU_2021_4220.pdf

# St Croix Valley Natural Gas Company
# https://apps.psc.wi.gov/PDFfiles/Annual%20Reports/IOU/IOU_2021_5230.pdf

# Wisconsin Gas
# https://apps.psc.wi.gov/PDFfiles/Annual%20Reports/IOU/IOU_2021_6650.pdf

# TODO double check where this factor came from
therms_to_MCF <- 1 / 10.38

WIutilities_in_scope <- read_rds(here("_energy", "data", "WI_natGas_inScope_utilityCountyPairs.RDS"))

combined_WIgasUtil_activityData <- WIutilities_in_scope %>%
  st_drop_geometry(WIutilities_in_scope) %>%
  select(utility_name, county_name) %>%
  mutate(
    # total energy delivered by each utility
    util_total_mcf = case_when(
      utility_name == "Midwest Natural Gas Incorporated" ~
        24210191 * therms_to_MCF,
      utility_name == "Northern States Power Company - Wisconsin" ~
        203165849 * therms_to_MCF,
      utility_name == "St Croix Valley Natural Gas Company" ~
        13266914 * therms_to_MCF,
      utility_name == "Wisconsin Gas" ~
        1881722450 * therms_to_MCF
    ),
    # total customers over a utility's entire service territory
    utility_TotalCustomerCount = case_when(
      utility_name == "Midwest Natural Gas Incorporated" ~ 18793,
      utility_name == "Northern States Power Company - Wisconsin" ~ 113012,
      utility_name == "St Croix Valley Natural Gas Company" ~ 9227,
      utility_name == "Wisconsin Gas" ~ 645576,
    ),
    # customers served by each utility in a given county
    utilityCustomer_county = case_when(
      utility_name == "Midwest Natural Gas Incorporated" &
        county_name == "St. Croix" ~ 5573,
      utility_name == "Northern States Power Company - Wisconsin" &
        county_name == "Pierce" ~ 107,
      utility_name == "Northern States Power Company - Wisconsin" &
        county_name == "St. Croix" ~ 15990,
      utility_name == "St Croix Valley Natural Gas Company" &
        county_name == "Pierce" ~ 5410,
      utility_name == "St Croix Valley Natural Gas Company" &
        county_name == "St. Croix" ~ 3817,
      utility_name == "Wisconsin Gas" &
        county_name == "Pierce" ~ 3320,
      utility_name == "Wisconsin Gas" &
        county_name == "St. Croix" ~ 4252
    ),
    # proportion of customers accounts per county
    propCustomerAccountsInCounty =
      utilityCustomer_county / utility_TotalCustomerCount,
    # allocate activity data to in-scope counties based on proportion of customers
    mcf_delivered = util_total_mcf * propCustomerAccountsInCounty
  )


# Assuming each row in mn_electricity_data represents a utility's electricity delivery in a county, process and merge data -- this will be a separate data colelction process spanning excel reports submitted to state
processed_wi_gasUtil_activityData <- combined_WIgasUtil_activityData %>%
  mutate(
    CO2_emissions = mcf_delivered * epa_emissionsHub_naturalGas_factor_lbsCO2_perMCF,
    CH4_emissions = mcf_delivered * epa_emissionsHub_naturalGas_factor_lbsCH4_perMCF * GWP_CH4,
    N2O_emissions = mcf_delivered * epa_emissionsHub_naturalGas_factor_lbsN2O_perMCF * GWP_N2O,
    CO2e_emissions = CO2_emissions + CH4_emissions + N2O_emissions
  )

# Aggregate data by county, add identifiers for state and sector
WIcounty_level_gas_emissions <- processed_wi_gasUtil_activityData %>%
  group_by(county_name) %>%
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
    state = "WI",
    sector = "Natural Gas",
    year = 2021
  )


write_rds(processed_wi_gasUtil_activityData, here("_energy", "data", "wisconsin_gasUtils_ActivityAndEmissions"))
write_rds(WIcounty_level_gas_emissions, here("_energy", "data", "wisconsin_county_GasEmissions.RDS"))
