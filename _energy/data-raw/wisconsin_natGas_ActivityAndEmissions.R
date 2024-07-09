source("R/_load_pkgs.R")
source("_energy/data-raw/_energy_emissions_factors.R")
# WI data is reported in therms delivered. to match MN and the EPA conversion factor, which are reported in mcf, we use the conversion factor supplied by the EIA --
# One thousand cubic feet (Mcf) of natural gas equals 1.038 MMBtu, or 10.38 therms.
# https://www.eia.gov/tools/faqs/faq.php?id=45&t=8

# maybe make an appendix with ALL report links of relevant utilities across NG and elec
# Midwest Natural Gas Incorporated
# 2005: https://apps.psc.wi.gov/PDFfiles/Annual%20Reports/IOU/IOU_2005_3670.pdf
# 2021: https://apps.psc.wi.gov/PDFfiles/Annual%20Reports/IOU/IOU_2021_3670.pdf

# Northern States Power Company - Wisconsin
# 2005: https://apps.psc.wi.gov/PDFfiles/Annual%20Reports/IOU/IOU_2005_4220.pdf
# 2021: https://apps.psc.wi.gov/PDFfiles/Annual%20Reports/IOU/IOU_2021_4220.pdf

# St Croix Valley Natural Gas Company
# 2005: https://apps.psc.wi.gov/PDFfiles/Annual%20Reports/IOU/IOU_2005_5230.pdf
# 2021: https://apps.psc.wi.gov/PDFfiles/Annual%20Reports/IOU/IOU_2021_5230.pdf

# Wisconsin Gas
# 2005: https://apps.psc.wi.gov/PDFfiles/Annual%20Reports/IOU/IOU_2005_6650.pdf
# 2021: https://apps.psc.wi.gov/PDFfiles/Annual%20Reports/IOU/IOU_2021_6650.pdf

therms_to_MCF <- 1 / 10.38

WIutilities_in_scope <- read_rds(here("_energy", "data", "WI_natGas_inScope_utilityCountyPairs.RDS")) %>%
  # Create two rows for each utility to house 2005 and 2021 activity data
  expand_grid(year = c(2005, 2021))


combined_WIgasUtil_activityData <- WIutilities_in_scope %>%
  st_drop_geometry(WIutilities_in_scope) %>%
  select(utility_name, county_name, year) %>%
  # WI utilities report gas sales in therms -- apply EIA transformation factor to tabulate activity in mcf
  mutate(
    util_total_mcf = case_when(
      year == 2005 & utility_name == "Midwest Natural Gas Incorporated" ~ 18816267 * therms_to_MCF,
      year == 2005 & utility_name == "Northern States Power Company - Wisconsin" ~ 147572365 * therms_to_MCF, # gas sold (incl interdepartmental) -- NOT total send out (that metric includes transport gas)
      year == 2005 & utility_name == "St Croix Valley Natural Gas Company" ~ 9534249 * therms_to_MCF,
      year == 2005 & utility_name == "Wisconsin Gas" ~ 728522194 * therms_to_MCF, # total send out... need to make sure "transport" is not included.... final sale only.
      year == 2021 & utility_name == "Midwest Natural Gas Incorporated" ~ 23181392 * therms_to_MCF,
      year == 2021 & utility_name == "Northern States Power Company - Wisconsin" ~ 171102649 * therms_to_MCF, 
      year == 2021 & utility_name == "St Croix Valley Natural Gas Company" ~ 11155826 * therms_to_MCF,
      year == 2021 & utility_name == "Wisconsin Gas" ~ 751394716 * therms_to_MCF # previously recorded: 1881722450 -- big drop since such a large transport business
    ),
    utility_TotalCustomerCount = case_when(
      year == 2005 & utility_name == "Midwest Natural Gas Incorporated" ~ 13845,
      year == 2005 & utility_name == "Northern States Power Company - Wisconsin" ~ 93588, 
      year == 2005 & utility_name == "St Croix Valley Natural Gas Company" ~ 6939,
      year == 2005 & utility_name == "Wisconsin Gas" ~ 583336,
      year == 2021 & utility_name == "Midwest Natural Gas Incorporated" ~ 18793,
      year == 2021 & utility_name == "Northern States Power Company - Wisconsin" ~ 113012,
      year == 2021 & utility_name == "St Croix Valley Natural Gas Company" ~ 9227,
      year == 2021 & utility_name == "Wisconsin Gas" ~ 645576
    ),
    utilityCustomer_county = case_when(
      year == 2005 & utility_name == "Midwest Natural Gas Incorporated" & county_name == "St. Croix" ~ 3516,
      year == 2005 & utility_name == "Northern States Power Company - Wisconsin" & county_name == "Pierce" ~ 0,
      year == 2005 & utility_name == "Northern States Power Company - Wisconsin" & county_name == "St. Croix" ~ 12138, # 11878 (incl city) + 260 (unallocated to a city, but within county)
      year == 2005 & utility_name == "St Croix Valley Natural Gas Company" & county_name == "Pierce" ~ 4573,
      year == 2005 & utility_name == "St Croix Valley Natural Gas Company" & county_name == "St. Croix" ~ 2366,
      year == 2005 & utility_name == "Wisconsin Gas" & county_name == "Pierce" ~ 3039,
      year == 2005 & utility_name == "Wisconsin Gas" & county_name == "St. Croix" ~ 3453,
      year == 2021 & utility_name == "Midwest Natural Gas Incorporated" & county_name == "St. Croix" ~ 5573,
      year == 2021 & utility_name == "Northern States Power Company - Wisconsin" & county_name == "Pierce" ~ 107,
      year == 2021 & utility_name == "Northern States Power Company - Wisconsin" & county_name == "St. Croix" ~ 15990,
      year == 2021 & utility_name == "St Croix Valley Natural Gas Company" & county_name == "Pierce" ~ 5410,
      year == 2021 & utility_name == "St Croix Valley Natural Gas Company" & county_name == "St. Croix" ~ 3817,
      year == 2021 & utility_name == "Wisconsin Gas" & county_name == "Pierce" ~ 3320,
      year == 2021 & utility_name == "Wisconsin Gas" & county_name == "St. Croix" ~ 4252
    )
  ) %>%
  mutate(
    # proportion of customers accounts per county
    propCustomerAccountsInCounty =
      utilityCustomer_county / utility_TotalCustomerCount
  ) %>%
  mutate(
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
  group_by(county_name, year) %>%
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
      na.rm = TRUE,
      .groups = "keep"
    ),
    total_CO2e_emissions_tons = total_CO2e_emissions_lbs / 2000,
    emissions_metric_tons_co2e = total_CO2e_emissions_lbs %>%
      units::as_units("pound") %>%
      units::set_units("metric_ton") %>%
      as.numeric()
  ) %>%
  mutate(
    state = "WI",
    sector = "Natural gas",
  )


write_rds(processed_wi_gasUtil_activityData, here("_energy", "data", "wisconsin_gasUtils_ActivityAndEmissions.RDS"))
write_rds(WIcounty_level_gas_emissions, here("_energy", "data", "wisconsin_county_GasEmissions.RDS"))
