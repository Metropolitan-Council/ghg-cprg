source("R/_load_pkgs.R")
source("_energy/data-raw/_energy_emissions_factors.R")
source("R/plot_county_emissions.R")
source("_energy/data-raw/_energy_emissions_factors.R")

#add to lockfile once finalized
library(stringr)

cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_ctu <- readRDS("_meta/data/cprg_ctu.RDS")
mn_util_type <- readRDS("_energy/data/distinct_electricity_util_type_MN.RDS")
minnesota_elec_estimate_2021 <- readRDS("_energy/data/minnesota_elecUtils_ActivityAndEmissions_2021.RDS")

#read in time series of eGRID emissions factor data and pivot wider to make one row per year with all three emission types
egridTimeSeries <- epa_ghg_factor_hub$egridTimeSeries %>%
  pivot_wider(names_from = emission, values_from = value)

# NREL SLOPE energy consumption and expenditure data download, cleaning, and viz

# 1 Mmbtu is 0.293071 MWH
mmbtu_to_mwh <- 0.293071

# 1000 cubic feet is 1.038 MMBtu
# https://www.naturalgasintel.com/natural-gas-converter/
# 1 mmbtu is 1 mcf
mmbtu_to_mcf <- 1

#have to grab activity vs. emissions, so that activity specifically can be parceled out
countyActivity <- minnesota_elec_estimate_2021 %>%
  group_by(county) %>%
  summarize(
    mWh_delivered_county = sum(mWh_delivered, na.rm = TRUE)
  ) %>%
  ungroup()

# if file doesn't already exist...
if (file.exists("_energy/data-raw/nrel_slope/energy_consumption_expenditure_business_as_usual_county.csv") == FALSE) {

  # download from NREL directly
  download.file("https://gds-files.nrel.gov/slope/energy_consumption_expenditure_business_as_usual.zip",
    destfile = "_energy/data-raw/nrel_slope/energy_consumption_expenditure_business_as_usual.zip"
  )
  #unpack the files into dedicated sub-directory
  unzip("_energy/data-raw/nrel_slope/energy_consumption_expenditure_business_as_usual.zip",
    exdir = "_energy/data-raw/nrel_slope/"
  )

}

nrel_slope_cprg_county <- read.csv("_energy/data-raw/nrel_slope/energy_consumption_expenditure_business_as_usual_county.csv") %>%
  clean_names() %>%
  inner_join(cprg_county,
             by = c(
               "state_name",
               "county_name"
             )
  ) %>%
  mutate(source = ifelse(source == "ng", "Natural gas", "Electricity")) %>%
  select(-geometry)

#read in, clean up, and filter to MN/WI before joining to year-sector-source expanded cprg_ctu schema
nrel_city_clean <- read.csv("_energy/data-raw/nrel_slope/energy_consumption_expenditure_business_as_usual_city.csv") %>% 
  clean_names() %>%
  filter(state_name == 'Minnesota') %>%
  
  # align NREL city naming with cprg_ctu and clean up source
  mutate(
    city_name = str_replace_all(city_name, "St\\.", "Saint"), 
    source = ifelse(source == "ng", "Natural gas", "Electricity")
  )


# create scaffolding of final, finest data granularity based on NREL -- sector-source-year
sectors <- c("commercial", "residential", "industrial")
sources <- c("Electricity", "Natural gas")
years <- seq(2017, 2050)

# Create all combinations of sector-source-year (for use on cities) and simplified sector-source (for county data)
sector_source_year <- expand.grid(sector = sectors, source = sources, year = years)
sector_source <- expand.grid(sector = sectors, source = sources)




nrel_slope_cprg_city <- cprg_ctu %>%
  # add 204 rows representing 34 years (2017-2050 inclusive) of NREL data, 3 sectors, and 2 sources (33*3*2=204)
  expand_grid(sector_source_year) %>%
  
  left_join(nrel_city_clean,
             by = c(
               "state_name",
               "ctu_name" = "city_name",
               "year",
               "sector",
               "source"
             )
  ) %>%
  
  # Identify CTU_NAMEs that have both City and non-City classifications
  group_by(ctu_name, state_name) %>%
    mutate(has_city_class = any(ctu_class == 'CITY')) %>%
    ungroup() %>%
  
  # If there's a City version of the same CTU_NAME, null out joined values for the non-City rows
  mutate(across(c(consumption_mm_btu, expenditure_us_dollars), 
                ~ ifelse(ctu_class != 'CITY' & has_city_class, NA, .))) %>%
  
  # clean up unnecessary columns
  select(
    -has_city_class,
    -state_geography_id,
    -geography_id
  )



ctu_population <- readRDS("_meta/data/ctu_population.RDS") %>%
  filter(inventory_year > 2004) %>%
  left_join(cprg_county %>% select(geoid, county_name, state_abb), by = 'geoid') %>%
  filter(state_abb == 'MN') %>%
  rename(year = inventory_year) 

# Expand the dataset
expanded_ctu_population_sector_source <- ctu_population %>%
  # Cross join with sector-source combinations
  expand_grid(sector_source)



# city-level

# For city 2005... 

# Only keep MN core metro cities for city-level analysis
#join county to city
nrel_slope_cprg_cityProps_County <- nrel_slope_cprg_city %>%
  filter(year < 2025) %>%
  left_join(nrel_slope_cprg_county,
            by = c(
              "county_name",
              "state_name",
              "sector" = "sector",
              "year" = "year",
              "source" = "source"
            )    
  ) %>%
  filter(county_name %in% c('Anoka', 'Carver', 'Dakota', 'Hennepin', 'Ramsey', 'Scott', 'Washington')) %>%
  select(
    -cprg_area.x,
    -cprg_area.y,
    -gnis,
    -geography_id,
    -county_name_full,
    -state_geography_id,
    -state_abb.x,
    -state_abb.y,
    -statefp.x,
    -statefp.y
  ) %>%
  rename(
     city_consumption_mm_btu = consumption_mm_btu.x,
     county_consumption_mm_btu = consumption_mm_btu.y,
     city_expenditure_usd = expenditure_us_dollars.x,
     county_expenditure_usd = expenditure_us_dollars.y
  ) %>%
  mutate(
    cityPropOfCounty_consumption_mm_btu = city_consumption_mm_btu / county_consumption_mm_btu,
    cityPropOfCounty_expenditure_usd = city_expenditure_usd / county_expenditure_usd
  ) %>%
  st_drop_geometry() %>%
  select(-geometry)


nrel_AllCityTownships_county_activityPopProp_reference <- nrel_slope_cprg_cityProps_County %>%
  left_join(expanded_ctu_population_sector_source,
            by = join_by('ctu_name',
                         'ctu_class',
                         'county_name',
                         'sector',
                         'source',
                         'year')
  ) %>%
  mutate(
    cityConsumption_countyPopDownscaled_mmbtu = county_consumption_mm_btu * ctu_proportion_of_county_pop
  ) %>%
  rename(city_name = ctu_name) %>%
  select(-geoid.x,
         -geoid.y) %>%
  # check to make sure that NREL estimates for cities that exist in multiple counties are parceled out by source-sector
  group_by(city_name, year, sector, source) %>%
  mutate(
    total_ctu_population = sum(ctu_population, na.rm = TRUE),
    multi_county = n_distinct(county_name) > 1  # Flag multi-county cities with a Boolean
  ) %>%
  ungroup() %>%
  # Calculate the proportion of CTU total pop within each COCTU
  mutate(
    ctu_population_proportion = ctu_population / total_ctu_population,
    # only calc these columns, using flag variable multi_county
    disagg_city_consumption_mm_btu = ifelse(
      multi_county,
      city_consumption_mm_btu * ctu_population_proportion,
      NA
    ),
    disagg_city_expenditure_usd = ifelse(
      multi_county,
      city_expenditure_usd * ctu_population_proportion,
      NA
    )
  )
# clarify variables to ensure clear sourcing.
  
# coalesce and describe source
  


nrel_emissions_inv_city <- bind_rows(
  # electricity emissions
  nrel_AllCityTownships_county_activityPopProp_reference %>%
    filter(source == "Electricity") %>%
    left_join(egridTimeSeries,
              by = join_by(year == Year)
    ) %>%
    rowwise() %>%
    mutate(
      # convert mmbtu to Mwh
      city_consumption_mwh = coalesce(disagg_city_consumption_mm_btu, city_consumption_mm_btu) * mmbtu_to_mwh,
      cityPopDownscaled_consumption_mwh = cityConsumption_countyPopDownscaled_mmbtu * mmbtu_to_mwh,
      county_consumption_mwh = county_consumption_mm_btu * mmbtu_to_mwh,
      # apply emission factor and convert to metric tons
      co2_city = (city_consumption_mwh * `lb CO2`) %>%
        units::as_units("lb") %>%
        units::set_units("ton") %>%
        as.numeric(),
      ch4_city = (city_consumption_mwh * `lb CH4`) %>%
        units::as_units("lb") %>%
        units::set_units("ton") %>%
        as.numeric(),
      n2o_city = (city_consumption_mwh * `lb N2O`) %>%
        units::as_units("lb") %>%
        units::set_units("ton") %>%
        as.numeric(),
      co2e_city =
        co2_city +
        (ch4_city * gwp$n2o) +
        (n2o_city * gwp$n2o),
      co2_cityPopDownscaled = (cityPopDownscaled_consumption_mwh * `lb CO2`) %>%
        units::as_units("lb") %>%
        units::set_units("ton") %>%
        as.numeric(),
      ch4_cityPopDownscaled = (cityPopDownscaled_consumption_mwh * `lb CH4`) %>%
        units::as_units("lb") %>%
        units::set_units("ton") %>%
        as.numeric(),
      n2o_cityPopDownscaled = (cityPopDownscaled_consumption_mwh * `lb N2O`) %>%
        units::as_units("lb") %>%
        units::set_units("ton") %>%
        as.numeric(),
      co2e_cityPopDownscaled =
        co2_cityPopDownscaled +
        (ch4_cityPopDownscaled * gwp$n2o) +
        (n2o_cityPopDownscaled * gwp$n2o),
      co2_county = (county_consumption_mwh * eGRID_MROW_emissionsFactor_CO2_2021) %>%
        units::as_units("lb") %>%
        units::set_units("ton") %>%
        as.numeric(),
      ch4_county = (county_consumption_mwh * eGRID_MROW_emissionsFactor_CH4_2021) %>%
        units::as_units("lb") %>%
        units::set_units("ton") %>%
        as.numeric(),
      n2o_county = (county_consumption_mwh * eGRID_MROW_emissionsFactor_N2O_2021) %>%
        units::as_units("lb") %>%
        units::set_units("ton") %>%
        as.numeric(),
      co2e_county =
        co2_county +
        (ch4_county * gwp$n2o) +
        (n2o_county * gwp$n2o)
    ),
  # natural gas emissions
  nrel_AllCityTownships_county_activityPopProp_reference %>%
    filter(source == "Natural gas") %>%
    rowwise() %>%
    mutate(
      # convert mmbtu to mcf
      city_consumption_mcf = coalesce(disagg_city_consumption_mm_btu, city_consumption_mm_btu) * mmbtu_to_mcf,
      cityPopDownscaled_consumption_mcf = cityConsumption_countyPopDownscaled_mmbtu * mmbtu_to_mcf,
      county_consumption_mcf = county_consumption_mm_btu * mmbtu_to_mcf,
      # apply emission factor and convert to metric tons
      co2_city = (city_consumption_mcf * epa_emissionsHub_naturalGas_factor_lbsCO2_perMCF) %>%
        units::as_units("lb") %>%
        units::set_units("ton") %>%
        as.numeric(),
      ch4_city = (city_consumption_mcf * epa_emissionsHub_naturalGas_factor_lbsCH4_perMCF) %>%
        units::as_units("lb") %>%
        units::set_units("ton") %>%
        as.numeric(),
      n2o_city = (city_consumption_mcf * epa_emissionsHub_naturalGas_factor_lbsN2O_perMCF) %>%
        units::as_units("lb") %>%
        units::set_units("ton") %>%
        as.numeric(),
      co2e_city =
        co2_city +
        (ch4_city * gwp$n2o) +
        (n2o_city * gwp$n2o),
      co2_cityPopDownscaled = (cityPopDownscaled_consumption_mcf * epa_emissionsHub_naturalGas_factor_lbsCO2_perMCF) %>%
        units::as_units("lb") %>%
        units::set_units("ton") %>%
        as.numeric(),
      ch4_cityPopDownscaled = (cityPopDownscaled_consumption_mcf * epa_emissionsHub_naturalGas_factor_lbsCH4_perMCF) %>%
        units::as_units("lb") %>%
        units::set_units("ton") %>%
        as.numeric(),
      n2o_cityPopDownscaled = (cityPopDownscaled_consumption_mcf * epa_emissionsHub_naturalGas_factor_lbsN2O_perMCF) %>%
        units::as_units("lb") %>%
        units::set_units("ton") %>%
        as.numeric(),
      co2e_cityPopDownscaled =
        co2_cityPopDownscaled +
        (ch4_cityPopDownscaled * gwp$n2o) +
        (n2o_cityPopDownscaled * gwp$n2o),
      co2_county = (county_consumption_mcf * epa_emissionsHub_naturalGas_factor_lbsCO2_perMCF) %>%
        units::as_units("lb") %>%
        units::set_units("ton") %>%
        as.numeric(),
      ch4_county = (county_consumption_mcf * epa_emissionsHub_naturalGas_factor_lbsCH4_perMCF) %>%
        units::as_units("lb") %>%
        units::set_units("ton") %>%
        as.numeric(),
      n2o_county = (county_consumption_mcf * epa_emissionsHub_naturalGas_factor_lbsN2O_perMCF) %>%
        units::as_units("lb") %>%
        units::set_units("ton") %>%
        as.numeric(),
      co2e_county =
        co2_county +
        (ch4_county * gwp$n2o) +
        (n2o_county * gwp$n2o)
    )
) %>%
  mutate(
    category = ifelse(sector == "residential", "Residential", "Non-residential"),
    sector_raw = sector,
    sector = "Energy"
  ) %>%
  #remove electricity specific emissions factor columns
  select(
    -eGrid_Subregion,
    -factor_type,
    -per_unit,
    -Source,
    -`lb CO2`,
    -`lb CH4`,
    -`lb N2O`
  )

saveRDS(nrel_emissions_inv_city, "_energy/data-raw/nrel_slope/nrel_emissions_inv_city.RDS")

#compare city figures 1) provided directly by NREL to 2) those downscaled from county figures provided by NREL using CTU pop proportion of county populations


countySummary_nrelCity <- nrel_slope_cprg_cityProps_County %>%
  st_drop_geometry() %>%
  group_by(year, county_name, sector, source) %>%
  summarize(
    sectorSource_accountedByCities = sum(cityPropOfCounty_consumption_mm_btu),
    sectorSource_consumptionToteCities = sum(city_consumption_mm_btu),
    countyTotalConsumption = max(county_consumption_mm_btu),
    .groups = 'keep'
  ) %>%
  mutate(
    QA_calc = sectorSource_consumptionToteCities / countyTotalConsumption
  )
  #city-source-sector prop of county-source-sector emissions



nrel_emissions_inv_county <- bind_rows(
  # electricity emissions
  nrel_slope_cprg_county %>%
    #Emission INVENTORY is < 2025, forecasts is >= 2025
    filter(year < 2025) %>%
    filter(source == "Electricity") %>%
    rowwise() %>%
    mutate(
      # convert mmbtu to Mwh
      consumption_mwh = consumption_mm_btu * mmbtu_to_mwh,
      # apply emission factor and convert to metric tons
      co2 = (consumption_mwh * eGRID_MROW_emissionsFactor_CO2_2021) %>%
        units::as_units("lb") %>%
        units::set_units("ton") %>%
        as.numeric(),
      ch4 = (consumption_mwh * eGRID_MROW_emissionsFactor_CH4_2021) %>%
        units::as_units("lb") %>%
        units::set_units("ton") %>%
        as.numeric(),
      n2o = (consumption_mwh * eGRID_MROW_emissionsFactor_N2O_2021) %>%
        units::as_units("lb") %>%
        units::set_units("ton") %>%
        as.numeric(),
      co2e =
        co2 +
          (ch4 * gwp$n2o) +
          (n2o * gwp$n2o)
    ),
  # natural gas emissions
  nrel_slope_cprg_county %>%
    filter(source == "Natural gas") %>%
    filter(year < 2025) %>%
    rowwise() %>%
    mutate(
      # convert mmbtu to mcf
      consumption_mcf = consumption_mm_btu * mmbtu_to_mcf,
      # apply emission factor and convert to metric tons
      co2 = (consumption_mcf * epa_emissionsHub_naturalGas_factor_lbsCO2_perMCF) %>%
        units::as_units("lb") %>%
        units::set_units("ton") %>%
        as.numeric(),
      ch4 = (consumption_mcf * epa_emissionsHub_naturalGas_factor_lbsCH4_perMCF) %>%
        units::as_units("lb") %>%
        units::set_units("ton") %>%
        as.numeric(),
      n2o = (consumption_mcf * epa_emissionsHub_naturalGas_factor_lbsN2O_perMCF) %>%
        units::as_units("lb") %>%
        units::set_units("ton") %>%
        as.numeric(),
      co2e =
        co2 +
          (ch4 * gwp$n2o) +
          (n2o * gwp$n2o)
    )
) %>%
  mutate(
    category = ifelse(sector == "residential", "Residential", "Non-residential"),
    sector_raw = sector,
    sector = "Energy"
  )

saveRDS(nrel_emissions_inv_county, "_energy/data-raw/nrel_slope/nrel_emissions_inv_county.RDS")


# find county proportions by year and source
nrel_emissions_region <- nrel_emissions_inv_county %>%
  group_by(year, sector, sector_raw, category, source) %>%
  summarize(
    consumption_mm_btu = sum(consumption_mm_btu),
    expenditure_us_dollars = sum(expenditure_us_dollars),
    co2e = sum(co2e)
  )

nrel_emissions_region %>%
  filter(
    year == 2021,
    source == "Electricity"
  ) %>%
  group_by(year, source, sector_raw) %>%
  summarize(co2e = sum(co2e))


nrel_slope_county_proportions <- nrel_emissions_inv_county %>%
  group_by(county_name, year, source) %>%
  select(county_name, year, source, sector_raw, co2e) %>%
  pivot_wider(
    names_from = sector_raw,
    values_from = co2e
  ) %>%
  rowwise() %>%
  summarize(
    total = commercial + residential + industrial,
    commercial = commercial / total,
    industrial = industrial / total,
    residential = residential / total,
    .groups = "keep"
  ) %>%
  ungroup() %>%
  mutate(county = county_name) %>%
  select(-total, -county_name)


nrel_slope_city_emission_proportions <- nrel_emissions_inv_city %>%
  # Group and summarize to aggregate within each city-year's sector_raw
  group_by(city_name, ctu_class, year, source, sector_raw) %>%
  summarize(
    co2e_city = sum(co2e_city, na.rm = TRUE),
    co2e_cityPopDownscaled = sum(co2e_cityPopDownscaled, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  # Pivot both co2e_city and co2e_cityPopDownscaled at the same time
  pivot_wider(
    names_from = sector_raw,
    values_from = c(co2e_city, co2e_cityPopDownscaled),
    names_glue = "{.value}_{sector_raw}"
  ) %>%
  
  # Calculate totals and proportions
  mutate(
    # Totals
    total_nrelCity = rowSums(select(., starts_with("co2e_city_")), na.rm = TRUE),
    total_popDownscale = rowSums(select(., starts_with("co2e_cityPopDownscaled_")), na.rm = TRUE),
    
    # Proportions for nrelCity emissions
    commercial_city = ifelse(total_nrelCity > 0, co2e_city_commercial / total_nrelCity, NA),
    industrial_city = ifelse(total_nrelCity > 0, co2e_city_industrial / total_nrelCity, NA),
    residential_city = ifelse(total_nrelCity > 0, co2e_city_residential / total_nrelCity, NA),
    
    # Proportions for cityPopDownscale emissions
    commercial_downscale = ifelse(total_popDownscale > 0, co2e_cityPopDownscaled_commercial / total_popDownscale, NA),
    industrial_downscale = ifelse(total_popDownscale > 0, co2e_cityPopDownscaled_industrial / total_popDownscale, NA),
    residential_downscale = ifelse(total_popDownscale > 0, co2e_cityPopDownscaled_residential / total_popDownscale, NA)
  ) %>%
  select(-total_popDownscale,
         -total_nrelCity,
         -co2e_city_commercial,
         -co2e_city_industrial,
         -co2e_city_residential,
         -co2e_cityPopDownscaled_commercial,
         -co2e_cityPopDownscaled_industrial,
         -co2e_cityPopDownscaled_residential)


#fix downstream references to county level RDS (named nrel_slope_county_proportions)
saveRDS(nrel_slope_county_proportions, "_energy/data-raw/nrel_slope/nrel_slope_city_emission_proportions.RDS")
saveRDS(nrel_slope_city_emission_proportions, "_energy/data-raw/nrel_slope/nrel_slope_city_emission_proportions.RDS")

# ARCHIVED COMMENTS/NOTES
# For city 2021, use NREL-forecasted city proportion of forecasted COUNTY total emissions to allocate actual emissions gathered at county level (Separate from NREL)
# and then use to allocate city level forecast PROPORTIONS to allocate to sectors
# do projected activity-emissions at city-sector lefvel add up? a test to write.

# calculate activity/emissions reported by NREL SLOPE at COUNTY level that AREN'T REPORTED at city level. 
# use population numbers and weighted per capita to fill in emissions estimations at city level for those cities that NREL SLOPE doesn't directly provide. 

