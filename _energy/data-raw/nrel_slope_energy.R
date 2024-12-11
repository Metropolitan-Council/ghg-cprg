source("R/_load_pkgs.R")
source("_energy/data-raw/_energy_emissions_factors.R")
source("R/plot_county_emissions.R")

#add to lockfile once finalized
library(stringr)

cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_ctu <- readRDS("_meta/data/cprg_ctu.RDS")
mn_util_type <- readRDS("_energy/data/distinct_electricity_util_type_MN.RDS")
minnesota_elec_estimate_2021 <- readRDS("_energy/data/minnesota_elecUtils_ActivityAndEmissions_2021.RDS")

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
# download from NREL directly
download.file("https://gds-files.nrel.gov/slope/energy_consumption_expenditure_business_as_usual.zip",
  destfile = "_energy/data-raw/nrel_slope/energy_consumption_expenditure_business_as_usual.zip"
)
unzip("_energy/data-raw/nrel_slope/energy_consumption_expenditure_business_as_usual.zip",
  exdir = "_energy/data-raw/nrel_slope/"
)


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

nrel_slope_cprg_city <- cprg_ctu %>%
  left_join(read.csv("_energy/data-raw/nrel_slope/energy_consumption_expenditure_business_as_usual_city.csv") %>% 
              clean_names() %>%
              mutate(city_name = str_replace_all(city_name, "St\\.", "Saint")),
             by = c(
               "state_name",
               "ctu_name" = "city_name"
             )
  ) %>%
  
  # Identify CTU_NAMEs that have both City and non-City classifications
  group_by(ctu_name, state_name) %>%
    mutate(has_city_class = any(ctu_class == 'CITY')) %>%
    ungroup() %>%
  
  # If there's a City version of the same CTU_NAME, null out joined values for the non-City rows
  mutate(across(c(sector, year, geography_id, source, consumption_mm_btu, expenditure_us_dollars), 
                ~ ifelse(ctu_class != 'CITY' & has_city_class, NA, .))) %>%
  
  # Clean up the source column as per original logic
  mutate(source = ifelse(source == "ng", "Natural gas", "Electricity")) %>%
  
  # clean up unnecessary columns
  select(
    -has_city_class,
    -geoid_wis
  )



# Define the new columns
sectors <- c("commercial", "residential", "industrial")
sources <- c("Electricity", "Natural gas")

# Create all combinations of sectors and sources
sector_source <- expand.grid(sector = sectors, source = sources)



ctu_population <- readRDS("_meta/data/ctu_population.RDS") %>%
  filter(inventory_year > 2004) %>%
  left_join(cprg_county %>% select(geoid, county_name), by = 'geoid') %>%
  rename(year = inventory_year)

# Expand the dataset
expanded_ctu_population_sector_source <- ctu_population %>%
  # Cross join with sector-source combinations
  expand_grid(sector_source)



# city-level

# For city 2005... 


#join county to city
nrel_slope_cprg_cityProps_County <- nrel_slope_cprg_city %>%
  filter(year < 2024) %>%
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
    -geography_id.x,
    -geography_id.y,
    -county_name_full,
    -state_geography_id.x,
    -state_geography_id.y,
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
  st_drop_geometry()


nrel_AllCityTownships_county_activityPopProp_reference <- nrel_slope_cprg_cityProps_County %>%
  full_join(expanded_ctu_population_sector_source,
            by = join_by('ctu_name',
                         'ctu_class',
                         'county_name',
                         'sector',
                         'source',
                         'year')
  ) %>%
  select(-geoid.x,
        -geoid.y
  ) %>%
  mutate(
    cityConsumption_countyPopDownscaled_mmbtu = county_consumption_mm_btu * ctu_proportion_of_county_pop,
    state_name = "Minnesota"
  )


nrel_emissions_inv_cityQA <- bind_rows(
  # electricity emissions
  nrel_AllCityTownships_county_activityPopProp_reference %>%
    filter(source == "Electricity") %>%
    rowwise() %>%
    mutate(
      # convert mmbtu to Mwh
      city_consumption_mwh = city_consumption_mm_btu * mmbtu_to_mwh,
      cityPopDownscaled_consumption_mwh = cityConsumption_countyPopDownscaled_mmbtu * mmbtu_to_mwh,
      county_consumption_mwh = county_consumption_mm_btu * mmbtu_to_mwh,
      # apply emission factor and convert to metric tons
      co2_city = (city_consumption_mwh * eGRID_MROW_emissionsFactor_CO2_2021) %>%
        units::as_units("lb") %>%
        units::set_units("ton") %>%
        as.numeric(),
      ch4_city = (city_consumption_mwh * eGRID_MROW_emissionsFactor_CH4_2021) %>%
        units::as_units("lb") %>%
        units::set_units("ton") %>%
        as.numeric(),
      n2o_city = (city_consumption_mwh * eGRID_MROW_emissionsFactor_N2O_2021) %>%
        units::as_units("lb") %>%
        units::set_units("ton") %>%
        as.numeric(),
      co2e_city =
        co2_city +
        (ch4_city * gwp$n2o) +
        (n2o_city * gwp$n2o),
      co2_cityPopDownscaled = (cityPopDownscaled_consumption_mwh * eGRID_MROW_emissionsFactor_CO2_2021) %>%
        units::as_units("lb") %>%
        units::set_units("ton") %>%
        as.numeric(),
      ch4_cityPopDownscaled = (cityPopDownscaled_consumption_mwh * eGRID_MROW_emissionsFactor_CH4_2021) %>%
        units::as_units("lb") %>%
        units::set_units("ton") %>%
        as.numeric(),
      n2o_cityPopDownscaled = (cityPopDownscaled_consumption_mwh * eGRID_MROW_emissionsFactor_N2O_2021) %>%
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
      city_consumption_mcf = city_consumption_mm_btu * mmbtu_to_mcf,
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
  )

saveRDS(nrel_emissions_inv_cityQA, "_energy/data-raw/nrel_slope/nrel_emissions_inv_cityQA.RDS")
#compare city figures 1) provided directly by NREL to 2) those downscaled from county figures provided by NREL using CTU pop proportion of county populations


countySummary_nrelCity <- nrel_slope_cprg_cityProps_County%>%
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

# issues to address
# double counting of cities across counties..
# removing portions of cities from city total that don't exist within the county study area JUST FOR calculation of proportions...




  
  
#join to pop and pop prop table... use to infill townships/towns/villages?





#For city 2021, use NREL-forecasted city proportion of forecasted COUNTY total emissions to allocate actual emissions gathered at county level (Separate from NREL)
# and then use to allocate city level forecast PROPORTIONS to allocate to sectors
# do projected activity-emissions at city-sector lefvel add up? a test to write.


# calculate activity/emissions reported by NREL SLOPE at COUNTY level that AREN'T REPORTED at city level. 

# use population numbers and weighted per capita to fill in emissions estimations at city level for those cities that NREL SLOPE doesn't directly provide. 


# nrel_slope_state <- read.csv("_energy/data-raw/nrel_slope/energy_consumption_expenditure_business_as_usual_state.csv") %>%
#   clean_names()


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

# plot_ly(
#   data = nrel_emissions_region %>%
#     filter(year == 2021),
#   x = ~sector_raw,
#   y = ~co2e,
#   color = ~source
# )


nrel_slope_proportions <- nrel_emissions_inv_county %>%
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
  filter(year == 2021) %>%
  ungroup() %>%
  mutate(county = county_name) %>%
  select(-total, -county_name)

saveRDS(nrel_emissions_inv_county, "_energy/data-raw/nrel_slope/nrel_emissions_inv_county.RDS")
saveRDS(nrel_slope_proportions, "_energy/data-raw/nrel_slope/nrel_slope_proportions.RDS")

# plot_ly(
#   data = nrel_emissions_region %>%
#     filter(source == "Electricity"),
#   x = ~year,
#   y = ~co2e,
#   color = ~str_to_sentence(category),
#   type = "bar"
# ) %>%
#   plotly_layout(
#     main_title = "Electricity",
#     subtitle = "",
#     x_title = "Year",
#     y_title = "Metric tones CO<sub>2</sub>e",
#     legend_title = "Sector"
#   )
#
#
# plot_ly(
#   data = nrel_emissions_region %>%
#     filter(source == "Natural gas"),
#   x = ~year,
#   y = ~co2e,
#   color = ~str_to_sentence(category),
#   type = "bar"
# ) %>%
#   plotly_layout(
#     main_title = "Natural gas",
#     subtitle = "",
#     x_title = "Year",
#     y_title = "Metric tones CO<sub>2</sub>e",
#     legend_title = "Sector"
#   )
