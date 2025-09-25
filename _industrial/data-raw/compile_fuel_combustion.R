#### Script to read in and process EPA GHG FLIGHT data
source("R/_load_pkgs.R")

cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_ctu <- readRDS("_meta/data/cprg_ctu.RDS") %>%
  mutate(city_name = str_to_title(gsub("ST.", "Saint", ctu_name)))
industrial_hub <- readRDS("_meta/data/epa_ghg_factor_hub.RDS") %>%
  extract2("industrial_combustion") %>%
  clean_names()


### download emissions by fuel type data: https://www.epa.gov/ghgreporting/data-sets
ind_unit_data <-
  read_excel(
    file.path(
      here::here(),
      "_industrial/data-raw/emissions_by_unit_and_fuel_type_c_d_aa.xlsx"
    ),
    sheet = "UNIT_DATA",
    skip = 6
  ) %>%
  clean_names() %>%
  filter(
    state %in% c("MN", "WI"),
    # remove power plants and municipal waste (double-counting)
    !grepl("D", industry_type_subparts),
    !grepl("HH", industry_type_subparts),
    !industry_type_sectors == "Power Plants"
  ) %>%
  mutate(city_name = str_to_title(gsub("ST.", "Saint", city, ignore.case = TRUE))) %>%
  inner_join(., cprg_ctu %>% select(state_abb, city_name, county_name) %>%
    st_drop_geometry(),
  by = c("state" = "state_abb", "city_name")
  )

ind_fuel_data <- read_excel(file.path(here::here(), "_industrial/data-raw/emissions_by_unit_and_fuel_type_c_d_aa.xlsx"),
  sheet = "FUEL_DATA",
  skip = 5
) %>%
  clean_names() %>%
  filter(
    state %in% c("MN", "WI"),
    # remove power plants and municipal waste (doublecounting)
    !grepl("D", industry_type_subparts),
    !grepl("HH", industry_type_subparts),
    !industry_type_sectors == "Power Plants"
  ) %>%
  mutate(city_name = str_to_title(gsub("ST.", "Saint", city, ignore.case = TRUE))) %>%
  inner_join(., cprg_ctu %>% select(state_abb, city_name, county_name) %>%
    st_drop_geometry(),
  by = c("state" = "state_abb", "city_name")
  )

# unique fuel types and matching to ghg factor hub

unique(ind_fuel_data$specific_fuel_type)
unique(industrial_hub$fuel_type)

ind_fuel_data %>%
  filter(!specific_fuel_type %in% industrial_hub$fuel_type) %>%
  distinct(specific_fuel_type)

# only 5, manually changing them to match hub
ind_fuel_data <- ind_fuel_data %>%
  mutate(corrected_fuel_type = case_when(
    specific_fuel_type == "Natural Gas (Weighted U.S. Average)" ~ "Natural Gas",
    specific_fuel_type == "Wood and Wood Residuals (dry basis)" ~ "Wood and Wood Residuals",
    specific_fuel_type == "Coke" ~ "Coal Coke",
    specific_fuel_type == "Liquefied petroleum gases (LPG)" ~ "Liquefied Petroleum Gases (LPG)",
    specific_fuel_type == "Subbituminous" ~ "Sub-bituminous Coal",
    TRUE ~ specific_fuel_type
  ))

### create conversion factors to back-translate CO2e to activity data

industrial_hub <- industrial_hub %>%
  # remove mmBtu - all fuels also have volume/weight
  filter(
    per_unit != "mmBtu",
    emission != "mmBtu"
  ) %>%
  # convert to metric tons of gas
  mutate(mt_gas = as.numeric(case_when(
    grepl("CO2", emission) ~ value * units::as_units("kilogram") %>%
      units::set_units("metric_ton"),
    TRUE ~ value * units::as_units("gram") %>%
      units::set_units("metric_ton")
  ))) %>%
  # convert to CO2e using IPCC 4 assessment values (consistent with EPA dataset not ours)
  mutate(mt_co2e = case_when(
    grepl("CH4", emission) ~ mt_gas * 25,
    grepl("N2O", emission) ~ mt_gas * 298,
    TRUE ~ mt_gas
  )) %>%
  # units per mt of co2e
  mutate(unit_co2e = as.numeric(1 / mt_co2e))

# create conversion table from ch4/n2o co2e to activity units
co2e_to_unit <- industrial_hub %>%
  filter(
    fuel_type %in% ind_fuel_data$corrected_fuel_type,
    !grepl("CO2", emission)
  ) %>%
  mutate(gas = gsub("g ", "", emission)) %>%
  pivot_wider(
    id_cols = c(fuel_type, per_unit),
    names_from = gas,
    values_from = unit_co2e
  )

### calculate the activity units of each unit's combustion by fuel type
ind_fuel_activity <- ind_fuel_data %>%
  select(
    facility_id, facility_name, industry_type_subparts,
    county_name, city_name, reporting_year,
    unit_name, general_fuel_type, corrected_fuel_type,
    fuel_methane_ch4_emissions_mt_co2e,
    fuel_nitrous_oxide_n2o_emissions_mt_co2e
  ) %>%
  left_join(., co2e_to_unit,
    by = c("corrected_fuel_type" = "fuel_type")
  ) %>%
  # back-calculating units from BOTH gases to compare
  mutate(
    unit_ch4 = fuel_methane_ch4_emissions_mt_co2e * CH4,
    unit_n2o = fuel_nitrous_oxide_n2o_emissions_mt_co2e * N2O
  )

### How do ch4 and n2o units compare?
ggplot(ind_fuel_activity, aes(x = unit_ch4, y = unit_n2o)) +
  geom_point() +
  geom_abline(slope = 1, linetype = "dashed", color = "Red") +
  facet_wrap(~per_unit, scales = "free")
# looks good overall, very little disagreement, will take average unit btw gas calcs

# arrange to back-calculate activity to all gas emissions (i.e. include co2)
unit_to_mt <- industrial_hub %>%
  filter(fuel_type %in% ind_fuel_data$corrected_fuel_type) %>%
  mutate(gas = sub(".*? ", "", emission)) %>%
  pivot_wider(
    id_cols = c(fuel_type),
    names_prefix = "mt_unit_",
    names_from = gas,
    values_from = mt_gas
  )

# calculate each gas emission in specific gas MT and co2e MT
ind_fuel_emissions <- ind_fuel_activity %>%
  left_join(., unit_to_mt,
    by = c("corrected_fuel_type" = "fuel_type")
  ) %>%
  # average activities
  mutate(avg_activity = (unit_ch4 + unit_n2o) / 2) %>%
  mutate(
    mt_co2 = avg_activity * mt_unit_CO2,
    mt_co2_co2e = mt_co2,
    mt_ch4 = avg_activity * mt_unit_CH4,
    mt_ch4_co2e = mt_ch4 * 25,
    mt_n2o = avg_activity * mt_unit_N2O,
    mt_n2o_co2e = mt_n2o * 298
  ) %>%
  select(-c(
    fuel_methane_ch4_emissions_mt_co2e,
    fuel_nitrous_oxide_n2o_emissions_mt_co2e,
    per_unit, CH4, N2O, unit_ch4, unit_n2o,
    mt_unit_CO2, mt_unit_CH4, mt_unit_N2O
  )) %>%
  pivot_longer(
    cols = 10:15,
    names_to = "units_emissions",
    values_to = "values_emissions"
  )

unit_emissions <- ind_fuel_emissions %>%
  filter(grepl("co2e", units_emissions)) %>%
  group_by(
    reporting_year, facility_id, county_name, city_name, unit_name,
    general_fuel_type, corrected_fuel_type
  ) %>%
  summarize(mt_co2e = sum(values_emissions))

unit_emissions %>%
  filter(corrected_fuel_type == "Natural Gas") %>%
  group_by(reporting_year) %>%
  summarize(mt_co2e = sum(mt_co2e))

ind_fuel_emissions %>%
  filter(industry_type_subparts == "C") %>%
  filter(grepl("co2e", units_emissions)) %>%
  group_by(reporting_year) %>%
  summarize(mt_co2e = sum(values_emissions))

### save three forms of this data: activity, emissions by gas MT,
# emissions by gas MT co2e

ind_fuel_activity_out <- ind_fuel_activity %>%
  mutate(avg_activity = (unit_ch4 + unit_n2o) / 2) %>%
  select(facility_id,
    facility_name,
    industry_type_subparts,
    county_name,
    city_name,
    reporting_year,
    unit_name,
    general_fuel_type,
    specific_fuel_type = corrected_fuel_type,
    units_activity = per_unit,
    value_activity = avg_activity
  ) %>%
  ### add flag for likely doublecounts - will filter from emissions
  mutate(doublecount = if_else(
    general_fuel_type == "Natural Gas" &
      !grepl("Y", industry_type_subparts),
    "Yes", "No"
  ))

ind_fuel_activity_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "facility_id", class(ind_fuel_activity_out$facility_id), "Facility specific ID",
    "facility_name", class(ind_fuel_activity_out$facility_name), "Facility name",
    "industry_type_subparts", class(ind_fuel_activity_out$industry_type_subparts), "Sibpart code for industry type of facility",
    "county_name", class(ind_fuel_activity_out$county_name), "County name",
    "city_name", class(ind_fuel_activity_out$city_name), "City name",
    "reporting_year", class(ind_fuel_activity_out$reporting_year), "Year of activity",
    "unit_name", class(ind_fuel_activity_out$unit_name), "Name of combustion unit",
    "general_fuel_type", class(ind_fuel_activity_out$general_fuel_type), "General category of fuel combusted: Natural Gas, Petroleum, Coal, Other",
    "specific_fuel_type", class(ind_fuel_activity_out$specific_fuel_type), "Specific type of fuel combusted",
    "value_activity", class(ind_fuel_activity_out$value_activity), "Numerical value of activity data",
    "units_activity", class(ind_fuel_activity_out$units_activity), "Units of activity data",
    "doublecount", class(ind_fuel_activity_out$doublecount), "Whether activity is likely to be double counted in utility analysis"
  )

saveRDS(ind_fuel_activity_out, "./_industrial/data/fuel_combustion_activity.rds")
saveRDS(ind_fuel_activity_meta, "./_industrial/data/fuel_combustion_activity_meta.rds")

## fuel combustion by gas output

ind_fuel_gas_emissions_out <- ind_fuel_emissions %>%
  filter(!grepl("co2e", units_emissions)) %>%
  select(facility_id,
    facility_name,
    industry_type_subparts,
    county_name,
    city_name,
    reporting_year,
    unit_name,
    general_fuel_type,
    specific_fuel_type = corrected_fuel_type,
    units_emissions,
    values_emissions
  ) %>%
  ### add flag for likely doublecounts - will filter from emissions
  mutate(doublecount = if_else(
    general_fuel_type == "Natural Gas" &
      !grepl("Y", industry_type_subparts),
    "Yes", "No"
  ))

ind_fuel_gas_emissions_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "facility_id", class(ind_fuel_gas_emissions_out$facility_id), "Facility specific ID",
    "facility_name", class(ind_fuel_gas_emissions_out$facility_name), "Facility name",
    "industry_type_subparts", class(ind_fuel_gas_emissions_out$industry_type_subparts), "Sibpart code for industry type of facility",
    "county_name", class(ind_fuel_gas_emissions_out$county_name), "County name",
    "city_name", class(ind_fuel_gas_emissions_out$city_name), "City name",
    "reporting_year", class(ind_fuel_gas_emissions_out$reporting_year), "Year of activity",
    "unit_name", class(ind_fuel_gas_emissions_out$unit_name), "Name of combustion unit",
    "general_fuel_type", class(ind_fuel_gas_emissions_out$general_fuel_type), "General category of fuel combusted: Natural Gas, Petroleum, Coal, Other",
    "specific_fuel_type", class(ind_fuel_gas_emissions_out$specific_fuel_type), "Specific type of fuel combusted",
    "values_emissions", class(ind_fuel_gas_emissions_out$values_emissions), "Numerical value of emissions data",
    "units_emissions", class(ind_fuel_gas_emissions_out$units_emissions), "Units of emissions data",
    "doublecount", class(ind_fuel_activity_out$doublecount), "Whether activity is likely to be double counted in utility analysis"
  )

saveRDS(ind_fuel_gas_emissions_out, "./_industrial/data/fuel_combustion_emissions_by_gas.rds")
saveRDS(ind_fuel_gas_emissions_meta, "./_industrial/data/fuel_combustion_emissions_by_gas_meta.rds")

## fuel combustion by co2e


ind_fuel_co2e_emissions_out_doublecount <- ind_fuel_emissions %>%
  filter(
    grepl("co2e", units_emissions),
    !values_emissions == 0
  ) %>%
  group_by(
    facility_id,
    facility_name,
    industry_type_subparts,
    county_name,
    city_name,
    reporting_year,
    unit_name,
    general_fuel_type,
    corrected_fuel_type
  ) %>%
  summarize(values_emissions = sum(values_emissions)) %>%
  mutate(units_emissions = "Metric tons of CO2 equivalency") %>%
  rename(specific_fuel_type = corrected_fuel_type) %>%
  filter((general_fuel_type == "Natural Gas" &
    !grepl("Y", industry_type_subparts)))


ind_fuel_co2e_emissions_out <- ind_fuel_emissions %>%
  filter(
    grepl("co2e", units_emissions),
    !values_emissions == 0
  ) %>%
  group_by(
    facility_id,
    facility_name,
    industry_type_subparts,
    county_name,
    city_name,
    reporting_year,
    unit_name,
    general_fuel_type,
    corrected_fuel_type
  ) %>%
  summarize(values_emissions = sum(values_emissions)) %>%
  mutate(units_emissions = "Metric tons of CO2 equivalency") %>%
  rename(specific_fuel_type = corrected_fuel_type) %>%
  filter((general_fuel_type != "Natural Gas" |
    grepl("Y", industry_type_subparts)))

ind_fuel_co2e_emissions_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "facility_id", class(ind_fuel_co2e_emissions_out$facility_id), "Facility specific ID",
    "facility_name", class(ind_fuel_co2e_emissions_out$facility_name), "Facility name",
    "industry_type_subparts", class(ind_fuel_co2e_emissions_out$industry_type_subparts), "Subpart code for industry type of facility",
    "county_name", class(ind_fuel_co2e_emissions_out$county_name), "County name",
    "city_name", class(ind_fuel_co2e_emissions_out$city_name), "City name",
    "reporting_year", class(ind_fuel_co2e_emissions_out$reporting_year), "Year of activity",
    "unit_name", class(ind_fuel_co2e_emissions_out$unit_name), "Name of combustion unit",
    "general_fuel_type", class(ind_fuel_co2e_emissions_out$general_fuel_type), "General category of fuel combusted: Natural Gas, Petroleum, Coal, Other",
    "specific_fuel_type", class(ind_fuel_co2e_emissions_out$specific_fuel_type), "Specific type of fuel combusted",
    "values_emissions", class(ind_fuel_co2e_emissions_out$values_emissions), "Numerical value of emissions data",
    "units_emissions", class(ind_fuel_co2e_emissions_out$units_emissions), "Units of emissions data"
  )

saveRDS(ind_fuel_co2e_emissions_out, "./_industrial/data/fuel_combustion_emissions.rds")
saveRDS(ind_fuel_co2e_emissions_out_doublecount, "./_industrial/data/fuel_combustion_emissions_doublecount.rds")
saveRDS(ind_fuel_co2e_emissions_meta, "./_industrial/data/fuel_combustion_emissions_meta.rds")
