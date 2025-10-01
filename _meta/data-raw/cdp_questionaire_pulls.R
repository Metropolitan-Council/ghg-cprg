### CDP reporting script

county_emissions <- readRDS(file.path(here::here(), "_meta/data/cprg_county_emissions.RDS")) %>%
  filter(!county_name %in% c(
    "St. Croix",
    "Pierce",
    "Chisago",
    "Sherburne"
  ))

# set year for reporting
year_use <- 2022

# 3.1.1

# write_csv(county_emissions %>% filter(emissions_year <= 2022 & emissions_year >=2005),
#           paste0(wd,"/county_2022_inventory.csv"))

# 3.1.3

county_emissions %>%
  filter(
    emissions_year == year_use,
    sector == "Residential",
    category != "Electricity"
  ) %>%
  pull(value_emissions) %>%
  sum()

county_emissions %>%
  filter(
    emissions_year == year_use,
    sector == "Commercial",
    category != "Electricity"
  ) %>%
  pull(value_emissions) %>%
  sum()

county_emissions %>%
  filter(
    emissions_year == year_use,
    sector == "Industrial",
    category == "Building Fuel"
  ) %>%
  pull(value_emissions) %>%
  sum()

county_emissions %>%
  filter(
    emissions_year == year_use,
    category == "Building Fuel"
  ) %>%
  pull(value_emissions) %>%
  sum()

county_emissions %>%
  filter(
    emissions_year == year_use,
    source == "On-road"
  ) %>%
  pull(value_emissions) %>%
  sum()

county_emissions %>%
  filter(
    emissions_year == year_use,
    category == "Aviation"
  ) %>%
  pull(value_emissions)

county_emissions %>%
  filter(
    emissions_year == year_use,
    sector == "Transportation"
  ) %>%
  pull(value_emissions) %>%
  sum()

county_emissions %>%
  filter(
    emissions_year == year_use,
    category == "Solid waste"
  ) %>%
  group_by(source) %>%
  summarize(value_emissions = sum(value_emissions))

county_emissions %>%
  filter(
    emissions_year == year_use,
    category == "Wastewater"
  ) %>%
  pull(value_emissions) %>%
  sum()

county_emissions %>%
  filter(
    emissions_year == year_use,
    sector == "Waste"
  ) %>%
  pull(value_emissions) %>%
  sum()

county_emissions %>%
  filter(
    emissions_year == year_use,
    category == "Industrial processes"
  ) %>%
  pull(value_emissions) %>%
  sum()

county_emissions %>%
  filter(
    emissions_year == year_use,
    category == "Livestock"
  ) %>%
  pull(value_emissions) %>%
  sum()

county_emissions %>%
  filter(
    emissions_year == year_use,
    category == "Sequestration"
  ) %>%
  pull(value_emissions) %>%
  sum()

county_emissions %>%
  filter(
    emissions_year == year_use,
    category == "Cropland"
  ) %>%
  pull(value_emissions) %>%
  sum()

# 4.1

electric_county <- readRDS("_energy/data/minnesota_county_elec_ActivityAndEmissions.RDS") %>%
  filter(!county %in% c(
    "St. Croix",
    "Pierce",
    "Chisago",
    "Sherburne"
  ))

electric_county %>%
  group_by(year) %>%
  summarize(total_mwh = sum(total_mWh_delivered))

# mrow egrid 43.6% from renewables


# 6.6.1

county_emissions %>%
  filter(
    emissions_year == 2005,
    value_emissions > 0
  ) %>%
  pull(value_emissions) %>%
  sum()

county_emissions %>%
  filter(
    emissions_year == year_use,
    value_emissions > 0
  ) %>%
  pull(value_emissions) %>%
  sum()
