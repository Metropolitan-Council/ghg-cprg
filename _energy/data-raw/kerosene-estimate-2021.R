source("R/_load_pkgs.R")
source("R/global_warming_potential.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
epa_ghg_factor_hub <- readRDS("_meta/data/epa_ghg_factor_hub.RDS")

kerosene_factors <- epa_ghg_factor_hub$stationary_combustion %>%
  filter(
    `Fuel type` == "Kerosene",
    per_unit == "mmBtu"
  )



kerosene_efficiency_grams <-
  # CO2 emissions per mmBtu of propane used, converted from kg to g
  kerosene_factors %>%
  filter(emission == "kg CO2") %>%
  magrittr::extract2("value") %>%
  units::as_units("kilogram") %>%
  units::set_units("gram") %>%
  as.numeric() +
  # methane emissions per mmBtu propane scale to CO2 equivalency
  (kerosene_factors %>% filter(emission == "g CH4") %>% magrittr::extract2("value") * gwp$ch4) +
  # n20 emissions per mmBtu propane scale to CO2 equivalency
  (kerosene_factors %>% filter(emission == "g N2O") %>% magrittr::extract2("value") * gwp$n2o)


# convert grams to kilograms
# this value rougly tracks with another source https://www.carbonsolutions.com/clients/CalculatorTALxAbout.html
kerosene_efficiency_kg <-
  kerosene_efficiency_grams %>%
  units::as_units("grams") %>%
  units::set_units("kilograms") %>%
  as.numeric()

# kerosene mmBtu generation estimates are not provided at the state level for MN and WI due to inadequate sample size.
# Best approximation I can see is regional usage rates, which may skew high
# source: https://www.eia.gov/consumption/residential/data/2020/state/pdf/ce2.1.st.pdf
eia2020 <- read.csv("_energy/data-raw/eia-recs-region-2020.csv")


# these are the estimated per household values of million btu generation for households that use kerosene or fuel oil.
# WI and MN are in different subregions of Midwest, so using different estimates for each of those
mn_kero_use <- as.numeric(eia2020[7, 12])
wi_kero_use <- as.numeric(eia2020[6, 12])

# ### look up codes in ACS
# load_variables(year = 2020, dataset = "acs5") %>%
#   mutate(concept_short = substr(concept, 1, 10)) %>%
#   distinct(concept_short) %>%
#   print(n = 10000)
#
# #### house heating fuel
# v_heat <- load_variables(year = 2021, dataset = "acs5") %>%
#   mutate(concept_short = substr(concept, 1, 10)) %>%
#   filter(concept_short == "HOUSE HEAT") %>%
#   print(n = 10000)

# get number of households in each county using propane
mn_kero_hh <- tidycensus::get_acs(
  geography = "county",
  variables = "B25040_005",
  state = "MN",
  year = 2021
) %>%
  filter(GEOID %in% cprg_county$GEOID) %>%
  rowwise() %>%
  mutate(
    # multiply average propane use by household be estimated number of households
    mmBtu = estimate * as.numeric(mn_kero_use),
    # multiply mmBtu per county by emissions factor, convert to metric tons
    CO2e = mmBtu * kerosene_efficiency_kg / 1000
  )

# repeat for WI
wi_kero_hh <- tidycensus::get_acs(
  geography = "county",
  variables = "B25040_005",
  state = "WI",
  year = 2021
) %>%
  filter(GEOID %in% cprg_county$GEOID) %>%
  rowwise() %>%
  mutate(
    # multiply average propane use by household be estimated number of households
    mmBtu = estimate * as.numeric(wi_kero_use),
    # multiply mmBtu per county by emissions factor and then convert to metric tonnes
    CO2e = mmBtu * kerosene_efficiency_kg / 1000
  )
# bind data
kero_county <- rows_append(mn_kero_hh, wi_kero_hh)
kero_county

total_regional_kerosene_emissions <- sum(kero_county$CO2e) # total regional emissions of the 11 county area
total_regional_kerosene_emissions

waldo::compare(kero_county, readRDS("_energy/data-raw/kerosene_use_county.RDS"))
saveRDS(kero_county, "_energy/data-raw/kerosene_use_county.RDS")
