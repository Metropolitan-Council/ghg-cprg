# Compile summary values for quantifying how many VMT we can reduce
source("R/_load_pkgs.R")
source("R/cprg_colors.R")
source("_meta/data-raw/projections/interpolate_emissions.R")

source("_meta/data-raw/ctu_coctu_index.R")
rtdm_forecast_county <- readRDS("_transportation/data/rtdm_forecast_county.RDS")
rtdm_emissions_forecast <- readRDS("_transportation/data/rtdm_county_emissions_forecast.RDS")
co_pop <- read_rds("_meta/data/census_county_population.RDS") %>%
  filter(cprg_area == TRUE)

demographic_forecast_11_county <- readRDS("_meta/data/demographic_forecast_11_county.RDS")

vmt_reduction_pct <- vmt_reduction_pct

# we need to get to 20% vmt reduction per person
# how many VMT is that?

co_vmt <- readRDS("_transportation/data/dot_vmt.RDS") %>%
  filter(
    cprg_area == TRUE
  )

# find the 2019 VMT per person, daily and annual
pop_reduction <- co_pop %>%
  left_join(co_vmt,
    by = join_by(
      geoid, county_name, cprg_area,
      population_year == vmt_year
    )
  ) %>%
  filter(population_year == 2019) %>%
  group_by(population_year, cprg_area) %>%
  summarize(
    annual_vmt = sum(annual_vmt),
    daily_vmt = sum(daily_vmt),
    population = sum(population)
  ) %>%
  mutate(
    vmt_per_person = annual_vmt / population,
    vmt_per_person_daily = daily_vmt / population,
    reduction_amount = vmt_per_person * vmt_reduction_pct,
    reduction_amount_daily = vmt_per_person_daily * vmt_reduction_pct,
    vmt_per_person_reduction = vmt_per_person - (reduction_amount),
    vmt_per_person_daily_reduction = vmt_per_person_daily - (reduction_amount_daily)
  )


# find the forecast VMT per person, daily and annual
vmt_per_person_forecast <-
  rtdm_forecast_county %>%
  left_join(cprg_county) %>%
  filter(cprg_area == TRUE) %>%
  group_by(vmt_year) %>%
  summarise(
    network_passenger_annual = sum(network_passenger_annual),
    network_truck_annual = sum(network_truck_annual),
    network_vmt_annual = sum(network_vmt_annual),
    network_vmt = sum(network_vmt)
  ) %>%
  mutate(inventory_year = as.numeric(vmt_year)) %>%
  left_join(
    demographic_forecast_11_county %>%
      filter(variable == "total_pop") %>%
      group_by(inventory_year, variable) %>%
      summarize(value = sum(value)) %>%
      mutate(inventory_year = case_when(
        inventory_year == 2022 ~ 2023,
        TRUE ~ inventory_year
      )),
    by = c("inventory_year")
  ) %>%
  mutate(
    vmt_per_person = network_vmt / value,
    vmt_per_person_annual = network_vmt_annual / value,
    vmt_per_person_reduction = vmt_per_person_annual - (vmt_per_person_annual * vmt_reduction_pct)
  )


# pull individual values
baseyear_annual_per_person <- vmt_per_person_forecast %>%
  filter(vmt_year == min(vmt_year)) %>%
  pull(vmt_per_person_annual)

baseyear_daily_per_person <- vmt_per_person_forecast %>%
  filter(vmt_year == min(vmt_year)) %>%
  pull(vmt_per_person)

forecast_population <- vmt_per_person_forecast %>%
  filter(vmt_year == max(vmt_year)) %>%
  pull(value)

forecast_annual_vmt <- vmt_per_person_forecast %>%
  filter(vmt_year == max(vmt_year)) %>%
  pull(network_vmt_annual)

# our current VMT reduction level based on the RTDM base year to 2050
# is 3% (Imagine 2050, evaluation and performance chapter)
# Our  annual VMT per person for base year is 7798.195
# a 20% reduction would be
vmt_target_per_person <- baseyear_annual_per_person * 0.8
vmt_target_per_person_daily <- baseyear_daily_per_person * 0.8

# total vmt at vmt target
vmt_target <- forecast_population * vmt_target_per_person

# total vmt at baseline forecast
# this corresponds to about
(vmt_target - forecast_annual_vmt) / forecast_annual_vmt
# decrease in total annual regional VMT
#
#
