# This script will develop an expected housing profile for the region for use
# with
source("R/_load_pkgs.R")
#### residential building emissions estimate rely on four building categories
#### will use urbansim estimates for seven county
#  ACS estimates for surrounding counties combined with TAZ forecast data

# load in forecast data

urbansim <- read_rds("_meta/data/urbansim_data.RDS")
taz_forecast <- read_rds("_meta/data/demographic_forecast_11_county.RDS")

# get ACS data

source("R/_load_pkgs.R")
library(tidycensus)


# desired variables
housing_vars <- c(
  "B25024_002", # 1-unit, detached
  "B25024_003", # 1-unit, attached
  "B25024_004", # 2 units
  "B25024_005", # 3-4 units
  "B25024_006", # 5-9 units
  "B25024_007", # 10-19 units
  "B25024_008", # 20-49 units
  "B25024_009", # 50+ units
  "B25024_010" # Mobile home
)

mn_housing <- get_acs(
  geography = "county",
  variables = housing_vars,
  state = "MN",
  county = c("Chisago", "Sherburne"),
  survey = "acs5",
  year = 2022
) %>%
  clean_names()


wi_housing <- get_acs(
  geography = "county",
  variables = housing_vars,
  state = "WI",
  county = c("St. Croix", "Pierce"),
  survey = "acs5",
  year = 2022
) %>%
  clean_names()

## get 2010 data

mn_housing_2010 <- get_acs(
  geography = "county",
  variables = housing_vars,
  state = "MN",
  county = c("Chisago", "Sherburne"),
  year = 2010
) %>%
  clean_names()

wi_housing_2010 <- get_acs(
  geography = "county",
  variables = housing_vars,
  state = "WI",
  county = c("St. Croix", "Pierce"),
  year = 2010
) %>%
  clean_names()

housing_type <- bind_rows(mn_housing, wi_housing) %>%
  mutate(variable = recode(variable,
    "B25024_002" = "single_family_detached",
    "B25024_003" = "single_family_attached",
    "B25024_004" = "single_family_attached",
    "B25024_005" = "multifamily_units",
    "B25024_006" = "multifamily_units",
    "B25024_007" = "multifamily_units",
    "B25024_008" = "multifamily_units",
    "B25024_009" = "multifamily_units",
    "B25024_010" = "manufactured_homes"
  )) %>%
  group_by(geoid, variable) %>%
  summarize(value = sum(estimate), .groups = "keep") %>%
  ungroup() %>%
  mutate(inventory_year = 2022)

# housing_type

housing_type_2010 <- bind_rows(mn_housing_2010, wi_housing_2010) %>%
  mutate(variable = recode(variable,
    "B25024_002" = "single_family_detached",
    "B25024_003" = "single_family_attached",
    "B25024_004" = "single_family_attached",
    "B25024_005" = "multifamily_units",
    "B25024_006" = "multifamily_units",
    "B25024_007" = "multifamily_units",
    "B25024_008" = "multifamily_units",
    "B25024_009" = "multifamily_units",
    "B25024_010" = "manufactured_homes"
  )) %>%
  group_by(geoid, variable) %>%
  summarize(value = sum(estimate), .groups = "keep") %>%
  ungroup() %>%
  mutate(inventory_year = 2010)


### calculate percentage of each housing type per county in 2022

housing_percent <- housing_type %>%
  group_by(geoid) %>%
  mutate(total_households = sum(value)) %>%
  ungroup() %>%
  mutate(housing_percent = value / total_households) %>%
  select(geoid, housing_type = variable, housing_percent)

taz_acs_forecast <- left_join(
  taz_forecast %>%
    filter(variable == "total_households"),
  housing_percent,
  by = "geoid"
) %>%
  filter(!is.na(housing_percent)) %>%
  mutate(dwelling_number = housing_percent * value)

# now compile housing from urbansim

urbansim_housing_county <- urbansim %>%
  filter(!is.na(coctu_id_gnis)) %>%
  mutate(
    housing_type = case_when(
      variable %in% c(
        "manufactured_homes"
      ) ~ "manufactured_homes",
      variable %in% c(
        "single_fam_det_sl_own",
        "single_fam_det_rent",
        "single_fam_det_ll_own"
      ) ~ "single_family_detached",
      variable %in% c(
        "single_fam_attached_own",
        "single_fam_attached_rent"
      ) ~ "single_family_attached",
      variable %in% c(
        "multi_fam_own",
        "multi_fam_rent"
      ) ~ "multifamily_units"
    ),
    geoid = paste0(
      "27",
      substr(coctu_id_gnis, 1, 3)
    )
  ) %>%
  filter(!is.na(housing_type)) %>%
  group_by(housing_type, geoid, inventory_year) %>%
  summarize(value = sum(value), .groups = "keep")


## bind data sources

eleven_county_housing_forecast <- bind_rows(
  taz_acs_forecast %>%
    select(inventory_year, geoid, value = dwelling_number, housing_type) %>%
    filter(inventory_year %in% c(2020, 2030, 2040, 2050)),
  housing_type_2010 %>%
    rename(housing_type = variable),
  urbansim_housing_county %>%
    filter(inventory_year %in% c(2010, 2020, 2030, 2040, 2050))
) %>%
  rename(sp_categories = housing_type) %>%
  # Make sure all year/category/geoid combinations exist
  group_by(geoid, sp_categories) %>%
  complete(inventory_year = full_seq(inventory_year, 1)) %>%
  arrange(geoid, sp_categories, inventory_year) %>%
  # Interpolate missing values linearly
  mutate(value = zoo::na.approx(value, x = inventory_year, na.rm = FALSE)) %>%
  ungroup()



## sum to region and output

regional_housing_forecast <- eleven_county_housing_forecast %>%
  group_by(inventory_year, sp_categories) %>%
  summarize(value = sum(value), .groups = "keep") %>%
  ungroup() %>%
  mutate(
    geog_name = "CCAP Region",
    value_change_from_base = value - value[inventory_year == 2020]
  )

# waldo::compare(regional_housing_forecast, readRDS("_meta/data/regional_housing_forecast.RDS"))

message("Saving regional housing projections data to: \n\t _meta/data/regional_housing_forecast.RDS")
write_rds(
  regional_housing_forecast,
  "_meta/data/regional_housing_forecast.RDS"
)


message("Finished regional housing projections")
