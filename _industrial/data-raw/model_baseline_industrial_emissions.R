### model early industrial emissions based on 2011-2022 GHGRP data
### and MPCA 2005-2020 state inventoy

cprg_county <- readRDS("_meta/data/cprg_county.rds") %>%
  st_drop_geometry()
ctu_population <- readRDS("_meta/data/ctu_population.rds") %>%
  mutate(ctu_name = str_replace_all(ctu_name, "St.", "Saint"))

source("R/_load_pkgs.R")

# load in mpca inventory work
mpca_industrial_inv <- readRDS(file.path(here::here(), "_meta/data/mpca_ghg_inv_2005_2020.RDS")) %>%
  filter(Sector %in% c("Waste", "Industrial"))

mpca_commercial_inv <- readRDS(file.path(here::here(), "_meta/data/mpca_ghg_inv_2005_2020.RDS")) %>%
  filter(Sector %in% c("Commercial"))

ghgrp_emissions <- readRDS(file.path(
  here::here(),
  "_industrial/data/ghgrp_industrial_point_sources_ctu.rds"
)) %>%
  mutate(city_name = str_replace_all(city_name, "St.", "Saint"))

subpart_c_emissions <- readRDS(file.path(here::here(), "_industrial/data/fuel_combustion_emissions.RDS")) %>%
  mutate(city_name = str_replace_all(city_name, "St.", "Saint"))

mpca_emissions <- readRDS(file.path(here::here(), "_industrial/data/mpca_fuel_emissions.RDS")) %>%
  mutate(ctu_name = str_replace_all(ctu_name, "St.", "Saint"))

ghgrp_emissions_combustion <- bind_rows(
  ghgrp_emissions %>% ungroup() %>%
    filter(
      source != "stationary_combustion",
      doublecount == "No"
    ) %>%
    select(
      inventory_year, facility_name, city_name, county_name, doublecount,
      value_emissions, category, source
    ),
  subpart_c_emissions %>%
    mutate(
      category = "fuel_combustion",
      source = if_else(general_fuel_type == "Other",
        specific_fuel_type, general_fuel_type
      )
    ) %>%
    ungroup() %>%
    select(
      inventory_year = reporting_year, facility_name, city_name, county_name,
      value_emissions = values_emissions,
      category, source
    )
) %>%
  mutate(city_name = str_to_title(city_name))

# problem with st paul refinery having diminished subpart c 2011 emissions
# not reflected in ghgrp


sppr_ratios <- left_join(
  ghgrp_emissions %>% filter( facility_name == "St. Paul Park Refining Company, LLC",
                              category == "stationary_combustion") %>% 
    group_by(inventory_year) %>% summarize(value_emissions_ghgrp = sum(value_emissions)) %>% 
    ungroup(),
  subpart_c_emissions %>% filter(facility_name == "St. Paul Park Refining Company, LLC") %>% 
    group_by(reporting_year) %>% summarize(value_emissions_subc = sum(values_emissions)) %>% 
    ungroup() %>% rename(inventory_year = reporting_year),
) %>% 
  mutate(ratio = value_emissions_subc / value_emissions_ghgrp)

# # A tibble: 13 × 4
# inventory_year value_emissions_ghgrp value_emissions_subc ratio
# <dbl>                 <dbl>                <dbl> <dbl>
#   1           2011               444753.              116524. 0.262
# 2           2012               445394.              481500. 1.08 
# 3           2013               414426.              447932. 1.08 
# 4           2014               545864.              597114. 1.09 
# 5           2015               534725.              585310. 1.09 
# 6           2016               506016.              560484. 1.11 
# 7           2017               523682.              593695. 1.13 
# 8           2018               541573.              588930. 1.09 
# 9           2019               581011.              580364. 0.999
# 10           2020               557969.              592715. 1.06 
# 11           2021               568676.              608100. 1.07 
# 12           2022               515393.              557448. 1.08 
# 13           2023               575766.              613922. 1.07 

# hold 2011 value
sppr_2011 <- sppr_ratios %>% filter(inventory_year == 2011) %>% pull(value_emissions_ghgrp) *
  sppr_ratios %>% filter(inventory_year != 2011) %>% pull(ratio) %>% mean()

### match ghgrp emission categories to mpca subsectors as much as possible
sort(unique(mpca_industrial_inv$Subsector))
sort(unique(ghgrp_emissions_combustion$source))

# refinery is a constant problem in these comparisons
subpart_c_emissions %>%
  filter(
    facility_name == "Flint Hills Resources Pine Bend Refinery",
    reporting_year == 2020
  ) %>%
  ungroup() %>%
  select(unit_name, general_fuel_type, values_emissions)

ghgrp_emissions %>%
  filter(
    facility_name == "Flint Hills Resources Pine Bend Refinery",
    inventory_year == 2020
  ) %>%
  ungroup() %>%
  select(category, source, value_emissions)

mpca_industrial_inv %>%
  filter(year == 2020, co2e > 0) %>%
  print(n = 50)

### create six categories - ind process, ref process, nat gas, oil, coal, other fuel combustion
ghgrp_simplified <- ghgrp_emissions_combustion %>%
  mutate(mpca_subsector = case_when(
    source %in% c(
      "fluorinated_ghg_production",
      "glass_production",
      "iron_and_steel_production",
      "lead_production",
      "magnesium_production",
      "electronics_manufacture",
      "petroleum_and_natural_gas_systems_transmission_compression"
    ) ~
      "Industrial processes",
    source %in% c("Fuel Gas", "Natural Gas") ~ "Natural gas", # seems likely to be grouped based on descriptions and magnitude of emissions
    source %in% c(
      "hydrogen_production", # large emissions, only happens in refineries, best guess here for matching MPCA
      "petroleum_refining"
    ) ~ "Refinery processes",
    source == "Petroleum Products" ~ "Oil",
    source %in% c("Agricultural Byproducts", "Wood and Wood Residuals") ~ "Other fuel combustion", # best worst option?
    source == "industrial_waste_landfills" ~ "Landfills", # MPCA technical confirms industrial waste emission is in this category
    TRUE ~ source
  )) %>%
  filter(mpca_subsector != "Municipal Solid Waste") %>%
  group_by(inventory_year, city_name, county_name, mpca_subsector) %>%
  summarize(value_emissions = sum(value_emissions)) %>%
  mutate(data_source = "GHGRP") %>%
  ungroup() %>% 
  ## replace st paul park natural gas with correct value from earlier
  mutate(value_emissions = if_else(city_name == "Saint Paul Park" &
                                     inventory_year == 2011 &
                                     mpca_subsector == "Natural gas",
                                   sppr_2011,
                                   value_emissions))

### Now add in MPCA data for cities without industrial emissions in GHGRP
### Later we need to look for industrial point sources in MPCA missed in GHGRP cities
### but this is easier said than done :/

mpca_industrial_missing <- mpca_emissions %>%
  filter(
    sector == "Industrial",
    !ctu_name %in% ghgrp_simplified$city_name,
    fuel_type != "Natural Gas"
  ) %>%
  mutate(mpca_subsector = case_when(
    fuel_category %in% c(
      "Natural Gas",
      "Other Fuels - Gaseous"
    ) ~ "Natural gas",
    fuel_category == "Coal and Coke" ~ "Coal",
    fuel_category == "Petroleum Products" ~ "Oil",
    TRUE ~ "Other fuel combustion"
  )) %>%
  group_by(inventory_year, county_name, ctu_name, mpca_subsector) %>%
  summarize(value_emissions = sum(value_emissions)) %>%
  mutate(data_source = "MPCA Fuel") %>%
  rename(city_name = ctu_name)

# bind these two data sources of measured emissions
industrial_emissions_measured <- bind_rows(
  ghgrp_simplified, mpca_industrial_missing
)

## taking a slightly different approach with MPCA, leaving those with non-obvious GHGRP correlates untransformed
mpca_inv_simplified <- mpca_industrial_inv %>%
  mutate(mpca_subsector = case_when(
    Subsector %in% c(
      "Industrial processes",
      "Glass manufacture",
      "Steel production",
      "Secondary lead production",
      "Magnesium casting",
      "Semiconductor manufacture"
    ) ~ "Industrial processes",
    Subsector %in% c(
      "Refinery processes",
      "Oil refining"
    ) ~ "Refinery processes",
    Subsector %in% c("Other fossil fuels") ~ "Other fuel combustion", # best worst option?
    TRUE ~ Subsector
  )) %>%
  group_by(year, mpca_subsector) %>%
  summarize(value_emissions_mpca_inv = sum(co2e))


ghgrp_mpca_emissions <-
  left_join(industrial_emissions_measured, mpca_inv_simplified,
    by = c(
      "inventory_year" = "year",
      "mpca_subsector"
    )
  ) %>%
  mutate(
    emission_percent = value_emissions / value_emissions_mpca_inv,
    emission_percent = if_else(is.infinite(emission_percent), NA, emission_percent)
  )

### create grid of needed city-subsector-year combinations
ghgrp_extrapolated <- expand.grid(
  inventory_year = seq(2005, 2020, by = 1),
  city_name = unique(ghgrp_mpca_emissions$city_name),
  county_name = unique(ghgrp_mpca_emissions$county_name),
  mpca_subsector = unique(ghgrp_mpca_emissions$mpca_subsector)
) %>%
  # filter to combinations found in inventory
  semi_join(., ghgrp_mpca_emissions %>%
    ungroup() %>%
    distinct(city_name, county_name, mpca_subsector)) %>%
  # bring in inventory data
  left_join(
    ghgrp_mpca_emissions %>%
      ungroup() %>%
      select(inventory_year, city_name, county_name, mpca_subsector, value_emissions, emission_percent),
    by = c("inventory_year", "city_name", "county_name", "mpca_subsector")
  ) %>%
  ### use na.kalman to extrapolate across time-series
  group_by(city_name, county_name, mpca_subsector) %>%
  arrange(inventory_year) %>%
  # first add zeros to 2011-2020 years if there aren't enough years(3+) for extrapolation
  mutate(non_na_count_2011_2020 = sum(!is.na(emission_percent) & inventory_year >= 2011 & inventory_year <= 2020, na.rm = TRUE)) %>%
  # Set NA to zero only if there are 1 or 2 years of data in the range
  mutate(emission_percent = if_else(
    is.na(emission_percent) & inventory_year >= 2011 & inventory_year <= 2020 & non_na_count_2011_2020 <= 2,
    0,
    emission_percent
  )) %>%
  # extrapolate
  mutate(
    emission_percent = na_kalman(emission_percent),
    data_type = ifelse(is.na(value_emissions), "modeled", "measured") # marking whether values are from the census or interpolation
  ) %>%
  ### bring mn state emissions back in
  left_join(., mpca_inv_simplified,
    by = c(
      "inventory_year" = "year",
      "mpca_subsector"
    )
  ) %>%
  # recalculate emissions based on percent of MPCA inventory
  mutate(value_emission_percentile = emission_percent * value_emissions_mpca_inv)

ghgrp_extrapolated_county <- ghgrp_extrapolated %>%
  group_by(inventory_year, county_name) %>%
  summarize(value_emissions = sum(value_emission_percentile))

ggplot(ghgrp_extrapolated_county, aes(x = inventory_year, y = value_emissions, col = county_name)) +
  geom_line()


### Now add in MPCA data for commercial sector

mpca_commercial <- mpca_emissions %>%
  filter(sector == "Commercial") %>%
  mutate(mpca_subsector = case_when(
    fuel_category %in% c(
      "Natural gas",
      "Other Fuels - Gaseous"
    ) ~ "Natural Gas",
    fuel_category == "Coal and Coke" ~ "Coal",
    fuel_category == "Petroleum Products" ~ "Oil",
    TRUE ~ "Other fossil fuel"
  )) %>%
  group_by(inventory_year, county_name, ctu_name, mpca_subsector) %>%
  summarize(value_emissions = sum(value_emissions)) %>%
  rename(city_name = ctu_name)

mpca_comm_simplied <- mpca_commercial_inv %>%
  mutate(mpca_subsector = case_when(
    Subsector %in% c("Other fossil fuels") ~ "Other fuel combustion", # best worst option?
    TRUE ~ Subsector
  )) %>%
  group_by(year, mpca_subsector) %>%
  summarize(value_emissions_mpca_inv = sum(co2e))

mpca_commercial_emissions <-
  left_join(mpca_commercial, mpca_comm_simplied,
    by = c(
      "inventory_year" = "year",
      "mpca_subsector"
    )
  ) %>%
  mutate(
    emission_percent = value_emissions / value_emissions_mpca_inv,
    emission_percent = if_else(is.infinite(emission_percent), NA, emission_percent)
  )

comm_extrapolated <- left_join(
  expand.grid(
    inventory_year = seq(2005, 2020, by = 1),
    city_name = unique(mpca_commercial$city_name),
    county_name = unique(mpca_commercial$county_name),
    mpca_subsector = unique(mpca_commercial$mpca_subsector)
  ) %>%
    semi_join(., mpca_commercial_emissions %>%
      ungroup() %>%
      distinct(city_name, county_name, mpca_subsector)),
  mpca_commercial_emissions %>%
    ungroup() %>%
    select(inventory_year, city_name, county_name, mpca_subsector, value_emissions, emission_percent),
  by = c("inventory_year", "city_name", "county_name", "mpca_subsector")
) %>%
  ### use na.kalman to extrapolate across time-series
  group_by(city_name, county_name, mpca_subsector) %>%
  arrange(inventory_year) %>%
  # first add zeros to 2011-2020 years if there aren't enough years(3+) for extrapolation
  mutate(non_na_count_2011_2020 = sum(!is.na(emission_percent) & inventory_year >= 2011 & inventory_year <= 2020, na.rm = TRUE)) %>%
  # Set NA to zero only if there are 1 or 2 years of data in the range
  mutate(emission_percent = if_else(
    is.na(emission_percent) & inventory_year >= 2011 & inventory_year <= 2020 & non_na_count_2011_2020 <= 2,
    0,
    emission_percent
  )) %>%
  # extrapolate
  mutate(
    emission_percent = na_kalman(emission_percent),
    data_type = ifelse(is.na(value_emissions), "modeled", "measured") # marking whether values are from the census or interpolation
  ) %>%
  ### bring mn state emissions back in
  left_join(., mpca_comm_simplied,
    by = c(
      "inventory_year" = "year",
      "mpca_subsector"
    )
  ) %>%
  # recalculate emissions based on percent of MPCA inventory
  mutate(value_emission_percentile = emission_percent * value_emissions_mpca_inv)

commercial_extrapolated_county <- comm_extrapolated %>%
  group_by(inventory_year, county_name) %>%
  summarize(value_emissions = sum(value_emission_percentile))

ggplot(commercial_extrapolated_county, aes(x = inventory_year, y = value_emissions, col = county_name)) +
  geom_line()

# combine and package

industrial_baseline <- bind_rows(
  ghgrp_extrapolated %>%
    select(inventory_year, city_name, county_name,
      source = mpca_subsector,
      data_type, value_emissions = value_emission_percentile
    ) %>%
    mutate(
      sector = "Industrial",
      category = case_when(
        source == "Refinery processes" ~ "Refinery processes",
        source %in% c("Landfills", "Industrial processes") ~ "Industrial processes",
        TRUE ~ "Stationary combustion"
      ),
      unit_emissions = "Metric tons CO2 equivalency",
      data_source = if_else(city_name %in% mpca_industrial_missing$city_name,
        "MPCA Reporting",
        "EPA GHG Reporting Program"
      ),
      data_source = if_else(data_type == "modeled", "Modeled data", data_source),
      factor_source = "EPA GHG Factor Hub"
    ),
  comm_extrapolated %>%
    select(inventory_year, city_name, county_name,
      source = mpca_subsector,
      data_type, value_emissions = value_emission_percentile
    ) %>%
    mutate(
      sector = "Commercial",
      category = "Stationary combustion",
      unit_emissions = "Metric tons CO2 equivalency",
      data_source = if_else(data_type == "modeled", "Modeled data", "MPCA Reporting"),
      factor_source = "EPA GHG Factor Hub"
    ),
  # add in 2021+ years
  mpca_commercial %>%
    filter(inventory_year >= 2021) %>%
    select(inventory_year, city_name, county_name,
      source = mpca_subsector,
      value_emissions
    ) %>%
    mutate(
      sector = "Commercial",
      category = "Stationary combustion",
      unit_emissions = "Metric tons CO2 equivalency",
      data_source = "MPCA Reporting",
      factor_source = "EPA GHG Factor Hub",
      data_type = NA
    ),
  industrial_emissions_measured %>%
    filter(inventory_year >= 2021) %>%
    select(inventory_year, city_name, county_name,
      source = mpca_subsector,
      value_emissions
    ) %>%
    mutate(
      sector = "Industrial",
      category = case_when(
        source == "Refinery processes" ~ "Refinery processes",
        source %in% c("Landfills", "Industrial processes") ~ "Industrial processes",
        TRUE ~ "Stationary combustion"
      ),
      unit_emissions = "Metric tons CO2 equivalency",
      factor_source = "EPA GHG Factor Hub",
      data_type = NA
    )
) %>%
  # bring in IDs
  left_join(cprg_county %>% select(county_name, geoid) %>% rename(county_id = geoid)) %>%
  select(-data_type)

baseline_county <- industrial_baseline %>%
  group_by(inventory_year, sector, county_name) %>%
  summarize(value_emissions = sum(value_emissions))

ggplot(baseline_county, aes(x = inventory_year, y = value_emissions, col = county_name)) +
  geom_line() +
  facet_wrap(~sector)

# one dataset is missing 2023, omitting for now

industrial_baseline <- industrial_baseline %>%
  filter(inventory_year <= 2022)

industrial_baseline_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "inventory_year", class(industrial_baseline$inventory_year), "Year of activity",
    "city_name", class(industrial_baseline$city_name), "City name",
    "county_name", class(industrial_baseline$county_name), "County name",
    "county_id", class(industrial_baseline$county_id), "County ID",
    "sector", class(industrial_baseline$sector), "Emissions sector",
    "category", class(industrial_baseline$category), "Emissions category",
    "source", class(industrial_baseline$source), "Emissions source",
    "category", class(industrial_baseline$category), "Emissions category",
    "value_emissions", class(industrial_baseline$value_emissions), "Numerical value of emissions data",
    "unit_emissions", class(industrial_baseline$unit_emissions), "Units of emissions data",
    "data_source", class(industrial_baseline$data_source), "Source of activity/emission data",
    "factor_source", class(industrial_baseline$factor_source), "Source of emission factor"
  )

saveRDS(industrial_baseline, "./_industrial/data/modeled_industrial_baseline_emissions.rds")
saveRDS(industrial_baseline_meta, "./_industrial/data/modeled_industrial_baseline_emissions_meta.rds")

# deprecated code to match subsectors more finely (lots of numerical issues)
# ghgrp_mpca_emissions <- ghgrp_emissions_combustion %>%
#   mutate(mpca_subsector = case_when(
#     source %in% c("Agricultural Byproducts",
#                   "fluorinated_ghg_production") ~ "Industrial processes",
#     source == "Fuel Gas" ~ "Natural Gas", #seems likely to be grouped based on descriptions and magnitude of emissions
#     source == "glass_production" ~ "Glass manufacture",
#     source == "hydrogen_production" ~ "Refinery processes", #large emissions, only happens in refineries, best guess here
#     source == "iron_and_steel_production" ~ "Steel production",
#     source == "lead_production" ~ "Secondary lead production",
#     source == "magnesium_production" ~ "Magnesium casting",
#     source == "Petroleum Products" ~ "Oil",
#     source == "petroleum_refining" ~ "Oil refining",
#     source == "Wood and Wood Residuals" ~ "Other fossil fuels", #best worst option?
#     source == "industrial_waste_landfills" ~ "Landfills", #MPCA technical confirms industrial waste emission is in this category
#     source == "electronics_manufacture" ~ "Semiconductor manufacture",
#     TRUE ~ source
#   )) %>%
#   group_by(inventory_year, city_name, county_name, mpca_subsector) %>%
#   summarize(value_emissions_ghgrp = sum(value_emissions)) %>%
#   right_join(mpca_industrial_inv,
#              by = c("inventory_year" = "year",
#                     "mpca_subsector" = "Subsector")) %>%
#   mutate(emission_percent = value_emissions_ghgrp/co2e,
#          emission_percent = if_else(is.infinite(emission_percent),NA, emission_percent))
#
