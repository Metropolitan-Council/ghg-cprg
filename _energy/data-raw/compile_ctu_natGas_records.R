### compile all ctu natural gas emissions

source("R/_load_pkgs.R")

## ctu and county data
cprg_ctu <- read_rds("_meta/data/cprg_ctu.RDS") %>%
  filter(!county_name %in% c("Chisago", "Sherburne", "St. Croix", "Pierce"))
cprg_county <- read_rds("_meta/data/cprg_county.RDS") %>%
  filter(!county_name %in% c("Chisago", "Sherburne", "St. Croix", "Pierce"))
ctu_population <- read_rds("_meta/data/ctu_population.RDS") %>%
  left_join(cprg_county %>% st_drop_geometry() %>% select(geoid, county_name)) %>%
  filter(!county_name %in% c("Chisago", "Sherburne", "St. Croix", "Pierce"))

# assign cities to counties where majority of population is
ctu_county_unique <- ctu_population %>%
  group_by(ctu_name, ctu_class) %>%
  filter(ctu_population == max(ctu_population)) %>%
  ungroup() %>%
  distinct(geoid, ctuid, ctu_name, ctu_class, county_name)

# county activity and emissions data
county_mcf <- readRDS(here("_energy", "data", "county_natgas_activitiy_emissions.RDS"))

## create storage frame of unique city and utility combos with all years
ctu_utility_year <- readRDS("_energy/data/ctu_ng_utility_intersect.RDS") %>%
  cross_join(data.frame(inventory_year = c(2007:2023))) %>%
  mutate(
    residential_mcf = NA,
    business_mcf = NA,
    total_mcf = NA
  ) %>%
  rename(utility = utility_name) %>%
  mutate(utility = case_when(
    utility == "CENTERPOINT ENERGY" ~ "CenterPoint Energy",
    utility == "NORTHERN STATES POWER CO" ~ "Xcel Energy",
    utility == "CIRCLE PINES UTILITY CO. (CENTENNIAL)" ~ "Centennial Utilities",
    utility == "MINNESOTA ENERGY RESOURCES" ~ "Minnesota Energy Resources",
    TRUE ~ utility
  ))

therms_to_mcf <- 1 / 10.38

## load formatted SQL utility data
sql_ng <- readRDS("_energy/data/ctu_ng_emissions_2015_2018.rds") %>%
  mutate(
    ctu_class = if_else(grepl("Twp.", ctu_name), "TOWNSHIP", "CITY"),
    ctu_name = str_replace_all(ctu_name, " Twp.", ""),
    ctu_name = str_replace_all(ctu_name, "St. ", "Saint "),
    ctu_class = if_else(ctu_name %in% c("Credit River", "Empire"),
      "CITY",
      ctu_class
    ),
    utility = data_source
  ) %>%
  filter(
    units_emissions == "Metric tons CO2",
    !is.na(therms_per_year)
  ) %>% # removes duplicates
  mutate(sector = case_when(customer_class == "Residential" ~ "Residential",
                            customer_class == "Total" ~ "Total",
    TRUE ~ "Business"
  )) %>%
  group_by(ctu_name, ctu_class, emissions_year, utility, sector) %>%
  summarise(
    mcf_per_year = sum(therms_per_year * therms_to_mcf, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = sector, values_from = mcf_per_year,
    names_glue = "{tolower(sector)}_mcf"
  ) %>%
  mutate(total_mcf = if_else(is.na(total_mcf),
                                   replace_na(business_mcf, 0) + replace_na(residential_mcf, 0),
                                   total_mcf)
  )


### load and format xcel ng data
xcel <- readRDS("_energy/data/Xcel_elecNG_activityData_2015_2023.rds") %>%
  filter(
    !is.na(mcf_delivered),
    source == "Natural Gas"
  ) %>%
  rename(emissions_year = year) %>%
  mutate(sector = case_when(
    sector_mapped == "residential" ~ "Residential",
    TRUE ~ "Business"
  )) %>%
  group_by(ctu_name, ctu_class, emissions_year, utility, sector) %>%
  summarise(mcf_per_year = sum(mcf_delivered, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = sector, values_from = mcf_per_year,
    names_glue = "{tolower(sector)}_mcf"
  ) %>%
  mutate(total_mcf = replace_na(business_mcf, 0) + replace_na(residential_mcf, 0))


### load and format CenterPoint ng data -- ensure that 'All' is kept for city-years without specific sector breakouts
centerpoint <- readRDS("_energy/data/centerpoint_activityData_2015_2023.rds") %>%
  filter(
    !is.na(mcf_delivered),
    source == "Natural Gas"
  ) %>%
  rename(emissions_year = year) %>%
  mutate(
    utility = "CenterPoint Energy",
    sector = case_when(
      sector == "Commercial/Industrial" ~ "Business",
      TRUE ~ sector # keeps Residential as Residential and All as All
    )
  ) %>%
  group_by(ctu_name, ctu_class, emissions_year, utility, sector) %>%
  summarise(mcf_per_year = sum(mcf_delivered, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = sector,
    values_from = mcf_per_year,
    names_glue = "{tolower(sector)}_mcf"
  ) %>%
  mutate(
    total_mcf = case_when(
      !is.na(business_mcf) & !is.na(residential_mcf) ~ business_mcf + residential_mcf,
      TRUE ~ all_mcf # fall back to all_mcf if detailed sectors are missing
    )
  ) %>%
  select(-all_mcf)


# check where we only have total numbers from Centerpoint and not sector specific
# centerpoint_city_year_with_only_totals <- centerpoint %>%
# group_by(ctu_name, emissions_year) %>%
#   summarize(
#     has_all = any(sector == "All"),
#     has_other = any(sector != "All"),
#     .groups = "drop"
#   ) %>%
#   filter(has_all, !has_other)

# no muni data load necessary -- the only NG muni in core metro operates in Circle Pines per overlap analysis, alongside Centerpoint, so effort to bring in nor warranted

# function: sequentially load data while keeping NAs
merge_ng_data <- function(base_df, new_data) {
  new_data_renamed <- new_data %>% rename(inventory_year = emissions_year)
  
  # rows already in base -- update NAs with new values
  updated <- base_df %>%
    left_join(new_data_renamed,
              by = c("ctu_name", "ctu_class", "inventory_year", "utility")
    ) %>%
    mutate(
      residential_mcf = if_else(!is.na(residential_mcf.y), residential_mcf.y, residential_mcf.x),
      business_mcf    = if_else(!is.na(business_mcf.y),    business_mcf.y,    business_mcf.x),
      total_mcf       = if_else(!is.na(total_mcf.y),       total_mcf.y,       total_mcf.x)
    ) %>%
    select(-ends_with(".x"), -ends_with(".y"))
  
  # rows in new_data that have no matching utility in the base scaffold
  novel_rows <- new_data_renamed %>%
    anti_join(base_df, by = c("ctu_name", "ctu_class", "inventory_year", "utility"))
  
  bind_rows(updated, novel_rows)
}

## load each dataset sequentially (deliberately override previous sql data with our data requests)

# make sure names conform
anti_join(sql_ng, ctu_utility_year, by = "utility") %>%
  distinct(utility) %>%
  arrange(utility)
sort(unique(ctu_utility_year$utility))


# perform the merges -- sql first to make sure any updated data from utilities is reflected
ctu_utility_year <- merge_ng_data(ctu_utility_year, sql_ng)
ctu_utility_year <- merge_ng_data(ctu_utility_year, centerpoint)
ctu_utility_year <- merge_ng_data(ctu_utility_year, xcel)


# Address clearly incongruent and poor-data data
ctu_utility_year <- ctu_utility_year %>%
  mutate(
    residential_mcf = if_else(ctu_name == "Dayton" & inventory_year == 2020, NA_real_, residential_mcf)
  )


### compare to rii totals and supplement where possible
# get a list of city-year combos we think are complete

ctu_year_complete <- ctu_utility_year %>%
  group_by(ctu_name, ctu_class, inventory_year) %>%
  filter(!any(is.na(total_mcf))) %>%
  summarize(total_mcf = sum(total_mcf)) %>%
  ungroup()
#


rii <- read_rds("_energy/data/rii_natGas_2007_2023.rds")

# merge with ctu_year_complete
rii_ctu_comp <- inner_join(ctu_year_complete,
  rii %>%
    group_by(ctu_name, ctu_class, year) %>%
    summarize(rii_mcf = sum(mcf_delivered)) %>%
    ungroup(),
  by = c("ctu_name", "ctu_class",
    "inventory_year" = "year"
  )
)

ggplot(data = rii_ctu_comp, aes(x = total_mcf, y = rii_mcf)) +
  geom_point() +
  geom_abline(slope = 1) +
  theme_bw()
## fairly tight, a few notable departure

rii_ctu_comp %>% filter(
  abs(total_mcf - rii_mcf) > 10000
)


### put RII data in for city-years without any utility data (skip partial sets)

rii_wide <- rii %>%
  mutate(sector_use = if_else(sector == "Residential",
    "Residential",
    "Business"
  )) %>%
  select(-sector) %>%
  pivot_wider(
    names_from = sector_use, values_from = mcf_delivered,
    names_glue = "{tolower(sector_use)}_mcf"
  ) %>%
  mutate(total_mcf = replace_na(business_mcf, 0) + replace_na(residential_mcf, 0))

# id cities with NO utility data

empty_city_years <- ctu_utility_year %>%
  group_by(ctu_name, ctu_class, inventory_year) %>%
  summarise(all_na = all(is.na(business_mcf) & is.na(residential_mcf) & is.na(total_mcf)), .groups = "drop") %>%
  filter(all_na) %>%
  select(ctu_name, ctu_class, inventory_year)

# pull out rii data matching above

rii_fill <- empty_city_years %>%
  left_join(rii_wide %>% rename(inventory_year = year),
    by = c("ctu_name", "ctu_class", "inventory_year")
  ) %>%
  select(ctu_name, ctu_class, inventory_year, utility, business_mcf, residential_mcf, total_mcf) %>%
  mutate(total_mcf = replace_na(business_mcf, 0) + replace_na(residential_mcf, 0)) %>%
  filter(!(is.na(business_mcf) | is.na(residential_mcf)))

ctu_utility_year <- ctu_utility_year %>%
  anti_join(rii_fill %>% select(ctu_name, ctu_class, inventory_year),
    by = c("ctu_name", "ctu_class", "inventory_year")
  ) %>%
  bind_rows(., rii_fill)


#check 
ctu_utility_year %>%
  filter(!is.na(total_mcf)) %>%
  count(utility, inventory_year) %>%
  arrange(desc(n))

# removal of powerplant data when suspected

fuel_combustion_activity <- read_rds("_industrial/data/fuel_combustion_activity.RDS")

### check: flag city-years where power plant gas consumption may inflate business_mcf

# conversion factor
scf_to_mcf <- 1 / 1000

# summarize GHGRP power plant NG consumption by city and year
powerplant_ng <- fuel_combustion_activity %>%
  filter(
    power_plant == TRUE,
    general_fuel_type == "Natural Gas",
    units_activity == "scf"
  ) %>%
  group_by(city_name, county_name, reporting_year) %>%
  summarise(
    powerplant_mcf = sum(value_activity * scf_to_mcf, na.rm = TRUE),
    facilities = paste(unique(facility_name), collapse = "; "),
    .groups = "drop"
  )

# join against city business totals -- fuzzy-ish match on city name
powerplant_check <- ctu_utility_year %>%
  filter(!is.na(business_mcf)) %>%
  group_by(ctu_name, ctu_class, inventory_year) %>%
  summarise(
    business_mcf = sum(business_mcf, na.rm = TRUE),
    total_mcf    = sum(total_mcf, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    powerplant_ng,
    by = c("ctu_name" = "city_name", "inventory_year" = "reporting_year")
  ) %>%
  filter(!is.na(powerplant_mcf)) %>%
  mutate(
    powerplant_share = powerplant_mcf / business_mcf,
    business_mcf_adj = business_mcf - powerplant_mcf,
    total_mcf_adj    = total_mcf    - powerplant_mcf
  ) %>%
  arrange(desc(powerplant_share))

# print flagged city-years for review
powerplant_check %>%
  select(
    ctu_name, inventory_year, facilities,
    business_mcf, powerplant_mcf, powerplant_share,
    business_mcf_adj, total_mcf_adj
  ) %>%
  print(n = 80)

# it seems like only Minneapolis in 2021-2023 has this data added

powerplant_adjustments <- powerplant_check %>%
  filter(
    ctu_name == "Minneapolis" & inventory_year %in% 2021:2023
  ) %>%
  select(ctu_name, ctu_class, inventory_year, powerplant_mcf)

ctu_utility_year <- ctu_utility_year %>%
  left_join(
    powerplant_adjustments,
    by = c("ctu_name", "ctu_class", "inventory_year")
  ) %>%
  mutate(
    business_mcf = if_else(!is.na(powerplant_mcf), business_mcf - powerplant_mcf, business_mcf),
    total_mcf    = if_else(!is.na(powerplant_mcf), total_mcf    - powerplant_mcf, total_mcf)
  ) %>%
  select(-powerplant_mcf)

## save output file

saveRDS(
  ctu_utility_year,
  "_energy/data/ctu_utility_mcf.RDS"
)
