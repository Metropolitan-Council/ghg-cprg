# compile ctu emissions for transportation, prep for compile_ctu_emissions read in
source("R/_load_pkgs.R")
source("_meta/data-raw/ctu_coctu_index.R")

# fetch gap-filled/modeled ctu level data from 2010 to 2022
mndot_vmt_ctu_gap_filled <- readRDS("_transportation/data/mndot_vmt_ctu_gap_filled.RDS") %>%
  filter(inventory_year < 2023)

# fetch mndot vmt by county, 2002-2022
mndot_vmt_county <- readRDS("_transportation/data-raw/mndot/mndot_vmt_county.RDS") %>%
  mutate(
    vmt_year = as.numeric(year),
    county_daily_vmt = daily_vmt,
    county_name = county
  ) %>%
  left_join(
    ctu_coctu_index %>%
      select(geoid, county_name) %>%
      unique(),
    by = c("county_name")
  ) %>%
  # remove Wright, Sherburne, WI counties
  filter(!is.na(geoid))

# find the percentage of county VMT each CTU makes up
# 2010-2022
ctu_vmt_percent <- (mndot_vmt_ctu_gap_filled) %>%
  left_join(mndot_vmt_county, by = c("geoid", "inventory_year" = "vmt_year")) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    ctu_vmt_percent = final_city_vmt / county_daily_vmt,
    vmt_year = as.numeric(inventory_year)
  ) %>%
  select(
    inventory_year, geoid, county_name, gnis,
    coctu_id_gnis, final_vmt_source, ctu_vmt_percent
  ) %>%
  unique()

# get the ctu percentage of county VMT for year 2010
ctu_vmt_pct_2010 <- ctu_vmt_percent %>%
  filter(inventory_year == "2010") %>%
  mutate(ctu_vmt_percent10 = ctu_vmt_percent) %>%
  select(coctu_id_gnis, geoid, gnis, county_name, ctu_vmt_percent10) %>%
  unique()

ctu_county_year_pct_index <- mndot_vmt_county %>%
  select(vmt_year, geoid, county_name = county, daily_vmt) %>%
  unique() %>%
  # get mapping of CTUs to counties
  left_join(
    ctu_coctu_index %>%
      select(-imagine_designation, -ctu_name_full, -ctu_name_full_county) %>%
      unique(),
    relationship = "many-to-many",
    by = join_by(geoid, county_name)
  ) %>%
  # get CTU VMT percent from 2010-2022
  left_join(ctu_vmt_percent,
    by = join_by(county_name, geoid, gnis, coctu_id_gnis, vmt_year == inventory_year)
  ) %>%
  unique() %>%
  # join with 2010 CTU VMT percent
  left_join(ctu_vmt_pct_2010,
    by = join_by(geoid, county_name, gnis, coctu_id_gnis)
  ) %>%
  # for years 2002-2010, use 2010 CTU VMT percentage
  # for years 2010-2022, use actual CTU proportion of county VMT
  mutate(
    ctu_vmt_percent = ifelse(is.na(ctu_vmt_percent), ctu_vmt_percent10, ctu_vmt_percent),
    pct_data_source = ifelse(is.na(final_vmt_source), "2010 CTU share of county VMT", final_vmt_source)
  ) %>%
  select(-ctu_vmt_percent10)

### merge in with onroad emissions


ctu_transportation_emissions <- readRDS("_transportation/data/onroad_emissions.RDS") %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    year = emissions_year,
    sector = "Transportation",
    source = paste0(vehicle_fuel_label, " fueled vehicles"),
    category = category,
    data_source = data_source,
    factor_source = moves_edition
  ) %>%
  group_by(emissions_year, geoid, county_name, sector, category, source) %>%
  # summarize up to county level emissions
  summarize(value_emissions = sum(emissions_metric_tons_co2e), .groups = "keep") %>%
  # join with ctu-county VMT percentaegs
  left_join(ctu_county_year_pct_index,
    by = c("county_name",
      "emissions_year" = "vmt_year",
      "geoid"
    ),
    relationship = "many-to-many"
  ) %>%
  ungroup() %>%
  mutate(
    # CTU emissions total is county emissions multiplied by
    # ctu proportion of county VMT
    value_emissions = value_emissions * ctu_vmt_percent,
    geog_level = "ctu"
  ) %>%
  # get ctuid column
  left_join(ctu_population %>%
    select(coctu_id_gnis, ctuid, ctu_class) %>%
    unique(), by = c("coctu_id_gnis")) %>%
  # group at the CTU level (not coctu) and summarize
  group_by(emissions_year, geog_level, sector, category, source, ctu_name, ctu_class, ctuid, gnis) %>%
  summarize(value_emissions = sum(value_emissions), .groups = "keep") %>%
  ungroup() %>%
  mutate(sector_alt = sector) %>% # electricity and nat gas need this
  select(
    emissions_year,
    geog_level,
    ctu_name,
    ctu_class,
    sector,
    category,
    sector_alt,
    source,
    value_emissions
  )

saveRDS(
  ctu_transportation_emissions,
  "_transportation/data/ctu_transportation_emissions.RDS"
)
