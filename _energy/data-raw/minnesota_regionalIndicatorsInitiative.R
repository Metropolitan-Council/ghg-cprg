source("R/_load_pkgs.R")

# Read in Regional Indicators Initiative data

# Load the dataset (Modify the file path as needed)
# Dataset acquired by downloading Tableau Workbook at RII site and extracting regional_indicators_Migrated Data.csv from within 
df <- read_csv(here("_energy", "data-raw", "rii", "regional_indicators_Migrated Data.csv"))


cprg_county <- readRDS("_meta/data/cprg_county.RDS")

ctu_population <- readRDS("_meta/data/ctu_population.RDS") %>%
  filter(inventory_year > 2006) %>%
  left_join(cprg_county %>% select(geoid, county_name, state_abb), by = "geoid") %>%
  filter(state_abb == "MN") %>%
  rename(year = inventory_year)

city_total_population <- ctu_population %>%
  distinct(ctu_name, ctu_class, year, county_name, ctu_population) %>% # Ensure unique rows per city-county-year
  group_by(ctu_name, ctu_class, year) %>%
  mutate(
    total_ctu_population = sum(ctu_population, na.rm = TRUE), # Sum populations across counties for each city-year
    multi_county = n_distinct(county_name) > 1
  ) %>%
  ungroup()


# Select relevant columns
df_cleaned <- df %>%
  select(
    ctu_name = City,
    city_id = `City Id`,
    county_id = `County Id`,
    state_abbr = State,  # Assuming "State" represents county
    year = Year,
    jobs = Jobs,
    households = Households,
    population = Population,
    `Com And Ind Electricity Mmbtu`,
    `Residential Electricity Mmbtu`,
    `Com And Ind Nat Gas Mmbtu`,
    `Residential Nat Gas Mmbtu`
  ) %>%
  mutate(year = as.integer(str_extract(year, "\\d{4}")),
         ctu_class = "CITY")  

# Pivot longer to create sector, source, and units columns
df_long <- df_cleaned %>%
  pivot_longer(
    cols = c(`Com And Ind Electricity Mmbtu`, `Residential Electricity Mmbtu`,
             `Com And Ind Nat Gas Mmbtu`, `Residential Nat Gas Mmbtu`),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    # Map sector based on column name
    sector = case_when(
      str_detect(metric, "Com And Ind") ~ "Commercial and Industrial",
      str_detect(metric, "Residential") ~ "Residential",
      TRUE ~ NA_character_
    ),
    
    # Map source (Electricity or Natural Gas)
    source = case_when(
      str_detect(metric, "Electricity") ~ "Electricity",
      str_detect(metric, "Nat Gas") ~ "Natural Gas",
      TRUE ~ NA_character_
    ),
    
    # Assign units as MMBtu
    units = "MMBtu",
    
    # Assign data source
    dataSource = "Regional Indicators Initiative"
  ) %>%
  select(-metric)  # Remove original metric column

# Conversion factors to align with other data
mmbtu_to_mwh <- 0.293071
mmbtu_to_mcf <- 1 / 1.038  # 0.96339...

# Ensure all necessary fields are included and properly ordered
df_final <- df_long %>%
  select(
    sector, value, units, ctu_name, year, source, dataSource, ctu_class
  ) %>%
  mutate(
    mwh_delivered = ifelse(source == "Electricity", value * mmbtu_to_mwh, NA_real_),
    mcf_delivered = ifelse(source == "Natural Gas", value * mmbtu_to_mcf, NA_real_)
  ) %>%
  select(-value, -units) %>%
  arrange(ctu_name, year, sector, source) %>%
  # Join city_total_population back to main dataset
  left_join(city_total_population,
            by = c("ctu_name", "ctu_class", "year"),
            relationship = "many-to-many"
  ) %>%
  # Calculate proportions and disaggregated values
  group_by(ctu_name, ctu_class, year, county_name) %>%
  mutate(
    ctu_population_proportion = ctu_population / total_ctu_population, # Calculate proportions
    disagg_mWh_delivered = ifelse(
      multi_county & !is.na(mwh_delivered),
      mwh_delivered * ctu_population_proportion,
      NA),
    disagg_mcf_delivered = ifelse(
      multi_county & !is.na(mcf_delivered),
      mcf_delivered * ctu_population_proportion,
      NA)
  ) %>%
  ungroup() %>%
  # Filter to core metro counties while keeping `county_name` intact
  filter(county_name %in% c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington")) %>%
  # exclude non-METC cities in metro
  filter(!ctu_name %in% c("Northfield", "Hanover", "New Prague", "Cannon Falls", "Rockford"))



minnesota_electricity_rii <- df_final %>%
  filter(source == "Electricity") 

minnesota_natGas_rii <- df_final %>%
  filter(source == "Natural Gas")


write_rds(minnesota_electricity_rii, here("_energy", "data", "rii_electricity_" ))

  




