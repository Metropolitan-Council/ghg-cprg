source("R/_load_pkgs.R")
source("_energy/data-raw/_energy_emissions_factors.R")

# Read in Regional Indicators Initiative data

# Load the dataset (Modify the file path as needed)
# Dataset acquired by downloading Tableau Workbook at RII site and extracting regional_indicators_Migrated Data.csv from within 
df <- read_csv(here("_energy", "data-raw", "rii", "regional_indicators_Migrated Data.csv"))

# Select relevant columns
df_cleaned <- df %>%
  select(
    city_name = City,
    city_id = `City Id`,
    county_id = `County Id`,
    county_name = State,  # Assuming "State" represents county
    year = Year,
    jobs = Jobs,
    households = Households,
    population = Population,
    `Com And Ind Electricity Mmbtu`,
    `Residential Electricity Mmbtu`,
    `Com And Ind Nat Gas Mmbtu`,
    `Residential Nat Gas Mmbtu`
  ) %>%
  mutate(year = as.integer(str_extract(year, "\\d{4}")))  # Extract numeric year

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

# Ensure all necessary fields are included and properly ordered
df_final <- df_long %>%
  mutate(ctu_class = "CITY") %>%
  select(
    sector, value, units, city_name, county_name, year, source, dataSource, ctu_class, population, jobs, households
  ) %>%
  arrange(city_name, year, sector, source)
