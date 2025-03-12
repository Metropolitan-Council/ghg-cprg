# Process Connexus data
source("R/_load_pkgs.R")

# Read city data
city_raw <- read_xlsx(here("_energy", "data-raw", "connexusDataRequest", "Connexus_County_City_Township_Consumption_2014_2024.xlsx"),
                    sheet = "City") %>%
  mutate(
    ctu_name = str_to_title(City),
    ctu_class = "CITY",
    mwh_delivered = case_when(
      Consumption == "REDACTED" ~ NA_real_, 
      grepl("^-?\\d*(\\.\\d+)?$", Consumption) ~ as.numeric(Consumption),  # checks if only numeric values are present
      TRUE ~ NA_real_  
    )
  ) %>%
  rename(
    sector = Class,
    customer_count = Premises,
    year = Year
  ) %>%
  select(
    -City, -Consumption
  )

# Read township data
township_raw <- read_xlsx(here("_energy", "data-raw", "connexusDataRequest", "Connexus_County_City_Township_Consumption_2014_2024.xlsx"),
                      sheet = "Township") %>%
  mutate(
    ctu_name = str_to_title(Township),
    ctu_class = "TOWNSHIP",
    mwh_delivered = case_when(
      Consumption == "REDACTED" ~ NA_real_, 
      grepl("^-?\\d*(\\.\\d+)?$", Consumption) ~ as.numeric(Consumption), # checks if only numeric values are present
      TRUE ~ NA_real_ 
    )
  ) %>%
  rename(
    sector = Class,
    customer_count = Premises,
    year = Year
  ) %>%
  select(
    -Township,
    -Consumption
  ) %>%
  mutate(
    ctu_name = case_when(
      grepl(" Twp$", ctu_name) ~ sub(" Twp$", "", ctu_name)
    )
  )

city_township_connexus <- rbind(city_raw, township_raw)
  
connexus_activityData_2014_2023 <- city_township_connexus %>%
  mutate(
    mwh_delivered = case_when(
      mwh_delivered == "REDACTED" ~ as.numeric(NA),
      TRUE ~ mwh_delivered)
  ) %>%
  filter(year != 2024)


# ctu and county reference, incl. population -- necessary for disaggregation to COCTU
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_ctu <- readRDS("_meta/data/cprg_ctu.RDS")
ctu_population <- readRDS("_meta/data/ctu_population.RDS") %>%
  filter(inventory_year > 2013 & inventory_year != 2024) %>%
  left_join(cprg_county %>% select(geoid, county_name, state_abb), by = "geoid") %>%
  filter(state_abb == "MN") %>%
  rename(year = inventory_year)

# Calculate unique total population by city-year-county
city_total_population <- ctu_population %>%
  distinct(ctu_name, ctu_class, year, county_name, ctu_population) %>% # Ensure unique rows per city-county-year
  group_by(ctu_name, ctu_class, year) %>%
  mutate(
    total_ctu_population = sum(ctu_population, na.rm = TRUE), # Sum populations across counties for each city-year
    multi_county = n_distinct(county_name) > 1
  ) %>%
  ungroup()


connexus_activityData_2014_2023 <- connexus_activityData_2014_2023 %>%
  # Join city_total_population back to main dataset
  full_join(city_total_population,
            by = c("ctu_name", "ctu_class", "year"),
            relationship = "many-to-many"
  ) %>%
  # Calculate proportions and disaggregated values
  group_by(ctu_name, ctu_class, year, county_name) %>%
  mutate(
    ctu_population_proportion = ctu_population / total_ctu_population,
    disagg_mwh_delivered = ifelse(
      multi_county,
      mwh_delivered * ctu_population_proportion,
      NA)
    ) %>%
  ungroup() %>%
  # Filter to core metro counties while keeping `county_name` intact
  filter(county_name %in% c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington")) %>%
  # exclude non-METC cities in metro
  filter(!ctu_name %in% c("Northfield", "Hanover", "New Prague", "Cannon Falls", "Rockford")) %>%
  mutate(
    source = "Electricity",
    utility = "Connexus Energy"
  ) %>%
  select(1:7, 13:14)
  
write_rds(connexus_activityData_2014_2023, here("_energy", "data", "connexus_activityData_2014_2023.RDS"))
