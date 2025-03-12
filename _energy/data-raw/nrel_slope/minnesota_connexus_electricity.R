# Process Connexus data
source("R/_load_pkgs.R")

# Read city data
city_raw <- read_xlsx(here("_energy", "data-raw", "connexusDataRequest", "Connexus_County_City_Township_Consumption_2014_2024.xlsx"),
                    sheet = "City") %>%
  mutate(
    ctu_name = str_to_title(City),
    ctu_class = "CITY"
  ) %>%
  rename(
    sector = Class,
    customer_count = Premises,
    mwh_delivered = Consumption,
    year = Year
  ) %>%
  select(
    -City
  )

# Read township data
township_raw <- read_xlsx(here("_energy", "data-raw", "connexusDataRequest", "Connexus_County_City_Township_Consumption_2014_2024.xlsx"),
                      sheet = "Township") %>%
  mutate(
    ctu_name = str_to_title(Township),
    ctu_class = "TOWNSHIP"
  ) %>%
  rename(
    sector = Class,
    customer_count = Premises,
    mwh_delivered = Consumption,
    year = Year
  ) %>%
  select(
    -Township
  )

city_township_connexus <- rbind(city_raw, township_raw)
  
connexus_activityData_2014_2024 <- city_township_connexus %>%
  mutate(
    mwh_delivered = case_when(
      mwh_delivered == "REDACTED" ~ NA_character_,
      TRUE ~ mwh_delivered)
  )


# ctu and county reference, incl. population -- necessary for disaggregation to COCTU
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_ctu <- readRDS("_meta/data/cprg_ctu.RDS")
ctu_population <- readRDS("_meta/data/ctu_population.RDS") %>%
  filter(inventory_year > 2014) %>%
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


centerpoint_activityData_2015_2023 <- df_long %>%
  # Remove records with no or unusable data -- may need to revisit to snag years like 2022 if 2021 or other years of interest are missing
  filter(!is.na(mcf_delivered)) %>%
  # Join city_total_population back to main dataset
  full_join(city_total_population,
            by = c("ctu_name", "year"),
            relationship = "many-to-many"
  ) %>%
  # Calculate proportions and disaggregated values
  group_by(ctu_name, ctu_class, year, county_name) %>%
  mutate(
    ctu_population_proportion = ctu_population / total_ctu_population, # Calculate proportions
    disagg_util_reported_customers = ifelse(
      multi_county,
      Customers * ctu_population_proportion,
      NA
    ),
    disagg_mcf_delivered = ifelse(
      multi_county,
      mcf_delivered * ctu_population_proportion,
      NA
    )
  ) %>%
  ungroup() %>%
  # Filter to core metro counties while keeping `county_name` intact
  filter(county_name %in% c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington")) %>%
  filter(!is.na(sector)) %>%
  filter(sector != "All")
