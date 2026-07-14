# Process Connexus data
source("R/_load_pkgs.R")

# Read city data
city_raw <- read_xlsx(here("_energy", "data-raw", "connexusDataRequest", "Connexus_County_City_Township_Consumption_2014_2024.xlsx"),
  sheet = "City"
) %>%
  mutate(
    ctu_name = str_to_title(City),
    ctu_class = "CITY",
    mwh_delivered = case_when(
      Consumption == "REDACTED" ~ NA_real_,
      grepl("^-?\\d*(\\.\\d+)?$", Consumption) ~ as.numeric(Consumption), # checks if only numeric values are present
      TRUE ~ NA_real_
    ) * 1e-3
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
  sheet = "Township"
) %>%
  mutate(
    ctu_name = str_to_title(Township),
    ctu_class = "TOWNSHIP",
    mwh_delivered = case_when(
      Consumption == "REDACTED" ~ NA_real_,
      grepl("^-?\\d*(\\.\\d+)?$", Consumption) ~ as.numeric(Consumption), # checks if only numeric values are present
      TRUE ~ NA_real_
    ) * 1e-3
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

# Read county data
county_raw <- read_xlsx(here("_energy", "data-raw", "connexusDataRequest", "Connexus_County_City_Township_Consumption_2014_2024.xlsx"),
  sheet = "County"
) %>%
  mutate(
    county_name = str_to_title(County),
    geog_level = "COUNTY",
    mwh_delivered = case_when(
      Consumption == "REDACTED" ~ NA_real_,
      grepl("^-?\\d*(\\.\\d+)?$", Consumption) ~ as.numeric(Consumption), # checks if only numeric values are present
      TRUE ~ NA_real_
    ) * 1e-3
  ) %>%
  rename(
    sector = Class,
    customer_count = Premises,
    year = Year
  ) %>%
  select(
    -County,
    -Consumption
  ) %>%
  mutate(
    source = "Electricity",
    utility = "Connexus Energy"
  )

city_township_connexus <- rbind(city_raw, township_raw)

# --- YoY spike/dip check at ctu_name + ctu_class level ---
ctu_yoy <- city_township_connexus %>%
  group_by(ctu_name, ctu_class, year) %>%
  summarise(total_mwh = sum(mwh_delivered, na.rm = TRUE), .groups = "drop") %>%
  arrange(ctu_name, ctu_class, year) %>%
  group_by(ctu_name, ctu_class) %>%
  mutate(
    prev_mwh = lag(total_mwh),
    pct_change = (total_mwh - prev_mwh) / prev_mwh * 100
  ) %>%
  ungroup()

flagged <- ctu_yoy %>%
  filter(abs(pct_change) >= 20) %>%
  arrange(ctu_name, ctu_class, year)

flagged %>% print(n = Inf)



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


connexus_activity <- city_township_connexus %>%
  # Join city_total_population back to main dataset
  full_join(city_total_population,
    by = c("ctu_name", "ctu_class", "year"),
    relationship = "many-to-many"
  ) %>%
  # Calculate proportions and disaggregated values
  group_by(ctu_name, ctu_class, year, county_name) %>%
  mutate(
    ctu_population_proportion = ctu_population / total_ctu_population,
    mwh_delivered = ifelse(
      multi_county,
      mwh_delivered * ctu_population_proportion,
      mwh_delivered
    )
  ) %>%
  ungroup() %>%
  # Filter to core metro counties while keeping `county_name` intact
  filter(county_name %in% c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington")) %>%
  # exclude non-METC cities in metro
  filter(!ctu_name %in% c("Northfield", "Hanover", "New Prague", "Cannon Falls", "Rockford")) %>%
  mutate(
    source = "Electricity",
    utility = "Connexus Energy"
  )  %>%
  select(ctu_name, 
         ctu_class,
         county_name,
         emissions_year = year,
         sector,
         mwh_delivered,
         source,
         utility)


write_rds(connexus_activity, here("_energy", "data", "connexus_electric_activity.RDS"))
write_rds(county_raw, here("_energy", "data", "connexus_electric_county_activity.RDS"))
