# Process Dakota Electrics data
source("R/_load_pkgs.R")

# Read city data
city_raw <- read_xlsx(here("_energy", "data-raw", "dakotaElectricDataRequest", "METC_DEA_Usage_Breakdown.xlsx")) %>%
  mutate(
    ctu_name = str_to_title(Municipality),
    sector = case_when(
      CUSTGROUP == "C&I" ~ "Commercial/Industrial",
      CUSTGROUP == "COM" ~ "Commercial",
      CUSTGROUP == "DEA" ~ "Dakota Electric Operations",
      CUSTGROUP == "IRR" ~ "Irrigation Services",
      CUSTGROUP == "RES" ~ "Residential",
    ),
    mwh_delivered = `Usage (kWh)` / 1000,
    utility = "Dakota Electric"
  ) %>%
  select(
    ctu_name,
    ctu_class = MunicipalityClass,
    inventory_year = ReportingYear,
    sector,
    customer_count = `Count Services`,
    mwh_delivered
  ) %>%
  # Dakota's 2018 data is faulty -- starts in March
  filter(!inventory_year == 2018)

# --- YoY spike/dip check at ctu_name + ctu_class level ---
ctu_yoy <- city_raw %>%
  group_by(ctu_name, ctu_class, inventory_year) %>%
  summarise(total_mwh = sum(mwh_delivered, na.rm = TRUE), .groups = "drop") %>%
  arrange(ctu_name, ctu_class, inventory_year) %>%
  group_by(ctu_name, ctu_class) %>%
  mutate(
    prev_mwh = lag(total_mwh),
    pct_change = (total_mwh - prev_mwh) / prev_mwh * 100
  ) %>%
  ungroup()

flagged <- ctu_yoy %>%
  filter(abs(pct_change) >= 10) %>%
  arrange(ctu_name, ctu_class, inventory_year)

flagged %>% print(n = Inf)
# Inver Grove Heights had substantial commercial growth 2021-2023 which appears legitimate


# code should be added to meta at some point -- COCTU disaggregation is happening in a lot of places
# ctu and county reference, incl. population -- necessary for disaggregation to COCTU
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_ctu <- readRDS("_meta/data/cprg_ctu.RDS")
ctu_population <- readRDS("_meta/data/ctu_population.RDS") %>%
  filter(inventory_year > 2013 & inventory_year != 2024) %>%
  left_join(cprg_county %>% select(geoid, county_name, state_abb), by = "geoid") %>%
  filter(state_abb == "MN")

# Calculate unique total population by city-inventory_year-county
city_total_population <- ctu_population %>%
  distinct(ctu_name, ctu_class, inventory_year, county_name, ctu_population) %>% # Ensure unique rows per city-county-inventory_year
  group_by(ctu_name, ctu_class, inventory_year) %>%
  mutate(
    total_ctu_population = sum(ctu_population, na.rm = TRUE), # Sum populations across counties for each city-inventory_year
    multi_county = n_distinct(county_name) > 1
  ) %>%
  ungroup()


dakota_electric_activity <- city_raw %>%
  # Join city_total_population back to main dataset
  left_join(city_total_population,
    by = c("ctu_name", "ctu_class", "inventory_year"),
    relationship = "many-to-many"
  ) %>%
  # Calculate proportions and disaggregated values
  group_by(ctu_name, ctu_class, inventory_year, county_name) %>%
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
    utility = "Dakota Electric Association"
  ) %>%
  select(ctu_name,
    ctu_class,
    county_name,
    emissions_year = inventory_year,
    sector,
    mwh_delivered,
    source,
    utility
  )

write_rds(dakota_electric_activity, here("_energy", "data", "dakota_electric_activity.RDS"))
