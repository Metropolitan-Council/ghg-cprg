# Process Xcel Community Reports data
source("R/_load_pkgs.R")
source("_energy/data-raw/_energy_emissions_factors.R")


# root directory with folders for each utility in scope (with each folder containing subfolders for all years which reporting to the state is available)
dir_centerpoint_reports <- here("_energy", "data-raw", "centerpoint_reports")


# CenterPoint provies their data in therms
mcf_per_therm <- 1/10.38


# Read CSV, ignoring the first two rows (nested headers) and extraneous notes outside the main data structure
df_raw <- read_xlsx(here("_energy", "data-raw", "centerpointDataRequest", "2015_2023MetCouncilCommunityNGData_PUBLIC.xlsx"),
                    range = "A3:U288",
                    col_names = FALSE) %>%
  select(,-3) # drop county column

# Define column names manually to reflect nesting structure
expected_columns <- c(
  "sector", "ctu_name",
  "2015_Energy", "2015_Customers",
  "2016_Energy", "2016_Customers",
  "2017_Energy", "2017_Customers",
  "2018_Energy", "2018_Customers",
  "2019_Energy", "2019_Customers",
  "2020_Energy", "2020_Customers",
  "2021_Energy", "2021_Customers",
  "2022_Energy", "2022_Customers",
  "2023_Energy", "2023_Customers"
)

# Assign the cleaned column names
colnames(df_raw) <- expected_columns

# Pivot to long format
df_long <- df_raw %>%
  pivot_longer(
    cols = starts_with("20"),  
    names_to = c("year", ".value"), # decompose nested column names into a year and value column for 1) Energy and 2) Customers
    names_sep = "_"
  ) %>%
  mutate(
    year = as.integer(year),
    ctu_name = str_to_title(ctu_name), # e.g., BLOOMINGTON --> Bloomington
    mcf_delivered = Energy * mcf_per_therm
  ) %>%
  select(-Energy)



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
