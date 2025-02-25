# Process Xcel Community Reports data
source("R/_load_pkgs.R")
source("_energy/data-raw/_energy_emissions_factors.R")


# root directory with folders for each utility in scope (with each folder containing subfolders for all years which reporting to the state is available)
dir_centerpoint_reports <- here("_energy", "data-raw", "centerpoint_reports")

# ctu and county reference, incl. population -- necessary for disaggregation to COCTU
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_ctu <- readRDS("_meta/data/cprg_ctu.RDS")
ctu_population <- readRDS("_meta/data/ctu_population.RDS") %>%
  filter(inventory_year > 2014) %>%
  left_join(cprg_county %>% select(geoid, county_name, state_abb), by = "geoid") %>%
  filter(state_abb == "MN") %>%
  rename(year = inventory_year)

# CenterPoint provies their data in therms, most 
mcf_per_therm <- 1/10.38



# Read CSV, ignoring the first two rows (nested headers) and extraneous notes outside the main data structure
df_raw <- read_xlsx(here("_energy", "data-raw", "centerpointDataRequest", "2015_2023MetCouncilCommunityNGData_PUBLIC.xlsx"),
                    range = "A3:U288",
                    col_names = FALSE) %>%
  select(,-3) # drop county column

# Define column names manually to reflect nesting structure
expected_columns <- c(
  "Rate Class", "ctu_name",
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
    names_to = c("Year", ".value"), # decompose nested column names into a year and value column for 1) Energy and 2) Customers
    names_sep = "_"
  ) %>%
  mutate(
    Year = as.integer(Year),
    ctu_name = str_to_title(ctu_name)  # e.g., BLOOMINGTON --> Bloomington
  )
