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



# Read CSV, treating first two rows as data
df_raw <- read_csv("your_file.csv", col_names = FALSE)

# Extract headers
years <- df_raw[1,]  # First row contains years
headers <- df_raw[2,]  # Second row contains actual column names

# Combine years and headers to create unique column names
col_names <- paste(headers, years, sep = "_")  # Concatenate year with variable names
col_names[1:2] <- c("Rate Class", "ctu_name")  # Rename columns and drop "County"

# Assign new column names
df_clean <- df_raw[-c(1,2), ]  # Remove first two rows
colnames(df_clean) <- col_names  # Apply new column names

# Pivot data into long format
df_long <- df_clean %>%
  pivot_longer(
    cols = starts_with("Energy") | starts_with("Customer"),  # Select all year-specific columns
    names_to = c("Variable", "Year"),
    names_sep = "_",
    values_drop_na = FALSE  # Keep NA values
  ) %>%
  pivot_wider(names_from = "Variable", values_from = "value") %>%
  rename(
    Energy_Quantity = `Energy Quantity (Therms)`,
    Customer_Count = `Customer Count by Installation`
  ) %>%
  mutate(
    Year = as.integer(Year),  # Convert Year to integer
    ctu_name = str_to_title(ctu_name)  # Convert ctu_name to regular capitalization
  )
