source("R/_load_pkgs.R")

cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_county_meta <- readRDS("_meta/data/cprg_county_meta.RDS")

census_county_population <- readRDS("_meta/data/census_county_population.RDS")

if (!file.exists("_meta/data-raw/mn-county-edr-historical-estimates-sdc-1990-2023_web_tcm36-646254.xlsx")) {
  # download directly from census.gov
  download.file("https://mn.gov/admin/assets/mn-county-edr-historical-estimates-sdc-1990-2023_web_tcm36-646254.xlsx",
                destfile = "_meta/data-raw/population/mn-county-edr-historical-estimates-sdc-1990-2023_web_tcm36-646254.xlsx",
                mode = "wb"
  )
}

mn_demography <- readxl::read_xlsx(
  "_meta/data-raw/population/mn-county-edr-historical-estimates-sdc-1990-2023_web_tcm36-646254.xlsx",
  sheet = 1,
  col_types = "text"
) %>% 
  clean_names() %>% 
  mutate(year = as.numeric(year),
         population = as.numeric(population))

mn_state_population <- mn_demography %>% 
  filter(geography_type == "State")

mn_county_population <- mn_demography %>% 
  filter(geography_type == "County",
         geography_name %in% cprg_county$county_name)

if (!file.exists("_meta/data-raw/Time_Series_Co_2024.xlsx")) {
  # download directly from census.gov
  download.file("https://doa.wi.gov/DIR/Time_Series_Co_2024.xlsx",
                destfile = "_meta/data-raw/population/Time_Series_Co_2024.xlsx",
                mode = "wb"
  )
}

wi_demography <- readxl::read_xlsx(
  "_meta/data-raw/population/Time_Series_Co_2024.xlsx",
  skip = 3,
  sheet = 1,
  col_types = "text"
) %>% 
  clean_names() %>% 
  select(-contains("census")) %>%  # Remove census columns (repeats)
  rename_with(~ str_extract(., "\\d{4}"), starts_with("x") | contains("estimate")) %>%
  pivot_longer(cols = starts_with("19") | starts_with("20"), 
               names_to = "year", 
               values_to = "population") %>% 
  mutate(year = as.numeric(year),
         population = as.numeric(population))

wi_state_population <- wi_demography %>% 
  filter(county_name == "STATE Total") %>% 
  mutate(state_name = "Wisconsin")

wi_county_population <- wi_demography %>% 
  filter(county_name %in% cprg_county[cprg_county$state_name == "Wisconsin", ]$county_name)

state_population <- bind_rows(mn_state_population %>% 
                                select(state_name = geography_name,
                                       inventory_year = year,
                                       population,
                                       households),
                              wi_state_population %>% 
                                select(state_name,
                                       inventory_year = year,
                                       population) %>% 
                                mutate(households = NA))

# create metadata
state_population_meta <-
  bind_rows(
    cprg_county_meta,
    tribble(
      ~Column, ~Class, ~Description,
      "population_year", class(state_population$population_year), "Population estimate year",
      "state_population", class(state_population$state_population), "Total state population estimate",
      "population_data_source", class(state_population$population_data_source), "Population estimate data source"
    )
  ) %>%
  filter(Column %in% names(state_population))

saveRDS(state_population, "_meta/data/state_population.RDS")
saveRDS(state_population_meta, "_meta/data/state_population_meta.RDS")
