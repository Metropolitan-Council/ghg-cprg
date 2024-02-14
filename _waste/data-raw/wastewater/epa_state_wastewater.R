source("R/_load_pkgs.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")

# mn epa -----
# summary page from the
# State Inventory and Projection Tool
# saved as a CSV
mn_epa <- readr::read_csv("_waste/data-raw/wastewater/epa/epa-mn-wastewater.csv",
  skip = 4,
  n_max = 3
) %>%
  pivot_longer(cols = 3:38, names_to = "Year", values_to = "CO2e") %>%
  select(-1) %>%
  rename(Emission_type = `Emissions (MMTCO2E)`) %>%
  mutate(State = "MN",
         STATE = "Minnesota")

# wi epa -----
# summary page from the
# State Inventory and Projection Tool
# saved as a CSV
wi_epa <- readr::read_csv("_waste/data-raw/wastewater/epa/epa-wi-wastewater.csv",
  skip = 4,
  n_max = 3
) %>%
  pivot_longer(cols = 3:38, names_to = "Year", values_to = "CO2e") %>%
  select(-1) %>%
  rename(Emission_type = `Emissions (MMTCO2E)`) %>%
  mutate(State = "WI",
         STATE = "Wisconsin")

# bind files and convert CO2e from MMTCO2e to metric tonnes CO2e
wastewater_epa <- bind_rows(mn_epa, wi_epa) %>%
  filter(!CO2e == "-") %>%
  mutate(CO2e = as.numeric(CO2e) * 10^6)

saveRDS(wastewater_epa, "_waste/data-raw/wastewater/epa_state_wastewater_by_year.RDS")

cprg_pop <- readRDS(file.path(here::here(), "_meta/data/cprg_population.RDS"))
cprg_county_proportions <- readRDS("_meta/data/cprg_county_proportions.RDS")

### using county population percentages, apportion state CO2e estimates to each county
wi_2021 <- cprg_county_proportions %>%
  filter(
    STATE == "Wisconsin",
    year == "2021"
  ) %>%
  mutate(
    epa_co2e = county_proportion_of_state_pop *
      as.numeric(wastewater_epa %>%
                   filter(Year == 2021 & State == "WI") %>%
                   summarize(value = sum(CO2e))) ### combine CH4 and N2O emissions
  )

### using county population percentages, apportion state CO2e estimates to each county
mn_2021 <- cprg_county_proportions %>%
  filter(
    STATE == "Minnesota",
    year == "2021"
  ) %>%
  mutate(
    epa_co2e = county_proportion_of_state_pop *
      as.numeric(wastewater_epa %>%
                   filter(Year == 2021 & State == "MN") %>%
                   summarize(value = sum(CO2e))) ### combine CH4 and N2O emissions
  )

# bind WI and MN
ww_epa_2021 <- rows_append(
  wi_2021 %>% dplyr::select(GEOID, NAME, epa_co2e),
  mn_2021 %>% dplyr::select(GEOID, NAME, epa_co2e)
)

# and save
saveRDS(ww_epa_2021, "_waste/data/epa_county_wastewater.RDS")

wastewater_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "GEOID ", class(ww_epa_2021$GEOID), "County ID",
    "NAME", class(ww_epa_2021$NAME), "Name of county",
    "epa_co2e", class(ww_epa_2021$epa_co2e), "Metric tons of CO2 equivalency generated from municipal wastewater treatment"
  )

saveRDS(wastewater_meta, "_waste/data/epa_county_wastewater_meta.RDS")
