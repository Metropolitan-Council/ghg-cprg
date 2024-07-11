source("R/_load_pkgs.R")
cprg_county <- readRDS(file.path(here::here(), "_meta/data/cprg_county.RDS"))
cprg_population <- readRDS(file.path(here::here(), "_meta/data/cprg_county_proportions.RDS"))



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
  mutate(
    State = "MN",
    STATE = "Minnesota"
  )

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
  mutate(
    State = "WI",
    STATE = "Wisconsin"
  )

# bind files and convert CO2e from MMTCO2e to metric tonnes CO2e
wastewater_epa <- bind_rows(mn_epa, wi_epa) %>%
  filter(!CO2e == "-") %>%
  group_by(Year, STATE) %>%
  summarize(co2e_state = sum(as.numeric(CO2e)) * 10^6)

wastewater_2005_2021 <- left_join(cprg_county_proportions %>% filter(year >= 2005 & year <= 2021),
  wastewater_epa,
  by = c("year" = "Year", "STATE" = "STATE")
) %>%
  mutate(co2e = county_proportion_of_state_pop * co2e_state) %>%
  select(STATE, STATEFP, NAME, GEOG_UNIT_ID = COUNTYFP, year, co2e)

# and save
saveRDS(wastewater_2005_2021, "_waste/data/epa_county_wastewater_2005_2021.RDS")

wastewater_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "STATE", class(wastewater_2005_2021$STATE), "State",
    "STATEFP", class(wastewater_2005_2021$STATEFP), "State FIPS code",
    "NAME", class(wastewater_2005_2021$NAME), "Name of county",
    "GEOG_UNIT_ID", class(wastewater_2005_2021$GEOG_UNIT_ID), "County FIPS code",
    "year", class(wastewater_2005_2021$year), "Year",
    "co2e", class(wastewater_2005_2021$co2e), "Metric tons of CO2 equivalency generated from municipal wastewater treatment"
  )

saveRDS(wastewater_meta, "_waste/data/epa_county_wastewater_2005_2021_meta.RDS")
