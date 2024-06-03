source("R/_load_pkgs.R")

## we'll scale emissions per capita, so we need MN pop estimates from 2005-2020

## function to get ACS 5 year estimates
fetch_mn_population <- function(year) {
  data <- tidycensus::get_acs(
    survey = "acs5",
    year = year,
    state = "MN",
    geography = "state",
    variables = c(total_pop = "DP05_0001E")
  )
  # Extract the population estimate
  population <- data$estimate
  return(data.frame(year = year, population = population))
}

# ACS only goes back to 2009
mn_pop <- do.call(rbind, lapply(2009:2020, fetch_mn_population))

# anchor population interpolation between 2000 and 2009, years with data
mn_2000 <- tidycensus::get_decennial(
  year = 2000,
  state = "MN",
  geography = "state",
  variables = c(total_pop = "P001001")
) %>% 
  mutate(year = 2000, population = value) %>% 
  dplyr::select(year,population)

# population interpolation function
interpolate_population <- function(start_year, end_year, start_pop, end_pop) {
  years <- start_year:end_year
  population <- approx(c(start_year, end_year), c(start_pop, end_pop), xout = years)$y
  return(data.frame(year = years, population = round(population,0)))
}

# Interpolating population and taking 2005 to 2008
mn_pop_full <- bind_rows(interpolate_population(2000, 2009, mn_2000$population[1], mn_pop$population[1]) %>% 
                      filter(year %in% c(2005:2008)),
                    mn_pop)

# read in mpca data, bind, and convert co2e to per capita for all sectors
mpca_inv <- bind_rows(
read_csv("_meta/data-raw/mpca_ag_inv.csv",skip = 1) %>% 
  pivot_longer(cols = -c(1:2),names_to = "year", values_to = "co2e") %>% 
  rename(Subsector = `Sources (group)`),
read_csv("_meta/data-raw/mpca_comm_inv.csv",skip = 1) %>% 
  pivot_longer(cols = -c(1:2),names_to = "year", values_to = "co2e") %>% 
  rename(Subsector = `Sources (group)`),
read_csv("_meta/data-raw/mpca_elec_inv.csv",skip = 1) %>% 
  pivot_longer(cols = -c(1:2),names_to = "year", values_to = "co2e") %>% 
  rename(Subsector = `Sources (group)`),
read_csv("_meta/data-raw/mpca_ind_inv.csv",skip = 1) %>% 
  pivot_longer(cols = -c(1:2),names_to = "year", values_to = "co2e") %>% 
  rename(Subsector = `Sources (group)`),
read_csv("_meta/data-raw/mpca_res_inv.csv",skip = 1) %>% 
  pivot_longer(cols = -c(1:2),names_to = "year", values_to = "co2e") %>% 
  rename(Subsector = `Sources (group)`),
read_csv("_meta/data-raw/mpca_tran_inv.csv",skip = 1) %>% 
  pivot_longer(cols = -c(1:2),names_to = "year", values_to = "co2e") %>% 
  rename(Subsector = `Sources (group)`),
read_csv("_meta/data-raw/mpca_waste_inv.csv",skip = 1) %>% 
  pivot_longer(cols = -c(1:2),names_to = "year", values_to = "co2e") %>% 
  rename(Subsector = `Sources (group)`)) %>% 
  mutate(year = as.numeric(year)) %>% 
left_join(.,mn_pop_full) %>% 
  mutate(co2e_per_cap = co2e /population)

mpca_inv_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "Sector", class(mpca_inv$Sector), "Sector",
    "Subsector", class(mpca_inv$Subsector), "Subsector",
    "year", class(mpca_inv$year), "Year of inventory",
    "co2e", class(mpca_inv$co2e), "Metric tonnes of CO2 equivalency generated",
    "population", class(mpca_inv$population), "Minnesota state population",
    "co2e_per_cap", class(mpca_inv$co2e_per_cap), "Metric tonnes of CO2 equivalency generated per capita"
  )
  
saveRDS(mpca_inv, "_meta/data/mpca_ghg_inv_2005_2020.RDS")

saveRDS(mpca_inv, "_meta/data/mpca_ghg_inv_2005_2020_meta.RDS")
