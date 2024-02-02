# allocate WI state emissions by county population

source("R/_load_pkgs.R")
# library(rvest)
# library(janitor)
# library(tibble)
library(dplyr)

# from https://widnr.widen.net/view/pdf/o9xmpot5x7/AM610.pdf?t.download=true
# WI GHG Emissions Inventory from the DNR, 2018 data
wi_total_emissions <- 2.2 * 10^6 # in mtco2e
wi_pop_2021 <- 5893718 # NOTE this is not retrieved from same place as cprg_pop
# check with Liz

# population values from cprg_pop
cprg_pop <- readRDS(file.path(here::here(), "R/data/cprg_population.RDS"))
wi_pop <- cprg_pop %>%
  filter(STATE == "Wisconsin") %>%
  select(
    County = NAME,
    population
  )

wi_emissions <- wi_pop %>%
  dplyr::mutate(
    percent_pop = population / wi_pop_2021,
    emissions_metric_tons_co2e = percent_pop * wi_total_emissions
  )

wi_emissions_meta <- tribble(
  ~Column, ~Class, ~Description,
  "County", class(wi_emissions$County), "WI county of waste origin, including state total",
  "population", class(wi_emissions$population), "Population of WI county (2020)",
  "percent_pop", class(wi_emissions$percent_pop), "Percent of WI population in county (2020)",
  "emissions_metric_tons_co2e", class(wi_emissions$emissions_metric_tons_co2e),
  "Total waste emissions allocated to county based on 2018 totals"
)

saveRDS(wi_emissions, paste0("_waste/data/wi_emissions.RDS"))
saveRDS(wi_emissions_meta, paste0("_waste/data/wi_emissions_meta.RDS"))
