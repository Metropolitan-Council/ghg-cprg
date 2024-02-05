# allocate WI state emissions by county population

source("R/_load_pkgs.R")

# from https://widnr.widen.net/view/pdf/o9xmpot5x7/AM610.pdf?t.download=true
# WI GHG Emissions Inventory from the DNR, 2018 data
wi_total_emissions <- 2.2 * 10^6 # in mtco2e
wi_pop_2021 <- 5893718 # NOTE this is not retrieved from same place as cprg_pop
# check with Liz

cprg_pop <- readRDS(file.path(here::here(), "_meta/data/cprg_population.RDS"))
cprg_county_proportions <- readRDS("_meta/data/cprg_county_proportions.RDS")

wi_pop <- cprg_county_proportions %>%
  filter(
    STATE == "Wisconsin",
    year == "2020"
  )

wi_emissions <- wi_pop %>%
  rowwise() %>%
  dplyr::mutate(
    emissions_metric_tons_co2e = county_proportion_of_state_pop * wi_total_emissions
  ) %>%
  left_join(cprg_county, by = "GEOID") %>%
  select(NAME = NAME.y, county_population, county_proportion_of_state_pop, emissions_metric_tons_co2e)

wi_emissions_meta <- tribble(
  ~Column, ~Class, ~Description,
  "NAME", class(wi_emissions$NAME), "WI county of waste origin, including state total",
  "county_population", class(wi_emissions$county_population), "Population of WI county (2020)",
  "county_proportion_of_state_pop", class(wi_emissions$county_proportion_of_state_pop), "Percent of WI population in county (2020)",
  "emissions_metric_tons_co2e", class(wi_emissions$emissions_metric_tons_co2e),
  "Total waste emissions allocated to county based on 2018 totals"
)

saveRDS(wi_emissions, paste0("_waste/data/wi_emissions.RDS"))
saveRDS(wi_emissions_meta, paste0("_waste/data/wi_emissions_meta.RDS"))
