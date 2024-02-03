# allocate WI state emissions by county population

source("R/_load_pkgs.R")

# from https://widnr.widen.net/view/pdf/o9xmpot5x7/AM610.pdf?t.download=true
# WI GHG Emissions Inventory from the DNR, 2018 data
wi_total_emissions <- 2.2 * 10^6 # in mtco2e
wi_pop_2021 <- 5893718 # NOTE this is not retrieved from same place as cprg_pop
# check with Liz

# population values from cprg_pop
cprg_pop <- readRDS(file.path(here::here(), "_meta/data/cprg_population.RDS"))

wi_pop <- tidycensus::get_decennial("county",
  state = "WI",
  variables = "P1_001N",
  year = 2020
) %>%
  mutate(
    population = sum(value),
    pop_percent = value / sum(value)
  ) %>%
  filter(GEOID %in% cprg_county$GEOID)


wi_emissions <- wi_pop %>%
  rowwise() %>%
  dplyr::mutate(
    emissions_metric_tons_co2e = pop_percent * wi_total_emissions
  ) %>%
  left_join(cprg_county, by = "GEOID") %>%
  select(NAME = NAME.y, population, pop_percent, emissions_metric_tons_co2e)

wi_emissions_meta <- tribble(
  ~Column, ~Class, ~Description,
  "NAME", class(wi_emissions$NAME), "WI county of waste origin, including state total",
  "population", class(wi_emissions$population), "Population of WI county (2020)",
  "pop_percent", class(wi_emissions$pop_percent), "Percent of WI population in county (2020)",
  "emissions_metric_tons_co2e", class(wi_emissions$emissions_metric_tons_co2e),
  "Total waste emissions allocated to county based on 2018 totals"
)

saveRDS(wi_emissions, paste0("_waste/data/wi_emissions.RDS"))
saveRDS(wi_emissions_meta, paste0("_waste/data/wi_emissions_meta.RDS"))
