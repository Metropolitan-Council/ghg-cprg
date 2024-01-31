source("R/_load_pkgs.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
# source propane and kerosene data
propane_use <- readRDS(file.path(here::here(), "_energy/data-raw/propane_use_county.RDS")) %>%
  mutate(fuel_type = "Propane")
kerosene_use <- readRDS(file.path(here::here(), "_energy/data-raw/kerosene_use_county.RDS")) %>%
  mutate(fuel_type = "Kerosene")

# row bind for output data
fuel_use <- bind_rows(
  propane_use,
  kerosene_use
) %>%
  # select(NAME, mmBtu, CO2e, fuel_type) %>%
  mutate(year = 2021) %>%
  left_join(cprg_county, by = "GEOID") %>%
  select(
    NAME = NAME.y,
    year,
    fuel_type,
    mmBtu,
    emissions_metric_tons_co2e = CO2e
  )

names(fuel_use)

fuel_use_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "NAME", class(fuel_use$NAME), "County name",
    "year", class(fuel_use$year), "Year",
    "fuel_type", class(fuel_use$fuel_type), "Type of fuel combusted",
    "mmBtu", class(fuel_use$mmBtu), "Millions of BTUs generated in county from household fuel use",
    "emissions_metric_tons_co2e", class(fuel_use$emissions_metric_tons_co2e), "Metric tonnes of CO2 equivalency generated from household fuel use"
  )

saveRDS(fuel_use, "_energy/data/fuel_use.RDS")
saveRDS(fuel_use_meta, "_energy/data/fuel_use_meta.RDS")
