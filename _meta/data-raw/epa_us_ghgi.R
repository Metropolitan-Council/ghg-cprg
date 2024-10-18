# Inventory of U.S. Greenhouse Gas Emissions and Sinks by State: 1990-2021
# https://www.epa.gov/ghgemissions/methodology-report-inventory-us-greenhouse-gas-emissions-and-sinks-state-1990-2021
source("R/_load_pkgs.R")
source("R/global_warming_potential.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")

if (!file.exists("_meta/data-raw/epa/us_ghgi/allstateghgdatapy2023readme_100323_0/AllStateGHGDataPY2023_100323.xlsx")) {
  # download file directly from EPA
  download.file("https://www.epa.gov/system/files/other-files/2023-10/allstateghgdatapy2023readme_100323_0.zip",
    destfile = "_meta/data-raw/epa/us_ghgi/allstateghgdatapy2023readme_100323_0.zip"
  )
  # unzip
  unzip("_meta/data-raw/epa/us_ghgi/allstateghgdatapy2023readme_100323_0.zip",
    exdir = "_meta/data-raw/epa/us_ghgi/allstateghgdatapy2023readme_100323_0"
  )
}

# ipcc sectors -----
ipcc_sectors <- readxl::read_xlsx("_meta/data-raw/epa/us_ghgi/allstateghgdatapy2023readme_100323_0/AllStateGHGDataPY2023_100323.xlsx",
  sheet = 2
) %>%
  clean_names()

ipcc_sector_category <- ipcc_sectors %>%
  select(
    ipcc_sector = sector, subsector, category, subcategory1, subcategory2,
    subcategory3, subcategory4, rownumber
  ) %>%
  unique()

ipcc <- ipcc_sectors %>%
  filter(
    state %in% c("MN", "WI")
  ) %>%
  pivot_longer(starts_with("y"),
    names_to = "inventory_year",
    values_to = "emission_grams"
  ) %>%
  mutate(
    inventory_year = str_remove(inventory_year, "y"),
    # reported in millions of metric tons
    # so convert to metric tons then to grams
    emission_grams = as.numeric(emission_grams) * 1000000 %>%
      units::as_units("metric_ton") %>%
      units::set_units("gram") %>%
      as.numeric()
  )


ipcc_transportation <-
  ipcc %>%
  filter(
    category == "Transportation",
    # these are things like asphalt, paints,
    # natural gas is used to move fuel through pipelines and in LNG vehicles
    # subsector != "Non-Energy Uses of Fossil Fuels",
    # inventory_year == 2021,
    !is.na(subsector)
    # state == "MN"
  ) %>%
  select(-subcategory2, -subcategory3, -subcategory4, -rownumber) %>%
  pivot_wider(
    names_from = "ghg",
    values_from = "emission_grams",
    values_fill = 0
  ) %>%
  clean_names() %>%
  group_by(sector, subsector, category, fuel, subcategory1, inventory_year, state) %>%
  summarize(
    co2 = sum(co2),
    ch4 = sum(ch4),
    n2o = sum(n2o),
    .groups = "keep"
  ) %>%
  group_by(sector, subsector, category, subcategory1, inventory_year, state) %>%
  summarize(
    co2_co2_equivalent =
      sum(co2, (ch4 * gwp$ch4), (n2o * gwp$n2o)),
    emissions_metric_tons_co2e = co2_co2_equivalent / 1000000,
    million_metric_tons_co2e = emissions_metric_tons_co2e / 1000000
  )

# for cross-reference
# MN 2021 as shown in GHG Data Explorer is the sum of CO2 for fossil fuel combustion
# fuel types Petroleum and Natural Gas

ipcc %>%
  group_by(
    inventory_year, state, sector, category, subsector, subcategory1, subcategory2,
    subcategory3, subcategory4, fuel
  ) %>%
  summarize(emissions_metric_tons_co2e = sum(emission_grams))


# economic sectors -----
econ_sectors <- readxl::read_xlsx(
  "_meta/data-raw/epa/us_ghgi/allstateghgdatapy2023readme_100323_0/AllStateGHGDataPY2023_100323.xlsx",
  sheet = 3
) %>%
  clean_names()

econ <- econ_sectors %>%
  filter(state %in% c("MN", "WI")) %>%
  select(-subcategory4, -rownumber) %>%
  pivot_longer(starts_with("y"),
    names_to = "inventory_year",
    values_to = "emission_grams"
  ) %>%
  mutate(
    inventory_year = str_remove(inventory_year, "y"),
    # reported in millions of metric tons
    # so convert to metric tons then to grams
    emission_grams = as.numeric(emission_grams) * 1000000 %>%
      units::as_units("metric_ton") %>%
      units::set_units("gram") %>%
      as.numeric()
  ) %>%
  unique()



econ_transportation <- econ %>%
  filter(econ_sector == "Transportation") %>%
  group_by(
    econ_sector, econ_source, subsector, subcategory1,
    state, ghg, inventory_year
  ) %>%
  summarize(
    emission_grams = sum(emission_grams),
    .groups = "keep"
  ) %>%
  pivot_wider(
    names_from = "ghg",
    values_from = "emission_grams",
    values_fill = 0
  ) %>%
  clean_names() %>% 
mutate(
  co2_co2_equivalent =
    sum(co2, (ch4 * gwp$ch4), (n2o * gwp$n2o)),
  emissions_metric_tons_co2e = co2_co2_equivalent / 1000000
)


# compare line numbers, sectors -----

ipcc_econ_mapping <- econ_sectors %>%
  select(
    econ_sector, subsector,
    subcategory1, subcategory2,
    subcategory3, subcategory4,
    rownumber
  ) %>%
  unique() %>%
  left_join(ipcc_sector_category,
    by = "rownumber",
    suffix = c(".econ", ".ipcc")
  ) %>%
  select(-rownumber) %>%
  unique()
