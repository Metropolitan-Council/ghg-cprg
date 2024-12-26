source("R/_load_pkgs.R")
mn_emissions <- readRDS(file.path(here::here(), "_waste/data/mn_emissions_2021.RDS"))
wi_emissions <- readRDS(file.path(here::here(), "_waste/data/solid_waste_WI_allyrs.RDS"))

# cleaning emissions data: make sure each has county, year, emissions, source
mn_cleaned <- mn_emissions %>%
  mutate(
    source = case_when(
      source == "Landfill" ~ "Landfill",
      source == "Onsite" ~ "Landfill",
      source == "WTE" ~ "Waste to energy",
      source == "Organics" ~ "Organics",
      source == "Recycling" ~ "Recycling"
    )
  ) %>%
  group_by(geoid, source) %>%
  mutate(
    sectorized_emissions = sum(value_emissions),
    data_source = "MPCA SCORE"
  ) %>%
  select(
    geoid,
    inventory_year,
    value_emissions = sectorized_emissions,
    source,
    data_source,
    units_emissions
  ) %>%
  distinct(.keep_all = TRUE)

wi_cleaned <- wi_emissions %>%
  filter(
    inventory_year == 2021
  ) %>%
  select(
    geoid,
    value_emissions,
    source,
    inventory_year,
    data_source,
    units_emissions
  )

# combine data
emissions_total <- mn_cleaned %>%
  bind_rows(wi_cleaned)

emissions_total_meta <- tribble(
  ~Column, ~Class, ~Description,
  "geoid", class(emissions_total$geoid), "FIPS code for county of waste origin",
  "inventory_year", class(emissions_total$inventory_year), "Year for which emissions were calculated",
  "value_emissions", class(emissions_total$value_emissions),
  "Emissions estimate in metric tons co2e",
  "units_emissions", class(emissions_total$units_emissions), "Emissions units",
  "source", class(emissions_total$source), "Waste processing source (Landfill, Recycling, Organics)",
  "data_source", class(emissions_total$source), "Original source of data"
)

saveRDS(emissions_total, file.path(here::here(), "_waste/data/solid_waste_2021.RDS"))
saveRDS(emissions_total_meta, file.path(here::here(), "_waste/data/solid_waste_2021_meta.RDS"))
