source("R/_load_pkgs.R")
mn_emissions <- readRDS(file.path(here::here(), "_waste/data/mn_emissions.RDS"))
wi_emissions <- readRDS(file.path(here::here(), "_waste/data/wi_emissions.RDS"))

# cleaning emissions data: make sure each has county, year, emissions, source
mn_cleaned <- mn_emissions %>%
  mutate(
    source = case_when(
      Method == "Landfill" ~ "Landfill",
      Method == "Onsite" ~ "Landfill",
      Method == "WTE" ~ "Waste to Energy",
      Method == "Organics" ~ "Organics",
      Method == "Recycling" ~ "Recycling"
    )
  ) %>%
  group_by(County, source) %>%
  mutate(
    sectorized_emissions = sum(emissions_metric_tons_co2e),
    data_source = "MPCA SCORE"
  ) %>%
  select(
    county = County,
    year = Year,
    emissions_metric_tons_co2e = sectorized_emissions,
    source,
    data_source
  ) %>%
  distinct(.keep_all = TRUE)

wi_cleaned <- wi_emissions %>%
  mutate(
    source = "Landfill",
    year = 2021,
    data_source = "Wisconsin DNR"
  ) %>%
  select(
    county = NAME,
    emissions_metric_tons_co2e,
    source,
    year,
    data_source
  )

# combine data
emissions_total <- mn_cleaned %>%
  bind_rows(wi_cleaned)

emissions_total_meta <- tribble(
  ~Column, ~Class, ~Description,
  "county", class(emissions_total$county), "County of waste origin",
  "year", class(emissions_total$year), "Year for which emissions were calculated",
  "emissions_metric_tons_co2e", class(emissions_total$emissions_metric_tons_co2e),
  "Emissions estimate in metric tons co2e",
  "source", class(emissions_total$source), "Waste processing method (Landfill, Recycling, Organics)"
)

saveRDS(emissions_total, file.path(here::here(), "_waste/data/county_sw_emissions.RDS"))
saveRDS(emissions_total_meta, file.path(here::here(), "_waste/data/county_sw_emissions_meta.RDS"))
