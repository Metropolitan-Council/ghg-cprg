source("R/_load_pkgs.R")
mn_emissions <- readRDS(file.path(here::here(), "_waste/data/mn_sw_emissions_co2e.RDS"))
wi_emissions <- readRDS(file.path(here::here(), "_waste/data/wi_emissions.RDS"))

# cleaning emissions data: make sure each has county, year, emissions, source
mn_cleaned <- mn_emissions %>%
  ungroup %>% 
  mutate(
    data_source = "MPCA SCORE"
  ) %>%
  select(
    geog_name,
    year,
    sector,
    category,
    source,
    data_source,
    emissions_metric_tons_co2e
  ) 

wi_cleaned <- wi_emissions %>%
  mutate(
    data_source = "Wisconsin DNR",
    sector = "Waste",
    category = "Solid waste"
  ) %>%
  select(
    geog_name = name,
    emissions_metric_tons_co2e,
    source = Method,
    sector,
    category,
    year,
    data_source
  )

# combine data
emissions_total <- mn_cleaned %>%
  bind_rows(wi_cleaned)

emissions_total_meta <- tribble(
  ~Column, ~Class, ~Description,
  "geog_name", class(emissions_total$geog_name), "County of waste origin",
  "year", class(emissions_total$year), "Year for which emissions were calculated",
  "sector", class(emissions_total$sector), "Emissions sector (Waste or Energy)",
  "category", class(emissions_total$category), "Sector subcategory (Solid waste or Waste to energy)",
  "source", class(emissions_total$source), "Waste processing method (Landfill, Recycling, Organics)",
  "data_source", class(emissions_total$data_source), "Source of activity data",
  "emissions_metric_tons_co2e", class(emissions_total$emissions_metric_tons_co2e),
  "Emissions estimate in metric tons co2e"
)

saveRDS(emissions_total, file.path(here::here(), "_waste/data/county_sw_emissions.RDS"))
saveRDS(emissions_total_meta, file.path(here::here(), "_waste/data/county_sw_emissions_meta.RDS"))
