#### Script to read in and process EPA GHG FLIGHT data
source("R/_load_pkgs.R")

cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_ctu <- readRDS("_meta/data/cprg_ctu.RDS")

### download flight data: https://ghgdata.epa.gov/ghgp/main.do
mn_flight <- read_excel(file.path(here::here(), "_industrial/data-raw/mn_flight.xls"))


#### There is no EPA flight records for Pierce or St. Croix counties in WI

mn_flight <- lapply(as.character(2010:2022), function(year) {
  read_excel(file.path(here::here(), "_industrial/data-raw/mn_flight.xls"),
    sheet = year,
    skip = 5
  )
}) %>%
  bind_rows() %>%
  clean_names()

## clean county and city names and filter to our region
cprg_flight <- mn_flight %>%
  mutate(
    county_name = str_to_sentence(gsub(" COUNTY", "", county_name)),
    city_name = str_to_title(gsub("ST.", "SAINT", city_name, ignore.case = TRUE))
  ) %>%
  filter(county_name %in% cprg_county$county_name)

cprg_flight %>% distinct(subparts)
# https://www.epa.gov/ghgreporting/resources-subpart-ghg-reporting
### key subparts to note here:
### D is electricity generation and should be discounted as it is inventoried elsewhere
### W is natural gas and petroleum systems. This is likely a large double count, though petroleum is only inventoried residentially
### HH is municipal solid waste landfills which is inventoried elsewhere
### TT is industrial waste landfills which are NOT inventoried elsewhere
cprg_flight %>%
  filter(grepl("R", subparts)) %>%
  distinct(facility_name)

cprg_flight <- cprg_flight %>%
  ## DD is electric transmission and would be filtered if it were included here (it is not)
  mutate(doublecount = if_else((grepl("D", subparts) | grepl("HH", subparts)),
    "Yes", "No"
  ))

cprg_flight_out <- cprg_flight %>%
  select(
    inventory_year = reporting_year,
    ghgrp_id,
    facility_name,
    latitude,
    longitude,
    city_name,
    county_name,
    state,
    value_emissions = ghg_quantity_metric_tons_co2e,
    doublecount,
    subparts
  ) %>%
  mutate(
    units_emissions = "Metric tons CO2e",
    sector = "Industrial",
    data_source = "EPA FLIGHT",
    factor_source = "EPA FLIGHT (no direct gas data provied)"
  )
### convert subparts to category/source at later date


cprg_flight_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "inventory_year", class(cprg_flight_out$inventory_year), "Year of survey",
    "city_name", class(cprg_flight_out$city_name), "City name",
    "county_name", class(cprg_flight_out$city_name), "County name",
    "state", class(cprg_flight_out$state), "State name",
    "ghgrp_id", class(cprg_flight_out$ghgrp_id), "Facility ID in GHG Reporting Program",
    "facility_name", class(cprg_flight_out$facility_name), "Facility name",
    "latitude", class(cprg_flight_out$latitude), "Latitude of industrial source",
    "longitude", class(cprg_flight_out$longitude), "Longitude of industrial source",
    "sector", class(cprg_flight_out$sector), "Emissions sector. One of Transportation, Energy, Waste, Nature, Agriculture",
    # "category", class(cprg_flight_out$category), "Category of emissions within given sector",
    # "source", class(cprg_flight_out$source), "Source of emissions. Most detailed sub-category in this table",
    "data_source", class(cprg_flight_out$data_source), "Activity data source",
    "factor_source", class(cprg_flight_out$factor_source), "Emissions factor data source",
    "value_emissions", class(cprg_flight_out$value_emissions), "Numerical value of emissions",
    "units_emissions", class(cprg_flight_out$units_emissions), "Units and gas type of emissions",
    "doublecount", class(cprg_flight_out$doublecount), "Is this emission counted in another sector?",
    "subparts", class(cprg_flight_out$subparts), "Subpart code for type of industrial facility"
  )

saveRDS(cprg_flight_out, "./_industrial/data/flight_industrial_point_sources_ctu.rds")
saveRDS(cprg_flight_meta, "./_industrial/data/flight_industrial_point_sources_ctu_meta.rds")

### summarize to the yearly county level while IDing doublecounts
flight_county_summary <- cprg_flight %>%
  group_by(reporting_year, county_name, doublecount) %>%
  summarize(co2e = sum(ghg_quantity_metric_tons_co2e)) %>%
  left_join(., cprg_county %>%
    select(county_name, geoid, state_name) %>%
    st_drop_geometry()) %>%
  select(
    inventory_year = reporting_year,
    county_name,
    state_name,
    value_emissions = co2e,
    doublecount
  ) %>%
  mutate(
    units_emissions = "Metric tons CO2e",
    sector = "Industrial",
    data_source = "EPA FLIGHT",
    factor_source = "EPA FLIGHT (no direct gas data provied)"
  )

### add meta table

saveRDS(flight_county_summary, "./_agriculture/data/flight_industrial_point_sources_county.rds")

ggplot(
  county_summary %>%
    filter(doublecount == "No"),
  aes(x = reporting_year, y = co2e, col = county_name)
) +
  geom_line() +
  theme_minimal()

leaflet(cprg_flight %>%
  filter(
    reporting_year == 2021,
    doublecount == "No"
  )) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    lng = ~longitude, # Replace 'longitude' with your actual longitude column name
    lat = ~latitude, # Replace 'latitude' with your actual latitude column name
    radius = ~ sqrt(ghg_quantity_metric_tons_co2e) / 70, # Scale bubble size by quantity
    color = "blue", # Color of the bubbles
    fillOpacity = 0.7,
    popup = ~ paste0(
      "<strong>City:</strong> ", city_name, "<br>",
      "<strong>Facility:</strong> ", facility_name, "<br>",
      "<strong>GHG Quantity (Metric Tons):</strong> ", ghg_quantity_metric_tons_co2e
    )
  )
