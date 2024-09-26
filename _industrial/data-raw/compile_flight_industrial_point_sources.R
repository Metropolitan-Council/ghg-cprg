#### Script to read in and process EPA GHG FLIGHT data
source("R/_load_pkgs.R")

cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_ctu <- readRDS("_meta/data/cprg_ctu.RDS")


mn_flight <-  read_excel(file.path(here::here(), "_industrial/data-raw/mn_flight.xls"))

#### There is no EPA flight records for Pierce or St. Croix counties in WI

mn_flight <- lapply(as.character(2010:2022), function(year) {
  read_excel(file.path(here::here(), "_industrial/data-raw/mn_flight.xls"), 
             sheet = year,
             skip = 5)
}) %>% 
  bind_rows() %>% 
  clean_names()

## clean county and city names and filter to our region
cprg_flight <- mn_flight %>% 
  mutate(county_name = str_to_sentence(gsub(" COUNTY", "", county_name)),
         city_name = str_to_title(gsub("ST.", "SAINT",city_name, ignore.case = TRUE))) %>% 
  filter(county_name %in% cprg_county$county_name)

cprg_flight %>% distinct(subparts)
# https://www.epa.gov/ghgreporting/resources-subpart-ghg-reporting
### key subparts to note here:
### D is electricity generation and should be discounted as it is inventoried elsewhere
### HH is municipal solid waste landfills which is inventoried elsewhere
### TT is industrial waste landfills which are NOT inventoried elsewhere

cprg_flight <- cprg_flight %>% 
## DD is electric transmission and would be filtered if it were included here (it is not)
  filter(!grepl("D", subparts),
         !grepl("HH", subparts))

county_summary <- cprg_flight %>% 
  group_by(reporting_year, county_name) %>% 
  summarize(co2e = sum(ghg_quantity_metric_tons_co2e))

ggplot(county_summary, aes(x = reporting_year, y = co2e, col = county_name)) + 
  geom_line()

leaflet(cprg_flight %>%
  filter(reporting_year == 2021)) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    lng = ~longitude,  # Replace 'longitude' with your actual longitude column name
    lat = ~latitude,   # Replace 'latitude' with your actual latitude column name
    radius = ~sqrt(ghg_quantity_metric_tons_co2e) / 70,  # Scale bubble size by quantity
    color = "blue",  # Color of the bubbles
    fillOpacity = 0.7,
    popup = ~paste0(
      "<strong>City:</strong> ", city_name, "<br>",
      "<strong>Facility:</strong> ", facility_name, "<br>",
      "<strong>GHG Quantity (Metric Tons):</strong> ", ghg_quantity_metric_tons_co2e
    )
  )
