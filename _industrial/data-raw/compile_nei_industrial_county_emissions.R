######### script for pulling industrial point source emissions from NEI ####
source("R/_load_pkgs.R")
source("R/global_warming_potential.R")

cprg_county <- readRDS("_meta/data/cprg_county.RDS")

# base URL
req_base <- httr2::request("https://data.epa.gov/efservice")
# supplementary tables
# fetch sectors
sectors <- req_base %>%
  httr2::req_url_path_append("SECTORS/CSV") %>%
  httr2::req_method("GET") %>%
  httr2::req_perform() %>%
  httr2::resp_body_string(encoding = "UTF-8") %>%
  readr::read_delim(
    delim = ",",
    show_col_types = FALSE
  )

### The below code should work according to the nei-model, but doesn't. Unable to find source meta data for point source
# sources <- req_base %>%
#   httr2::req_url_path_append("BRS_SOURCE_NAME/CSV") %>%
#   httr2::req_method("GET") %>%
#   httr2::req_perform() %>%
#   httr2::resp_body_string(encoding = "UTF-8") %>%
#   readr::read_delim(
#     delim = ",",
#     show_col_types = FALSE
#   )

industrial_sector <- sectors %>% 
  filter(grepl("Industrial", ei_sector))

fetch_nei_county <- function(year, state) {
  req_base %>%
    # county sector summary table, all rows
    httr2::req_url_path_append("COUNTY_SECTOR_SUMMARY/ROWS/") %>%
    # state
    httr2::req_url_path_append(paste0("STATE_NAME/", state)) %>%
    # year
    httr2::req_url_path_append("INVENTORY_YEAR/", year, "/") %>%
    # in CSV format
    httr2::req_url_path_append("CSV") %>%
    # Go!
    httr2::req_perform() %>%
    # read response as CSV
    httr2::resp_body_string(encoding = "UTF-8") %>%
    readr::read_delim(
      delim = ",",
      show_col_types = FALSE
    )
}

multi_year_industrial_county <-
  bind_rows(
    purrr::map_dfr(
      c(2023,
        2020,
        2017,
        2014,
        2011,
        2008
      ),
      fetch_nei_county,
      state = "Minnesota"
    ),
    purrr::map_dfr(
      c(2023,
        2020,
        2017,
        2014,
        2011,
        2008
      ),
      fetch_nei_county,
      state = "Wisconsin"
    )
  ) %>% 
  filter(sector_code %in% industrial_sector$sector_code) %>% 
  right_join(., cprg_county %>% 
               select(state_name,county_name,geoid) %>% 
               st_drop_geometry(),
             by = c("county_name","state_name")) %>% 
  mutate(metric_tons_emissions = emissions * units::as_units("short_ton") %>%
      units::set_units("metric_ton"))


multi_year_industrial_county_ghg <- multi_year_industrial_county %>% 
  filter(pollutant_type == "GHG", emissions != 0) %>% 
  left_join(sectors) %>% 
  mutate(mt_co2e = case_when(
    pollutant_code == "CH4" ~ metric_tons_emissions * gwp$ch4,
    pollutant_code == "N2O" ~ metric_tons_emissions * gwp$n2o,
    pollutant_code == "SF6" ~ metric_tons_emissions *  23500, ## ADD TO GWP TABLE
    pollutant_code == "CO2" ~ metric_tons_emissions
  ))

nei_ind_county_emissions_out <- multi_year_industrial_county_ghg %>% 
  group_by(state_name ,
           inventory_year,
           county_name,
           sector_code,
           ei_sector,
           sector_one,
           sector_two,
           sector_three) %>% 
  summarize(values_emissions = sum(mt_co2e )) %>% 
  mutate(units_emissions = "Metric tons of CO2 equivalency")

nei_ind_county_emissions_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "state_name", class(nei_ind_county_emissions_out$facility_id), "State name",
    "county_name", class(nei_ind_county_emissions_out$city_name), "County name",
    "inventory_year", class(nei_ind_county_emissions_out$reporting_year), "Year of emissions",
    "sector_code", class(nei_ind_county_emissions_out$unit_name), "Industrial sector code",
    "ei_sector", class(nei_ind_county_emissions_out$unit_name), "Industrial sector description",
    "sector_one", class(nei_ind_county_emissions_out$general_fuel_type), "Industrial sector: Process, combustion, solvent",
    "sector_two", class(nei_ind_county_emissions_out$values_emissions), "Higher specificity of industrial sector",
    "sector_three", class(nei_ind_county_emissions_out$units_emissions), "Type of fuel combusted (where applicable)",
    "values_emissions", class(nei_ind_county_emissions_out$values_emissions), "Numerical value of emissions data",
    "units_emissions", class(nei_ind_county_emissions_out$units_emissions), "Units of emissions data"
  )

saveRDS(nei_ind_county_emissions_out, "./_industrial/data/nei_county_industrial_emissions.rds")
saveRDS(nei_ind_county_emissions_meta, "./_industrial/data/nei_county_industrial_emissions_meta.rds")

unique(multi_year_industrial_county_ghg$ei_sector)

multi_year_industrial_county_ghg %>% 
  filter(inventory_year == 2020) %>% 
  group_by(county_name) %>% 
  summarize(mt_co2e = sum(mt_co2e))

multi_year_industrial_county_ghg %>% 
  filter(inventory_year == 2020,
         county_name == "Washington") %>% 
  group_by(ei_sector, sector_code) %>% 
  summarize(mt_co2e = sum(mt_co2e))
#### NEC includes electricity generation, which is major double count



ggplot(multi_year_industrial_county_ghg %>% 
         group_by(inventory_year,county_name) %>% 
         summarize(mt_co2e = sum(mt_co2e)), 
       aes(x = inventory_year , y = mt_co2e , col = county_name)) + 
  geom_line()


flight_nei_county_all <- left_join(multi_year_industrial_county_ghg %>% 
                                     group_by(inventory_year,county_name) %>% 
                                     summarize(mt_co2e = sum(mt_co2e)),
                               flight_county_summary %>% 
                                 filter(doublecount == "Yes") %>% 
                                 group_by(county_name,inventory_year) %>% 
                                 summarize(co2e_double = sum(value_emissions)),
                               by = c("county_name", "inventory_year")) %>% 
  replace(is.na(.), 0) %>% 
  mutate(emissions_leftover = mt_co2e - co2e_double) %>% 
  left_join(., cprg_county %>% 
              select(county_name,geometry))


ggplot(flight_nei_county_all %>% 
         group_by(inventory_year,county_name) %>% 
         summarize(mt_co2e = sum(emissions_leftover)), 
       aes(x = inventory_year , y = mt_co2e , col = county_name)) + 
  geom_line(size = 1.3)


pal <- colorNumeric(palette = "Reds", domain = flight_nei_county_all$emissions_leftover)

# Create the leaflet map
leaflet(st_as_sf(flight_nei_county_all %>% 
          filter(inventory_year == 2020))) %>%
  addProviderTiles("CartoDB.Positron") %>%  # Basic map tile
  addPolygons(
    fillColor = ~pal(emissions_leftover),
    weight = 1,  # Boundary thickness
    color = "black",  # Boundary color
    fillOpacity = 0.7,  # Transparency
    label = ~paste0(county_name, ": ", emissions_leftover, " mt CO2e"),
    highlight = highlightOptions(weight = 2, color = "white", fillOpacity = 0.9)
  ) %>%
  addLegend(pal = pal, values = ~emissions_leftover, opacity = 0.8, title = "MT CO2e")


flight_nei_county_small <- left_join(multi_year_industrial_county_ghg %>% 
                                     group_by(inventory_year,county_name) %>% 
                                     summarize(mt_co2e = sum(mt_co2e)),
                                   flight_county_summary %>% 
                                     group_by(county_name,inventory_year) %>% 
                                     summarize(co2e_double = sum(value_emissions)),
                                   by = c("county_name", "inventory_year")) %>% 
  replace(is.na(.), 0) %>% 
  mutate(emissions_leftover = mt_co2e - co2e_double)

ggplot(flight_nei_county_small %>% 
         group_by(inventory_year,county_name) %>% 
         summarize(tons_co2 = sum(emissions_leftover)), 
       aes(x = inventory_year , y = tons_co2 , col = county_name)) + 
  geom_line(size = 1.3)
