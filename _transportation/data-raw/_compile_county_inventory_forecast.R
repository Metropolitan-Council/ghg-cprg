# compile historic and forecast onroad emissions -----
# county level, from 2002 to 2050

source("R/_load_pkgs.R")
onroad_emissions <- readRDS("_transportation/data/onroad_emissions.RDS")
county_emissions_forecast_meta <- readRDS("_transportation/data/rtdm_county_emissions_forecast_meta.RDS")

onroad_emissions_forecast <- readRDS("_transportation/data/rtdm_county_emissions_forecast.RDS") %>%
  filter(inventory_year != 2023) %>%
  mutate(
    data_source = "RTDM and EPA MOVES4",
    emissions_year = inventory_year
  ) %>%
  group_by(county_name, geoid, emissions_year, data_source) %>%
  summarize(
    emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e),
    .groups = "keep"
  ) %>%
  select("county_name", "geoid", "emissions_year", "data_source", "emissions_metric_tons_co2e")


onroad_emissions_summary <- onroad_emissions %>%
  unique() %>%
  filter(geoid %in% onroad_emissions_forecast$geoid) %>%
  group_by(county_name, geoid, emissions_year, data_source) %>%
  summarize(
    emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e),
    .groups = "keep"
  )


county_emissions_hist_forecast <- onroad_emissions_summary %>%
  bind_rows(onroad_emissions_forecast) %>%
  arrange(county_name, emissions_year)



co_year_series <- county_emissions_hist_forecast %>%
  ungroup() %>%
  select(geoid, county_name) %>%
  unique() %>%
  expand_grid(emissions_year = 2002:2050)


county_emissions <- county_emissions_hist_forecast %>%
  right_join(co_year_series,
    by = join_by(county_name, geoid, emissions_year)
  ) %>%
  group_by(county_name) %>%
  arrange(county_name, emissions_year) %>%
  mutate(
    emissions_metric_tons_co2e = zoo::na.approx(emissions_metric_tons_co2e),
    data_source = ifelse(is.na(data_source), "Interpolation", data_source)
  )


write.csv(county_emissions, "_transportation/data-raw/county_emissions_time_series.CSV",
  row.names = FALSE
)

county_emissions_meta <- county_emissions_forecast_meta %>%
  filter(Column %in% names(county_emissions)) %>%
  bind_rows(
    tribble(
      ~Column, ~Class, ~Description,
      "emissions_year", class(county_emissions$emissions_year), "VMT forecast year",
      "data_source", class(county_emissions$data_source), "Data source for given county and year"
    )
  )

saveRDS(county_emissions, "_transportation/data/county_inventory_forecast_emissions.RDS")
saveRDS(county_emissions_meta, "_transportation/data/county_inventory_forecast_emissions_meta.RDS")
