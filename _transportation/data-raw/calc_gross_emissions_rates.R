source("R/_load_pkgs.R")
rtdm_forecast_ctu <- readRDS(file.path(here::here(), "_transportation/data/rtdm_forecast_ctu.RDS"))
rtdm_forecast_county <- readRDS(file.path(here::here(), "_transportation/data/rtdm_forecast_county.RDS"))
county_emissions_forecast_meta <- readRDS("_transportation/data/rtdm_county_emissions_forecast_meta.RDS")


county_inventory_forecast_emissions <- readRDS(file.path(here::here(), "_transportation/data/county_inventory_forecast_emissions.RDS")) %>%
  rowwise() %>%
  mutate(rounded_tons = round_emissions_metric_tons_co2e(emissions_metric_tons_co2e))


# read in VMT data
dot_vmt <- readRDS(file.path(here::here(), "_transportation/data/dot_vmt.RDS")) %>%
  filter(
    # vmt_year < 2022,
    vmt_year >= 2010,
    cprg_area == TRUE
  ) %>%
  group_by(county_name)


rtdm_vmt_time_series <- dot_vmt %>% 
  mutate(vmt_year = as.numeric(vmt_year)) %>% 
  filter(vmt_year <= min(rtdm_forecast_county$vmt_year)) %>% 
  select(vmt_year, geoid, county_name, annual_vmt, vmt_data_source = data_source) %>% 
  bind_rows(
    # bind forecasted county VMT
    rtdm_forecast_county %>% 
      mutate(annual_vmt = network_vmt_annual,
             vmt_data_source = "RTDM")
  ) %>% 
  select(1:5)


emissions_per_mile <- rtdm_vmt_time_series %>% 
  filter(geoid %in% county_inventory_forecast_emissions$geoid) %>% 
  left_join(county_inventory_forecast_emissions %>% 
              select(geoid, county_name, emissions_year, emissions_metric_tons_co2e, 
                     emissions_data_source = data_source),
            by = join_by(geoid, county_name, vmt_year == emissions_year)) %>% 
  mutate(emissions_per_mile = (emissions_metric_tons_co2e / annual_vmt) %>% 
           units::as_units("metric_ton") %>% 
           units::set_units("gram") %>% 
           as.numeric() %>% 
           round(digits = 3)) %>% 
  select(geoid, county_name, vmt_year, vmt_data_source, emissions_data_source, emissions_per_mile) %>% 
  unique()



emissions_per_mile_meta <- county_emissions_forecast_meta %>%
  filter(Column %in% names(emissions_per_mile)) %>%
  bind_rows(
    tribble(
      ~Column, ~Class, ~Description,
      "vmt_year", class(emissions_per_mile$vmt_year), "VMT forecast year",
      "vmt_data_source", class(emissions_per_mile$vmt_data_source), "VMT data source for given county and year",
      "emissions_data_source", class(emissions_per_mile$emissions_data_source), "Emissions data source for given county and year",
      "emissions_per_mile", class(emissions_per_mile$emissions_per_mile), "Grams CO~2~ and CO~2~ equivalent per vehicle mile traveled"
    )
  ) %>% 
  arrange(match(Column, names(emissions_per_mile)))

saveRDS(emissions_per_mile, "_transportation/data/gross_emissions_rates.RDS")
saveRDS(emissions_per_mile_meta, "_transportation/data/gross_emissions_rates_meta.RDS")
