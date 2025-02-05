# use nrel proportions to breakout county energy deliveries
# backcast where possible but be clear about interpolation/data weaknesses
source("R/global_warming_potential.R")


nrel_emissions <- readRDS("_energy/data-raw/nrel_slope/nrel_emissions_inv_county.RDS")
egrid_temporal <- readRDS("_meta/data/epa_ghg_factor_hub.RDS") %>% 
  pluck("egridTimeSeries") %>%
  mutate(# metric ton co2e per mwh 
    mt_co2e_mwh = case_when(
      emission == "lb CH4" ~ value * gwp$ch4 %>%
      units::as_units("pound") %>%
      units::set_units("metric_ton") %>%
      as.numeric(),
      emission == "lb N2O" ~ value * gwp$n2o %>%
        units::as_units("pound") %>%
        units::set_units("metric_ton") %>%
        as.numeric(),
      emission == "lb CO2" ~ value %>%
        units::as_units("pound") %>%
        units::set_units("metric_ton") %>%
        as.numeric()
  )) %>%
  # get rid of unnecessary columns from eGRID factor tables
  group_by(Year, Source) %>% 
  summarize(mt_co2e_mwh = sum(mt_co2e_mwh)) %>% 
  ungroup()

electric_raw <- readRDS(file.path(here:: here("_energy", "data", "minnesota_county_elec_ActivityAndEmissions.RDS")))%>%
  # bind_rows(readRDS(file.path(here::here(), "_energy/data/wisconsin_county_ElecEmissions.RDS")) %>%
  #             rename(county = county_name)) %>% 
  select(year, county, mwh = total_mWh_delivered, sector) %>% 
  #get 2005 MN data
  bind_rows(readRDS(file.path(here::here(), "_energy/data/minnesota_county_ElecEmissions.RDS")) %>% 
              select(year, county, mwh = total_mWh, sector))

electric_interpolated <- left_join(
  expand.grid(
    year = 2005:2023,
    county = unique(electric_raw$county),
    sector = "Electricity"
  ),
  electric_raw) %>% 
  mutate(mwh_modeled = na_kalman(mwh))

ggplot(electric_interpolated, aes(x = year, y = mwh_modeled, col = county)) +
  geom_line()

natgas_raw <- readRDS(file.path(here:: here("_energy", "data", "minnesota_county_GasEmissions.RDS"))) %>%
  bind_rows(readRDS(file.path(here::here(), "_energy/data/wisconsin_county_GasEmissions.RDS")) %>%
              rename(county = county_name))



nrel_proportions <- nrel_emissions %>% 
  group_by(county_name, source, year) %>% 
  mutate(total_co2e = sum(co2e),
         sector_proportion = co2e / total_co2e) %>% 
  select(county_name, source, sector_raw, year, sector_proportion)

  
electric_natgas_nrel_proportioned <- electric_raw %>%
  bind_rows(natgas_raw) %>%
  select(county, source = sector, year, emissions_metric_tons_co2e) %>%
  mutate(source = str_to_sentence(source)) %>%
  right_join(nrel_proportions,
            by = c("county" = "county_name", "source" = "source", "year")
  ) %>%
  mutate(
    sector_co2e = sector_proportion * emissions_metric_tons_co2e
  ) %>%
  pivot_longer(4:7,
               names_to = "category",
               values_to = "emissions_metric_tons_co2e"
  ) %>%
  mutate(category = str_to_sentence(category))

waldo::compare(electric_natgas_nrel_proportioned, readRDS("_energy/data/electric_natgas_nrel_proportioned.RDS"))
saveRDS(electric_natgas_nrel_proportioned, "_energy/data/electric_natgas_nrel_proportioned.RDS")
