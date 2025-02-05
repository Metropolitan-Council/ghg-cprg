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

natgas_ef_scf <- readRDS("_meta/data/epa_ghg_factor_hub.RDS") %>% 
  pluck("stationary_combustion") %>%
  filter(fuel_category == "Natural Gas" & per_unit == "scf") %>% 
  mutate(# metric ton co2e per mwh 
    mt_co2e_scf = case_when(
      emission == "g CH4" ~ value * gwp$ch4 %>%
        units::as_units("gram") %>%
        units::set_units("metric_ton") %>%
        as.numeric(),
      emission == "g N2O" ~ value * gwp$n2o %>%
        units::as_units("gram") %>%
        units::set_units("metric_ton") %>%
        as.numeric(),
      emission == "kg CO2" ~ value %>%
        units::as_units("kilogram") %>%
        units::set_units("metric_ton") %>%
        as.numeric(),
      TRUE ~ 0
    )) %>%
  # get rid of unnecessary columns from eGRID factor tables
  group_by(fuel_category, Source) %>% 
  summarize(mt_co2e_scf = sum(mt_co2e_scf)) %>% 
  ungroup()

electric_raw <- readRDS(file.path(here:: here("_energy", "data", "minnesota_county_elec_ActivityAndEmissions.RDS")))%>%
  # bind_rows(readRDS(file.path(here::here(), "_energy/data/wisconsin_county_ElecEmissions.RDS")) %>%
  #             rename(county = county_name)) %>% 
  select(year, county, mwh = total_mWh_delivered, sector) %>% 
  #get 2005 MN data
  bind_rows(readRDS(file.path(here::here(), "_energy/data/minnesota_county_ElecEmissions.RDS")) %>% 
              filter(year == 2005) %>% 
              select(year, county, mwh = total_mWh, sector))

electric_interpolated <- left_join(
  expand.grid(
    year = 2005:2023,
    county = unique(electric_raw$county),
    sector = "Electricity"
  ),
  electric_raw) %>% 
  mutate(mwh_modeled = na_kalman(mwh),
         data_source = if_else(is.na(mwh), "Interpolated", "Utility report")) %>% 
  left_join(egrid_temporal, by = c("year" = "Year")) %>% 
  mutate(value_emissions = mt_co2e_mwh * mwh_modeled,
         unit_emissions = "Metric tons CO2e",
         activity_type = "mWh delivered") %>% 
  select(year,
         county_name = county,
         sector,
         activity = mwh_modeled,
         activity_type,
         data_source,
         factor_source = Source,
         value_emissions,
         unit_emissions)

ggplot(electric_interpolated, aes(x = year, y = mwh_modeled, col = county)) +
  geom_line()

natgas_raw <- readRDS(file.path(here:: here("_energy", "data", "minnesota_county_GasEmissions.RDS"))) %>%
  bind_rows(readRDS(file.path(here::here(), "_energy/data/wisconsin_county_GasEmissions.RDS")))

### weird modeling happening for WI counies where CO2e is avail but scf is not. Check.
natgas_interpolated <- left_join(
  expand.grid(
    year = 2005:2023,
    county_name = unique(natgas_raw$county_name),
    sector = "Natural gas"
  ),
  natgas_raw) %>% 
  mutate(mcf_modeled = na_kalman(total_mcf),
         data_source = if_else(is.na(total_mcf), "Interpolated", "Utility report")) %>% 
  cross_join(natgas_ef_scf) %>% 
  mutate(value_emissions = mt_co2e_scf * mcf_modeled * 10^3,
         unit_emissions = "Metric tons CO2e",
         activity_type = "mcf delivered") %>% 
  select(year,
         county_name,
         sector,
         activity = mcf_modeled,
         activity_type,
         data_source,
         factor_source = Source,
         value_emissions,
         unit_emissions)

ggplot(natgas_interpolated, aes(x = year, y = mcf_modeled, col = county_name)) +
  geom_line()

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
