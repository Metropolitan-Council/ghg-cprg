#### pull in ctu mwh and convert to emissions
source("R/_load_pkgs.R")
source("R/global_warming_potential.R")

### read in emissions factor

egrid_temporal <- readRDS("_meta/data/epa_ghg_factor_hub.RDS") %>%
  pluck("egridTimeSeries") %>%
  mutate( # metric ton co2e per mwh
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
    )
  ) %>%
  # get rid of unnecessary columns from eGRID factor tables
  group_by(Year, Source) %>%
  summarize(mt_co2e_mwh = sum(mt_co2e_mwh)) %>%
  ungroup() %>%
  rename(inventory_year = Year)


### read in coctu data. Will use county proportions to cast backwards

coctu_busi <- read_rds("_energy/data-raw/predicted_coctu_business_mwh.rds")
coctu_res <- read_rds("_energy/data-raw/predicted_coctu_residential_mwh.rds")

cty_busi <- read_rds("_energy/data-raw/predicted_county_business_mwh.rds")
cty_res <- read_rds("_energy/data-raw/predicted_county_residential_mwh.rds")

#find mean county proportion for
coctu_busi_prop <- coctu_busi %>% 
  left_join(cty_busi) %>% 
  mutate(business_mwh_prop = business_mwh/business_mwh_predicted) %>% 
  group_by(ctu_name, ctu_class, county_name) %>% 
  summarize(mean_business_mwh_prop = mean(business_mwh_prop))

coctu_busi_pred <- cty_busi %>% 
  left_join(coctu_busi_prop) %>% 
  mutate(coctu_busi_mwh_pred = business_mwh_predicted * mean_business_mwh_prop) %>% 
  anti_join(coctu_busi,
            by = c("ctu_name", "ctu_class", "county_name", "inventory_year"))

ctu_busi_full <- bind_rows(coctu_busi_pred)

ctu_busi_predict <- read_rds("_energy/data-raw/forecast_ctu_business_mwh.rds") %>%
  filter(inventory_year <= 2023) %>%
  mutate(sector = "Business") %>%
  rename(mwh = business_mwh)
ctu_res_predict <- read_rds("_energy/data-raw/forecast_ctu_residential_mwh.rds") %>%
  filter(inventory_year <= 2023) %>%
  mutate(sector = "Residential") %>%
  rename(mwh = residential_mwh)


ctu_mwh <- bind_rows(
  ctu_res_predict,
  ctu_busi_predict
)

ctu_emissions <- ctu_mwh %>%
  left_join(egrid_temporal,
    by = "inventory_year"
  ) %>%
  mutate(
    value_emissions = mwh * mt_co2e_mwh,
    units_emissions = "Metric tons CO2e",
    factor_source = Source
  )


saveRDS(ctu_emissions, "_energy/data/_ctu_electricity_emissions.RDS")
