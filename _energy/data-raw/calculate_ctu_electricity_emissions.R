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


### read in activity data
ctu_mwh_res <- read_rds("_energy/data-raw/predicted_ctu_residential_mwh.RDS")
ctu_mwh_busi <- read_rds("_energy/data-raw/predicted_ctu_business_mwh.RDS")

ctu_mwh <- bind_rows(
  ctu_mwh_res %>%
    rename(
      mwh_predicted = residential_mwh_predicted,
      mwh_reported = residential_mwh
    ) %>%
    mutate(sector = "Residential"),
  ctu_mwh_busi %>%
    rename(
      mwh_predicted = business_mwh_predicted,
      mwh_reported = business_mwh
    ) %>%
    mutate(sector = "Nonresidential")
)

ctu_emissions <- ctu_mwh %>%
  left_join(egrid_temporal,
    by = "inventory_year"
  ) %>%
  mutate(
    value_emissions = if_else(is.na(mwh_reported),
      mwh_predicted * mt_co2e_mwh,
      mwh_reported * mt_co2e_mwh
    ),
    units_emissions = "Metric tons CO2e",
    factor_source = Source,
    data_source = if_else(is.na(mwh_reported),
      "Model prediction",
      "Utility report"
    )
  ) %>%
  select(-c(mwh_predicted, mwh_reported, Source, mt_co2e_mwh))


saveRDS(ctu_emissions, "_energy/data/_ctu_electricity_emissions.RDS")
