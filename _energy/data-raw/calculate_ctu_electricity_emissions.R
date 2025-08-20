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
  summarize(mt_co2e_mwh = sum(mt_co2e_mwh), .groups = "keep") %>%
  ungroup() %>%
  rename(inventory_year = Year)


### read in coctu data. Will use county proportions to cast backwards

coctu_busi <- read_rds("_energy/data-raw/predicted_coctu_business_mwh.rds") %>% 
  mutate(sector = "Business") %>% 
  rename(mwh = business_mwh)
coctu_res <- read_rds("_energy/data-raw/predicted_coctu_residential_mwh.rds") %>% 
  mutate(sector = "Residential")%>% 
  rename(mwh = residential_mwh)

coctu_mwh <- bind_rows(coctu_busi, coctu_res)

county_mwh <- readRDS(file.path(here::here("_energy", "data", "county_elec_activity.RDS")))


#find mean county proportion for known years
coctu_prop <- coctu_mwh %>% 
  left_join(county_mwh %>% 
              select(inventory_year = year,
                     county_name,
                     mwh_county = activity),
            by = join_by(county_name, inventory_year)) %>% 
  mutate(mwh_prop = mwh/mwh_county) %>% 
  group_by(ctu_name, ctu_class, county_name, sector) %>% 
  summarize(mean_mwh_prop = mean(mwh_prop), .groups = "keep")

#apply average county proportion to yearly county data for unknown ctu years
coctu_pred <- county_mwh %>% 
  select(inventory_year = year,
         county_name,
         mwh_county = activity)%>% 
  left_join(coctu_prop, by = "county_name", relationship = "many-to-many") %>% 
  mutate(mwh = mwh_county * mean_mwh_prop) %>% 
  anti_join(coctu_res, #remove known years (will report utility data where possible)
            by = c("ctu_name", "ctu_class", "county_name", "inventory_year")) %>% 
  mutate(data_source = "Modeled county proportion") %>% 
  select(-c(mwh_county, mean_mwh_prop))

#combine county props and ctu utility data
ctu_full <- bind_rows(coctu_pred,
                           coctu_mwh %>% 
                             select(-coctu_id_gnis)
) %>% 
  group_by(ctu_name, ctu_class, sector, inventory_year, data_source) %>% 
  summarize(mwh = sum(mwh), .groups = "keep") %>% 
  mutate(
         category = "Electricity",
         source = "Electricity") %>% 
  ungroup()

ctu_emissions <- ctu_full %>%
  left_join(egrid_temporal,
    by = "inventory_year"
  ) %>%
  mutate(
    value_emissions = round(mwh * mt_co2e_mwh, digits = 2),
    units_emissions = "Metric tons CO2e",
    factor_source = Source
  )

saveRDS(ctu_emissions, "_energy/data/_ctu_electricity_emissions.RDS")
