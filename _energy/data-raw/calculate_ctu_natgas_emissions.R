#### pull in ctu mcf and convert to emissions
source("R/_load_pkgs.R")
source("R/global_warming_potential.R")

### read in emissions factor

natgas_ef_scf <- readRDS("_meta/data/epa_ghg_factor_hub.RDS") %>%
  pluck("stationary_combustion") %>%
  filter(fuel_category == "Natural Gas" & per_unit == "scf") %>%
  mutate( # metric ton co2e per mwh
    mt_co2e_mcf = 10^3 * case_when(
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
    )
  ) %>%
  # get rid of unnecessary columns from eGRID factor tables
  group_by(fuel_category, Source) %>%
  summarize(mt_co2e_mcf = sum(mt_co2e_mcf), .groups = "keep") %>%
  ungroup()


### read in coctu data. Will use county proportions to cast backwards

coctu_busi <- read_rds("_energy/data-raw/predicted_coctu_business_mcf.rds") %>% 
  mutate(sector = "Business") %>% 
  rename(mcf = business_mcf)
coctu_res <- read_rds("_energy/data-raw/predicted_coctu_residential_mcf.rds") %>% 
  mutate(sector = "Residential")%>% 
  rename(mcf = residential_mcf)

coctu_mcf <- bind_rows(coctu_busi, coctu_res)

county_mcf <- readRDS(file.path(here::here("_energy", "data", "county_natgas_activity.RDS")))


#find mean county proportion for known years
coctu_prop <- coctu_mcf %>% 
  left_join(county_mcf %>% 
              select(inventory_year = year,
                     county_name,
                     mcf_county = activity),
            by = join_by(county_name, inventory_year)) %>% 
  mutate(mcf_prop = mcf/mcf_county) %>% 
  group_by(ctu_name, ctu_class, county_name, sector) %>% 
  summarize(mean_mcf_prop = mean(mcf_prop), .groups = "keep")

#apply average county proportion to yearly county data for unknown ctu years
coctu_pred <- county_mcf %>% 
  select(inventory_year = year,
         county_name,
         mcf_county = activity)%>% 
  left_join(coctu_prop, by = "county_name", relationship = "many-to-many") %>% 
  mutate(mcf = mcf_county * mean_mcf_prop) %>% 
  anti_join(coctu_res, # remove known years (will report utility data where possible)
            by = c("ctu_name", "ctu_class", "county_name", "inventory_year")) %>% 
  mutate(data_source = "Modeled county proportion") %>% 
  select(-c(mcf_county, mean_mcf_prop))

#combine county props and ctu utility data
ctu_full <- bind_rows(coctu_pred,
                      coctu_mcf %>% 
                        select(-coctu_id_gnis)
) %>% 
  group_by(ctu_name, ctu_class, sector, inventory_year, data_source) %>% 
  summarize(mcf = sum(mcf),.groups = "keep") %>% 
  mutate(
    category = "Building Energy",
    source = "Natural Gas") %>% 
  ungroup()

ctu_emissions <- ctu_full %>%
  cross_join(natgas_ef_scf %>% 
               select(factor_source = Source,
                      mt_co2e_mcf)) %>%
  mutate(
    value_emissions = round(mcf * mt_co2e_mcf, digits = 2),
    units_emissions = "Metric tons CO2e"
  )

saveRDS(ctu_emissions, "_energy/data/_ctu_natgas_emissions.RDS")
