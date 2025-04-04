### Develop model for predicting CTU business electricity usage ###

source("R/_load_pkgs.R")
source("_energy/data-raw/_energy_emissions_factors.R")

## load in supporting data
cprg_ctu <- read_rds("_meta/data/cprg_ctu.RDS") %>%
  filter(!county_name %in% c("Chisago", "Sherburne", "St. Croix", "Pierce")) 
cprg_county <- read_rds("_meta/data/cprg_county.RDS") %>%
  filter(!county_name %in% c("Chisago", "Sherburne", "St. Croix", "Pierce")) 
ctu_population <- read_rds("_meta/data/ctu_population.RDS") %>% 
  left_join(cprg_county %>% st_drop_geometry() %>% select(geoid, county_name)) %>%
  filter(!county_name %in% c("Chisago", "Sherburne", "St. Croix", "Pierce")) 

## load in utility data, keeping only complete city-years and collapse to city-year
## i.e. if city-year is missing ANY utility data, delete

#bring in weather data
#weather data
noaa <- readRDS("_meta/data/noaa_weather_monthly.rds")

noaa_year <- noaa %>% 
  group_by(inventory_year) %>% 
  summarize(heating_degree_days = sum(heating_degree_days),
            cooling_degree_days = sum(cooling_degree_days),
            temperature = mean(dry_bulb_temp))

ctu_utility_year <- read_rds("_energy/data/ctu_utility_mwh.RDS") %>% 
  group_by(ctu_name, ctu_class, inventory_year) %>%
  filter(!any(is.na(total_mwh))) %>%
  summarize(
    residential_mwh = sum(residential_mwh, na.rm = TRUE),
    business_mwh = sum(business_mwh, na.rm = TRUE),
    total_mwh = sum(total_mwh)) %>% 
  ungroup() 

urbansim <- readRDS("_meta/data/urbansim_data.RDS")
mn_parcel <- readRDS("_meta/data/ctu_parcel_data_2021.RDS")

#### BUSINESS ####


business <- c(
  "total_job_spaces",
  "js_type_1011",
  "js_type_13",
  "js_type_14",
  "zones_total_jobs_20_minutes_tt",
  "zones_total_jobs_45_minutes_tt",
  "max_industrial",
  "js_type_12",
  "jobs_sector_4",
  "jobs_sector_5",
  "jobs_sector_6",
  "jobs_sector_7",
  "jobs_sector_8",
  "jobs_sector_9",
  "jobs_sector_10",
  "js_type_12",
  "jobs_sector_1",
  "jobs_sector_2",
  "jobs_sector_3"
)

### create 2010-2025 urbansim residential dataset
urbansim_busi <- urbansim %>%
  filter(variable %in% business) %>%
  group_by(coctu_id, variable) %>% 
  complete(inventory_year = full_seq(c(min(inventory_year), 2025), 1)) %>% # add interstitial years and expand to 2025
  arrange(coctu_id, variable, inventory_year) %>% 
  mutate(value = approx(inventory_year, value, inventory_year, method = "linear", rule = 2)$y) %>% # allow extrapolation
  ungroup() %>% 
  pivot_wider(
    id_cols = c(coctu_id, inventory_year),
    names_from = variable,
    values_from = value
  ) %>%
  filter(!is.na(coctu_id)) %>% 
  mutate(ctu_id = str_sub(coctu_id, -7, -1),
         county_id = as.numeric(str_remove(coctu_id, paste0("0", ctu_id))),
         ctu_id = as.numeric(ctu_id)
  ) %>% 
  left_join(cprg_ctu %>% st_drop_geometry() %>% 
              distinct(ctu_name, thrive_designation, gnis),
            by = c("ctu_id" = "gnis")) %>% 
  left_join(cprg_county %>% st_drop_geometry() %>% 
              mutate(geoid = as.numeric(str_sub(geoid, -3, -1))) %>% 
              select(county_name, geoid),
            by = c("county_id" = "geoid"))

#create jobs proportions for coctu
coctu_jobs <- urbansim_busi %>%
  distinct(ctu_name, ctu_id, inventory_year, county_name, total_job_spaces) %>% # Ensure unique rows per city-county-year
  group_by(ctu_name, ctu_id, inventory_year) %>%
  mutate(
    ctu_class = if_else(nchar(ctu_id) == 7, "CITY", "TOWNSHIP"),
    total_ctu_jobs = sum(total_job_spaces, na.rm = TRUE), # Sum populations across counties for each city-year
    coctu_jobs_prop = total_job_spaces / total_ctu_jobs,
    multi_county = n_distinct(county_name) > 1
  ) %>%
  ungroup()

## split out business mwh based on job proportions for cities in two counties
coctu_busi_year <- ctu_utility_year %>%
  # Join city_total_population back to main dataset
  full_join(coctu_jobs,
            by = c("ctu_name", "ctu_class", "inventory_year"),
            relationship = "many-to-many"
  ) %>%
  # Calculate proportions and disaggregated values
  group_by(ctu_name, ctu_class, inventory_year, county_name) %>%
  mutate(
    business_mwh = ifelse(
      multi_county,
      business_mwh  * coctu_jobs_prop,
      business_mwh)
  ) %>%
  ungroup() %>% 
  filter(!is.na(business_mwh)) %>% 
  select(ctu_name, ctu_class, inventory_year, business_mwh, county_name)

# grab mn parcel data for business
mn_parcel_busi <- mn_parcel %>%
  filter(mc_classification %in% c(
    "commercial",
    "industrial",
    "public_building"
  )) %>%
  group_by(ctu_name, ctu_id, mc_classification) %>%
  summarize(total_emv = sum(total_emv), mean_year = mean(mean_year)) %>%
  pivot_wider(
    id_cols = c(ctu_name, ctu_id),
    names_from = mc_classification,
    values_from = c(total_emv, mean_year)
  ) %>%
  na_replace() %>%
  ungroup()

# merge into utility data
electricity_busi <- left_join(coctu_busi_year,
                             urbansim_busi,
                             by = c("ctu_name", "county_name", "inventory_year")
) %>%
  left_join(mn_parcel_busi %>% select(-ctu_name),
            by = c("ctu_id" = "ctu_id")
  ) %>% 
  #weather data
  left_join(noaa_year, by = "inventory_year")

rf_nonres_model <- randomForest(
  business_mwh ~
    thrive_designation +
    total_job_spaces +
    js_type_1011 +
    js_type_13 +
    js_type_14 +
    total_job_spaces +
    max_industrial +
    js_type_12 +
    jobs_sector_4 +
    jobs_sector_5 +
    jobs_sector_6 +
    jobs_sector_7 +
    jobs_sector_8 +
    jobs_sector_9 +
    jobs_sector_10 +
    jobs_sector_1 +
    jobs_sector_2 +
    jobs_sector_3 +
    cooling_degree_days,
  importance = T, data = electricity_busi
)

rf_nonres_model
p_full <- predict(rf_nonres_model, electricity_busi)
plot(p_full, electricity_busi$business_mwh)
abline(0, 1)

### predict ALL cities and rollback up to counties for all years

ctu_busi_predict <- cprg_ctu %>%
  left_join(urbansim_busi, by = c("gnis" = "ctu_id",
                                 "ctu_name",
                                 "county_name",
                                 "thrive_designation")) %>%
  left_join(noaa_year) %>% 
  mutate(mwh_predicted = predict(rf_nonres_model, .)) %>%
  filter(!is.na(mwh_predicted)) %>%  #removes 2025 data
  st_drop_geometry() %>% 
  group_by(ctu_name, ctu_class, inventory_year) %>% 
  summarize(business_mwh_predicted = sum(mwh_predicted)) %>% 
  ungroup()



ctu_busi_predict %>% distinct(ctu_name, ctu_class)

ctu_busi <- left_join(ctu_busi_predict,
                     ctu_utility_year %>% 
                       select(1:3,5))

### county

county_busi_predict <- cprg_ctu %>%
  left_join(urbansim_busi, by = c("gnis" = "ctu_id",
                                  "ctu_name",
                                  "county_name",
                                  "thrive_designation")) %>%
  left_join(noaa_year) %>% 
  mutate(mwh_predicted = predict(rf_nonres_model, .)) %>%
  filter(!is.na(mwh_predicted)) %>%  #removes 2025 data
  st_drop_geometry() %>% 
  group_by(county_name, inventory_year) %>% 
  summarize(business_mwh_predicted = sum(mwh_predicted)) %>% 
  ungroup()

#save intermediate rds
saveRDS(ctu_busi, "_energy/data-raw/predicted_ctu_business_mwh.rds")
saveRDS(county_busi_predict, "_energy/data-raw/predicted_county_business_mwh.rds")


### look at total mwh

mwh_county <- read_rds("_energy/data/minnesota_elecUtils_ActivityAndEmissions.RDS")

mwh_xcel_county <- mwh_county %>%
  filter(!is.na(mWh_delivered), utility == "XcelEnergy") %>%
  mutate(source = "Xcel County Report") %>%
  select(county_name = county, mwh = mWh_delivered, source) %>%
  filter(!county_name %in% c("Sherburne", "Chisago"))

xcel_ctu_county <- xcel %>%
  left_join(cprg_ctu, by = c("ctu_class", "ctu_name")) %>%
  filter(year == 2021, !is.na(county_name)) %>%
  group_by(county_name) %>%
  summarize(mwh = sum(mWh_delivered, na.rm = TRUE)) %>%
  mutate(source = "Xcel CTU Report") %>%
  select(county_name, mwh, source)

ggplot(
  rbind(mwh_xcel_county, xcel_ctu_county),
  aes(x = county_name, y = mwh, fill = source)
) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7)

mn_mwh <- read_rds("_energy/data/minnesota_elecUtils_ActivityAndEmissions.RDS") %>%
  group_by(county) %>%
  summarize(mwh = sum(mWh_delivered, na.rm = TRUE)) %>%
  mutate(
    sector = "Total",
    source = "Utility Report"
  ) %>%
  filter(!county %in% c("Chisago", "Sherburne", "St. Croix", "Pierce"))


### bring in NREL splits
nrel <- read_rds("_energy/data-raw/nrel_slope/nrel_slope_proportions.RDS")

mn_mwh_sector <- rbind(
  left_join(mn_mwh, nrel %>%
    filter(
      year == 2021,
      source == "Electricity"
    ),
  by = "county"
  ) %>%
    mutate(
      mwh = mwh * residential,
      sector = "Residential"
    ) %>%
    select(county, mwh, sector, source = source.x),
  left_join(mn_mwh, nrel %>%
    filter(
      year == 2021,
      source == "Electricity"
    ),
  by = "county"
  ) %>%
    mutate(
      mwh = mwh * (commercial + industrial),
      sector = "Commercial/Industrial"
    ) %>%
    select(county, mwh, sector, source = source.x)
)

ctu_mwh_predict <- rbind(
  ctu_res_predict %>% st_drop_geometry() %>%
    select(
      ctu_name = ctu_name.x,
      mwh_predicted,
      county_name
    ) %>%
    mutate(sector = "Residential"),
  ctu_nonres_predict %>% st_drop_geometry() %>%
    select(
      ctu_name = ctu_name.x,
      mwh_predicted,
      county_name
    ) %>%
    mutate(sector = "Commercial/Industrial")
) %>%
  mutate(source = "MC Model - RF")


mwh <- rbind(mn_mwh_sector, ctu_mwh_predict %>%
  group_by(county_name, sector, source) %>%
  summarize(mwh = sum(mwh_predicted)) %>%
  rename(county = county_name))

ggplot(mwh, aes(x = source, y = mwh, fill = sector)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.7) +
  facet_wrap(~county, scales = "free_y") + # Side-by-side sources
  # scale_fill_manual(values = c("Total" = "blue", "Commercial/Industrial" = "orange")) + # Customize colors
  labs(
    title = "Electricity Usage by County",
    x = "County",
    y = "MWh",
    fill = "Sector"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )



varImpPlot(rf_nonres_model)

nonres_simple <- lm(mWh_delivered ~ total_job_spaces, data = electricity_nonres)
summary(nonres_simple) # R2 = 0.9704

plot(mWh_delivered ~ total_job_spaces, data = electricity_nonres)
abline(nonres_simple)

ctu_predict_linear <- rbind(
  cprg_ctu %>%
    left_join(urbansim_res, by = c("gnis" = "ctu_id")) %>%
    left_join(mn_parcel_res, by = c("gnis" = "ctu_id", "ctu_name")) %>%
    mutate(mwh_predicted = predict(res_simple, .)) %>%
    mutate(sector = "Residential") %>%
    select(ctu_name, county_name, mwh_predicted, sector),
  cprg_ctu %>%
    left_join(urbansim_nonres, by = c("gnis" = "ctu_id")) %>%
    # left_join(mn_parcel_res, by = c("gnis" = "ctu_id")) %>%
    mutate(mwh_predicted = predict(nonres_simple, .)) %>%
    mutate(sector = "Commercial/Industrial") %>%
    select(ctu_name, county_name, mwh_predicted, sector)
)


county_predict_linear <- ctu_predict_linear %>%
  filter(!is.na(mwh_predicted)) %>%
  st_drop_geometry() %>%
  group_by(county_name, sector) %>%
  summarize(mwh_predicted = sum(mwh_predicted)) %>%
  mutate(
    # apply emission factor and convert to metric tons
    co2 = (mwh_predicted * eGRID_MROW_emissionsFactor_CO2_2021) %>%
      units::as_units("lb") %>%
      units::set_units("ton") %>%
      as.numeric(),
    ch4 = (mwh_predicted * eGRID_MROW_emissionsFactor_CH4_2021) %>%
      units::as_units("lb") %>%
      units::set_units("ton") %>%
      as.numeric(),
    n2o = (mwh_predicted * eGRID_MROW_emissionsFactor_N2O_2021) %>%
      units::as_units("lb") %>%
      units::set_units("ton") %>%
      as.numeric(),
    co2e =
      co2 +
        (ch4 * gwp$n2o) +
        (n2o * gwp$n2o)
  )

prediction_comparison_linear <- rbind(
  county_predict_linear %>%
    select(county_name, mwh = mwh_predicted, sector) %>%
    mutate(source = "MC model linear"),
  mn_mwh_sector %>%
    select(county_name = county, mwh, sector) %>%
    filter(county_name %in% county_res_predict$county_name) %>%
    mutate(source = "NREL")
)

mwh_add <- rbind(mwh, county_predict_linear %>%
  select(county = county_name, mwh = mwh_predicted, sector) %>%
  mutate(source = "MC model linear"))

mwh_predict_total <- ggplot(mwh_add, aes(x = source, y = mwh, fill = sector)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.7) +
  facet_wrap(~county, scales = "free_y") + # Side-by-side sources
  scale_fill_manual(
    values = c("Residential" = "cornflowerblue", "Commercial/Industrial" = "cornflowerblue"),
    guide = "none"
  ) + # Customize colors
  labs(
    title = "Electricity Usage by County - Total",
    x = "County",
    y = "MWh",
    fill = "Sector"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )

mwh_predict_total

mwh_predict_res <- ggplot(
  mwh_add %>% filter(sector == "Residential"),
  aes(x = source, y = mwh)
) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.7, fill = "firebrick4", col = "black") +
  facet_wrap(~county, scales = "free_y") + # Side-by-side sources
  labs(
    title = "Electricity Usage by County - Residential",
    x = "County",
    y = "MWh",
    fill = "Sector"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )

mwh_predict_res

mwh_predict_nonres <- ggplot(
  mwh_add %>% filter(sector != "Residential"),
  aes(x = source, y = mwh)
) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.7, fill = "#FFDB58", col = "black") +
  facet_wrap(~county, scales = "free_y") + # Side-by-side sources
  labs(
    title = "Electricity Usage by County - Non-Residential",
    x = "County",
    y = "MWh",
    fill = "Sector"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )

mwh_predict_nonres
