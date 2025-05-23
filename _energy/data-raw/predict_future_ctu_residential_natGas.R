### Develop model for predicting future CTU residential natural gas usage ###
# This script should be rerun after all updates to predicted_coctu_residential_mcf.rds
# from script _energy/data-raw/predict_current_ctu_residential_natGas.R

source("R/_load_pkgs.R")
library(nlme)
source("_energy/data-raw/_energy_emissions_factors.R")

## load in supporting data
cprg_ctu <- read_rds("_meta/data/cprg_ctu.RDS") %>%
  filter(
    !county_name %in% c("Chisago", "Sherburne", "St. Croix", "Pierce"),
    !thrive_designation == "Non-Council Area"
  ) %>%
  mutate(thrive_designation = as.factor(if_else(
    thrive_designation == "Rural Center",
    "Rural Residential", # renaming rural center as not enough cities have utility data for modeling
    thrive_designation
  )))
cprg_county <- read_rds("_meta/data/cprg_county.RDS") %>%
  filter(!county_name %in% c("Chisago", "Sherburne", "St. Croix", "Pierce"))


coctu_res <- read_rds("_energy/data-raw/predicted_coctu_residential_mcf.rds")

# predictor data
mn_parcel <- readRDS("_meta/data/ctu_parcel_data_2021.RDS") %>%
  mutate(ctu_id = stringr::str_pad(ctu_id, width = 8, pad = "0", side = "left"))
urbansim <- readRDS("_meta/data/urbansim_data.RDS")

noaa <- readRDS("_meta/data/noaa_weather_monthly.rds")

noaa_year <- noaa %>%
  group_by(inventory_year) %>%
  summarize(
    heating_degree_days = sum(heating_degree_days),
    cooling_degree_days = sum(cooling_degree_days),
    temperature = mean(dry_bulb_temp)
  )

# residential predictors
mn_parcel_res <- mn_parcel %>%
  filter(mc_classification %in% c(
    "single_family_home",
    "multifamily_home",
    "apartment"
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


# urbansim

residential <- c(
  "total_pop",
  "total_households",
  "total_residential_units",
  "manufactured_homes",
  "single_fam_det_sl_own",
  "single_fam_det_ll_own",
  "single_fam_det_rent",
  "single_fam_attached_own",
  "single_fam_attached_rent",
  "multi_fam_own",
  "multi_fam_rent"
)

### create 2005-2050 urbansim residential dataset
urbansim_res <- urbansim %>%
  filter(variable %in% residential) %>%
  group_by(coctu_id_gnis, ctu_id, variable) %>%
  complete(inventory_year = full_seq(c(2005, 2025), 1)) %>% # add interstitial years and expand to 2025
  arrange(coctu_id_gnis, ctu_id, variable, inventory_year) %>%
  mutate(value = approx(inventory_year, value, inventory_year, method = "linear", rule = 2)$y) %>% # allow extrapolation
  ungroup() %>%
  pivot_wider(
    id_cols = c(coctu_id_gnis, ctu_id, inventory_year),
    names_from = variable,
    values_from = value
  ) %>%
  filter(!is.na(coctu_id_gnis)) %>%
  mutate(
    county_id = stringr::str_sub(coctu_id_gnis, start = 1, end = 3)
  ) %>%
  left_join(
    cprg_ctu %>% st_drop_geometry() %>%
      distinct(ctu_name, ctu_class, gnis, thrive_designation),
    by = c("ctu_id" = "gnis")
  ) %>%
  left_join(
    cprg_county %>% st_drop_geometry() %>%
      mutate(geoid = str_sub(geoid, -3, -1)) %>%
      select(county_name, geoid),
    by = c("county_id" = "geoid")
  ) %>%
  left_join(
    noaa_year %>%
      select(inventory_year, heating_degree_days),
    by = "inventory_year"
  )


# merge into utility data
natgas_res <- left_join(coctu_res,
  urbansim_res,
  by = c("coctu_id_gnis", "ctu_name", "ctu_class", "county_name", "inventory_year")
) %>%
  left_join(mn_parcel_res %>% select(-ctu_name),
    by = c("ctu_id" = "ctu_id")
  )


#### basic linear model and anchor ####
### basic approach to determine average coefficients of several housing types,
### anchor cities to their latest mcf annual delivery,
### and back/forecast based on coefficients and estimated changes in housing stock

# fit basic model to known cities

natgas_res <- natgas_res %>%
  mutate(
    mfh = multi_fam_own + multi_fam_rent,
    sfh_ll = single_fam_det_ll_own,
    sfh_sl = single_fam_det_rent + single_fam_det_sl_own + manufactured_homes,
    sf_att = single_fam_attached_own + single_fam_attached_rent
  ) %>%
  # don't predict zeros (coctu issues)
  filter(residential_mcf > 0)

unit_model <- lm(
  residential_mcf ~
    mfh +
    sfh_ll +
    sfh_sl +
    sf_att,
  data = natgas_res
)
summary(unit_model)

plot(unit_model, which = 4)


summary(model_share)
# add ctu_name as random effect
unit_model_lme <- lme(
  residential_mcf ~ mfh +
    sfh_ll +
    sfh_sl +
    sf_att,
  random = ~ 1 | thrive_designation,
  data = natgas_res
)
summary(unit_model_lme)

# extract coefficients
unit_coefs <- data.frame(
  term = names(unit_model$coefficients),
  estimate = unit_model$coefficients
) %>%
  select(term, estimate) %>%
  filter(term != "(Intercept)")

# calculate urbansim deltas from base year to each other year
delta_units <- urbansim_res %>%
  mutate(
    mfh = multi_fam_own + multi_fam_rent,
    sfh_ll = single_fam_det_ll_own,
    sfh_sl = single_fam_det_rent + single_fam_det_sl_own + manufactured_homes,
    sf_att = single_fam_attached_own + single_fam_attached_rent
  ) %>%
  select(
    ctu_name, ctu_class, ctu_id, county_name, coctu_id_gnis, inventory_year,
    mfh, sfh_ll, sfh_sl, sf_att
  ) %>%
  pivot_longer(cols = -c(ctu_name, ctu_id, ctu_class, county_name, coctu_id_gnis, inventory_year), names_to = "unit_type", values_to = "unit_count") %>%
  inner_join(
    base_urbansim %>%
      pivot_longer(
        cols = mfh:sf_att,
        names_to = "unit_type", values_to = "base_unit_count"
      ) %>%
      select(coctu_id_gnis, unit_type, base_unit_count),
    by = c("coctu_id_gnis", "unit_type")
  ) %>%
  mutate(unit_delta = unit_count - base_unit_count)


# apply coefficients to delta and sum
delta_mcf <- delta_units %>%
  left_join(unit_coefs, by = c("unit_type" = "term")) %>%
  mutate(delta_mcf = unit_delta * estimate) %>%
  group_by(ctu_name, ctu_class, county_name, coctu_id_gnis, inventory_year) %>%
  summarise(delta_total_mcf = sum(delta_mcf, na.rm = TRUE), .groups = "drop") %>%
  # add delta to base year mcf
  left_join(base_urbansim %>% select(coctu_id_gnis, ctu_id, base_mcf), by = "coctu_id_gnis") %>%
  mutate(projected_mcf = base_mcf + delta_total_mcf)

## graph as check

ctu_res_delta <- left_join(
  delta_mcf,
  coctu_res %>%
    group_by(ctu_name, ctu_class, coctu_id_gnis, inventory_year) %>%
    summarize(residential_mcf = sum(residential_mcf))
)

# plot random grab of some cities
sample_ctus <- ctu_res_delta %>%
  filter(!is.na(projected_mcf)) %>%
  distinct(ctu_name, ctu_class, coctu_id_gnis) %>%
  slice_sample(n = 50)

mpls_ctu_id <- ctu_res_delta %>%
  filter(ctu_name == "Minneapolis") %>%
  distinct(ctu_name, ctu_class, coctu_id_gnis)

sample_ctus <- bind_rows(sample_ctus, mpls_ctu_id) %>%
  distinct(ctu_name, ctu_class, coctu_id_gnis)

plot_data_delta <- left_join(sample_ctus, ctu_res_delta)

ggplot(plot_data_delta, aes(x = inventory_year)) +
  geom_line(aes(y = projected_mcf, color = ctu_name, group = ctu_class), linewidth = 0.8) +
  geom_point(
    data = filter(plot_data_delta, !is.na(residential_mcf)),
    aes(y = residential_mcf, color = ctu_name, group = ctu_class),
    shape = 1, size = 2, stroke = 1
  ) +
  facet_wrap(~ctu_name, scales = "free_y") +
  labs(
    x = "Year",
    y = "Residential mcf",
    title = "Predicted and Observed Residential Natural Gas Use",
    subtitle = "Housing coefficient predictions (lines) and observed values (circles)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# output data from most recent data year onward (utility or RF model where necessary)

coctu_res <- mutate(coctu_res, data_source = if_else(
  data_source == "Model prediction",
  "Model prediction - inventory",
  data_source
))

# grab max inventory year per coctu_id_gnis
max_years <- coctu_res %>%
  group_by(coctu_id_gnis) %>%
  summarize(max_year = max(inventory_year), .groups = "drop")

# start delta_mcf at max_year +1
delta_new <- delta_mcf %>%
  inner_join(max_years, by = "coctu_id_gnis") %>%
  filter(inventory_year > max_year) %>%
  select(coctu_id_gnis,
    ctu_name,
    ctu_class,
    county_name,
    inventory_year,
    residential_mcf = projected_mcf
  ) %>%
  mutate(data_source = "Model prediction - forecast")

ctu_res <- bind_rows(
  coctu_res,
  delta_new
) %>%
  group_by(ctu_name, ctu_class, inventory_year, data_source) %>%
  summarize(residential_mcf = sum(residential_mcf))

## county

county_res <- bind_rows(
  coctu_res,
  delta_new
) %>%
  group_by(county_name, inventory_year) %>%
  summarize(residential_mcf = sum(residential_mcf)) %>%
  # data before 2019 is variable in how many ctus contribute, so keeping 2020 onward until
  # full inventory available
  filter(inventory_year >= 2020)

ggplot(
  county_res,
  aes(x = inventory_year, y = residential_mcf)
) +
  geom_line() +
  facet_wrap(. ~ county_name, scales = "free")

# save intermediate rds
saveRDS(ctu_res, "_energy/data-raw/forecast_ctu_residential_mcf.rds")
saveRDS(county_res, "_energy/data-raw/forecast_county_residential_mcf.rds")
