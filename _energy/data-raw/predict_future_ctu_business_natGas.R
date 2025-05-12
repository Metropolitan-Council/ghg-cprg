### Develop model for predicting future CTU residential nat_gas usage ###
# This script should be rerun after all updates to predicted_coctu_residential_mcf.rds
# from script _energy/data-raw/predict_current_ctu_residential_nat_gas.R

source("R/_load_pkgs.R")
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


coctu_busi <- read_rds("_energy/data-raw/predicted_coctu_business_mcf.rds")

#### Business predictors ####

mn_parcel <- readRDS("_meta/data/ctu_parcel_data_2021.RDS") %>%
  mutate(ctu_id = stringr::str_pad(ctu_id, width = 8, pad = "0", side = "left"))
urbansim <- readRDS("_meta/data/urbansim_data.RDS")

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
  "jobs_sector_1",
  "jobs_sector_2",
  "jobs_sector_3"
)

### create 2010-2050 urbansim residential dataset
urbansim_busi <- urbansim %>%
  filter(variable %in% business) %>%
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
    county_id = stringr::str_sub(coctu_id_gnis, start = 1, end = 3),
  ) %>%
  left_join(
    cprg_ctu %>% st_drop_geometry() %>%
      distinct(ctu_name, ctu_class, thrive_designation, gnis),
    by = c("ctu_id" = "gnis")
  ) %>%
  left_join(
    cprg_county %>% st_drop_geometry() %>%
      mutate(geoid = str_sub(geoid, -3, -1)) %>%
      select(county_name, geoid),
    by = c("county_id" = "geoid")
  )


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
nat_gas_busi <- left_join(coctu_busi,
  urbansim_busi,
  by = c("coctu_id_gnis", "ctu_name", "ctu_class", "county_name", "inventory_year")
) %>%
  left_join(mn_parcel_busi %>% select(-ctu_name),
    by = c("ctu_id" = "ctu_id")
  )


#### basic linear model and anchor ####
### basic approach to determine average coefficients of several job types,
### anchor cities to their latest mcf annual delivery,
### and back/forecast based on coefficients and estimated changes in jobs

# fit basic model to known cities

nat_gas_busi <- nat_gas_busi %>%
  mutate(
    job_spaces = js_type_1011 +
      js_type_13 +
      js_type_14 +
      js_type_12,
    comm_jobs = jobs_sector_4 +
      jobs_sector_5 +
      jobs_sector_6 +
      jobs_sector_7 +
      jobs_sector_8 +
      jobs_sector_9 +
      jobs_sector_10,
    ind_jobs = jobs_sector_1 +
      jobs_sector_2 +
      jobs_sector_3
  ) %>%
  # don't predict zeros (coctu issues)
  filter(business_mcf > 0)

unit_model <- lm(
  business_mcf ~
    comm_jobs +
    ind_jobs
    - 1,
  data = nat_gas_busi
)

summary(unit_model)

# observed some temporal downward trends (efficiency?), add year in
unit_model_time <- lm(
  business_mcf ~
    comm_jobs +
    ind_jobs +
    as.numeric(inventory_year),
  data = nat_gas_busi
)
summary(unit_model_time)


unit_model_latest <- lm(
  business_mcf ~ 0 +
    comm_jobs +
    ind_jobs,
  data = nat_gas_busi %>%
    group_by(coctu_id_gnis) %>%
    mutate(latest_year = max(inventory_year)) %>%
    filter(inventory_year == latest_year) %>%
    ungroup()
)
summary(unit_model_latest)


# add year as random effect
# library(nlme)
# unit_model_lme <- lme(
#   business_mcf ~
#     job_spaces +
#     comm_jobs +
#     ind_jobs,
#   random = ~1|inventory_year,
#   data = nat_gas_busi,
#   na.action = na.omit
# )
# summary(unit_model_lme)

# extract coefficients from model of interest
unit_coefs <- data.frame(
  term = names(unit_model_latest$coefficients),
  estimate = unit_model_latest$coefficients
) %>%
  select(term, estimate) %>%
  filter(term != "(Intercept)")

# use latest year of data for each city (that has data) as 'intercept'

latest_data <- nat_gas_busi %>%
  group_by(coctu_id_gnis) %>%
  filter(inventory_year == max(inventory_year, na.rm = TRUE))

base_mcf <- latest_data %>%
  select(coctu_id_gnis, inventory_year, base_year_mcf = business_mcf)

# get base values per coctu_id
base_urbansim <- latest_data %>%
  select(coctu_id_gnis, base_year = inventory_year, base_mcf = business_mcf) %>%
  inner_join(
    urbansim_busi %>%
      mutate(
        job_spaces = js_type_1011 +
          js_type_13 +
          js_type_14 +
          js_type_12,
        comm_jobs = jobs_sector_4 +
          jobs_sector_5 +
          jobs_sector_6 +
          jobs_sector_7 +
          jobs_sector_8 +
          jobs_sector_9 +
          jobs_sector_10,
        ind_jobs = jobs_sector_1 +
          jobs_sector_2 +
          jobs_sector_3
      ),
    by = "coctu_id_gnis"
  ) %>%
  filter(inventory_year == base_year)

# calculate urbansim deltas from base year to each other year
delta_units <- urbansim_busi %>%
  mutate(
    job_spaces = js_type_1011 +
      js_type_13 +
      js_type_14 +
      js_type_12,
    comm_jobs = jobs_sector_4 +
      jobs_sector_5 +
      jobs_sector_6 +
      jobs_sector_7 +
      jobs_sector_8 +
      jobs_sector_9 +
      jobs_sector_10,
    ind_jobs = jobs_sector_1 +
      jobs_sector_2 +
      jobs_sector_3
  ) %>%
  select(
    ctu_name, ctu_class, ctu_id, county_name, coctu_id_gnis, inventory_year,
    job_spaces, comm_jobs, ind_jobs
  ) %>%
  pivot_longer(cols = -c(ctu_name, ctu_id, ctu_class, county_name, coctu_id_gnis, inventory_year), names_to = "unit_type", values_to = "unit_count") %>%
  inner_join(
    base_urbansim %>%
      pivot_longer(
        cols = job_spaces:ind_jobs,
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

ctu_busi_delta <- left_join(
  delta_mcf,
  coctu_busi %>%
    group_by(ctu_name, ctu_class, coctu_id_gnis, inventory_year) %>%
    summarize(business_mcf = sum(business_mcf))
)

# plot random grab of some cities
sample_ctus <- ctu_busi_delta %>%
  filter(!is.na(projected_mcf)) %>%
  distinct(ctu_name, ctu_class, coctu_id_gnis) %>%
  slice_sample(n = 20)
#
# mpls_ctu_id <- ctu_busi_delta %>%
#   filter(ctu_name == "Minneapolis") %>%
#   pull(coctu_id)
#
# sample_ctus <- unique(c(sample_ctus, mpls_ctu_id[[1]]))

plot_data_delta <- left_join(sample_ctus, ctu_busi_delta)

ggplot(plot_data_delta, aes(x = inventory_year)) +
  geom_line(aes(y = projected_mcf, color = ctu_name, group = ctu_class), linewidth = 0.8) +
  geom_point(
    data = filter(plot_data_delta, !is.na(business_mcf)),
    aes(y = business_mcf, color = ctu_name),
    shape = 1, size = 2, stroke = 1
  ) +
  facet_wrap(~ctu_name, scales = "free_y") +
  labs(
    x = "Year",
    y = "Business mcf",
    title = "Predicted and Observed Business nat_gas Use",
    subtitle = "Job growth coefficient predictions (lines) and observed values (circles)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# output data from most recent data year onward (utility or RF model where necessary)

coctu_busi <- mutate(coctu_busi, data_source = if_else(
  data_source == "Model prediction",
  "Model prediction - inventory",
  data_source
))

# grab max inventory year per coctu_id
max_years <- coctu_busi %>%
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
    business_mcf = projected_mcf
  ) %>%
  mutate(data_source = "Model prediction - forecast")

ctu_busi <- bind_rows(
  coctu_busi,
  delta_new
) %>%
  group_by(ctu_name, ctu_class, inventory_year, data_source) %>%
  summarize(business_mcf = sum(business_mcf))

## county

county_busi <- bind_rows(
  coctu_busi,
  delta_new
) %>%
  group_by(county_name, inventory_year) %>%
  summarize(business_mcf = sum(business_mcf)) %>%
  # data before 2019 is variable in how many ctus contribute, so keeping 2020 onward until
  # full inventory available
  filter(inventory_year >= 2020)

ggplot(
  county_busi,
  aes(x = inventory_year, y = business_mcf)
) +
  geom_line() +
  facet_wrap(. ~ county_name, scales = "free")

# save intermediate rds
saveRDS(ctu_busi, "_energy/data-raw/forecast_ctu_business_mcf.rds")
saveRDS(county_busi, "_energy/data-raw/forecast_county_business_mcf.rds")
