# find average vehicle age in the region from the most recent Travel Behavior Inventory
source("R/_load_pkgs.R")
library(srvyr, warn.conflicts = FALSE)

cprg_county <- readRDS("_meta/data/cprg_county.RDS")

cprg_tbi_hh_counties <- c("Anoka MN", "Carver MN",
                          "Chisago MN", "Dakota MN", "Hennepin MN",
                          "PIERCE WI", "Ramsey MN",
                          "Scott MN", "Sherburne MN",  "ST. CROIX WI",
                          "Washington MN")

load(url(paste0(
  "https://github.com/Metropolitan-Council/metc.tbi.helper/raw/main/data/",
  "tbi21.rda"
)))

hh21 <- tbi21$household %>%
  # filter to households in the CPRG counties
  filter(hh_county %in% cprg_tbi_hh_counties)

veh21 <- tbi21$vehicle %>%
  filter(hh_id %in% hh21$hh_id) %>%
  mutate(
    fuel_orig = recode_factor(fuel,
      "Missing: Skip logic" = "Other/Not Provided",
      "Missing: Non-response" = "Other/Not Provided",
      "Other" = "Other/Not Provided"
    ),
    # these entries are from cars that were entered by hand by the survey respondent
    fuel = recode_factor(fuel,
      "Missing: Skip logic" = "Other/Not Provided",
      "Missing: Non-response" = "Other/Not Provided",
      "Other" = "Other/Not Provided",
      "Hybrid (HEV)" = "Gas + all other fuels",
      "Electric (EV)" = "Gas + all other fuels",
      "Flex fuel (FFV)" = "Gas + all other fuels",
      "Plug-in hybrid (PHEV)" = "Gas + all other fuels",
      "Other (e.g., natural gas, bio-diesel)" = "Gas + all other fuels",
      "Gas" = "Gas + all other fuels"
    )
  ) %>%
  filter(
    !fuel == "Other/Not Provided",
    !fuel_orig == "Other/Not Provided",
    !year == 1980
  ) %>% # this is actually NA.
  mutate(veh_year_bin = cut(year, breaks = c(1980, 1990, 2000, 2010, 2020, 2030)))

# vehicle age binned by original fuel type -----
tbi_veh_age_orig_fuel_binned <- veh21 %>%
  select(hh_id, veh_id, fuel_orig, year, veh_year_bin, hh_weight) %>%
  droplevels() %>%
  # get weights from households
  # as survey
  as_survey(ids = hh_id, weights = hh_weight) %>%
  group_by(fuel_orig, veh_year_bin) %>%
  # get mean
  summarize(
    n = unweighted(n()),
    est_n = survey_total(),
    est_pct = survey_prop(proportion = TRUE)
  ) %>%
  arrange(-est_n)

tbi_veh_age_orig_fuel_binned
saveRDS(tbi_veh_age_orig_fuel_binned, "_transportation/data-raw/tbi/tbi_veh_age_orig_fuel_binned.RDS")


# vehicle age binned by fuel type -----
tbi_veh_age_fuel_binned <- veh21 %>%
  select(hh_id, veh_id, fuel, year, veh_year_bin, hh_weight) %>%
  droplevels() %>%
  # get weights from households
  # as survey
  as_survey(ids = hh_id, weights = hh_weight) %>%
  group_by(fuel, veh_year_bin) %>%
  # get mean
  summarize(
    n = unweighted(n()),
    est_n = survey_total(),
    est_pct = survey_prop(proportion = TRUE)
  ) %>%
  arrange(-est_n)

tbi_veh_age_fuel_binned
saveRDS(tbi_veh_age_fuel_binned, "_transportation/data-raw/tbi/tbi_veh_age_fuel_binned.RDS")

# median vehicle year  -----
tbi_vehicle_age <- veh21 %>%
  select(hh_id, veh_id, fuel, year, hh_weight) %>%
  droplevels() %>%
  # get weights from households
  # as survey
  as_survey(ids = hh_id, weights = hh_weight) %>%
  # get mean
  summarize(
    year_median = survey_median(year, vartype = "se")
  )

tbi_vehicle_age
saveRDS(tbi_vehicle_age, "_transportation/data-raw/tbi/tbi_vehicle_age.RDS")


# median vehicle year by original fuel type -----
tbi_vehicle_orig_fuel_age <- veh21 %>%
  select(hh_id, veh_id, fuel_orig, year, hh_weight) %>%
  droplevels() %>%
  # get weights from households
  # as survey
  as_survey(ids = hh_id, weights = hh_weight) %>%
  group_by(fuel_orig) %>%
  # get mean
  summarize(
    year_median = survey_median(year, vartype = "se"),
    n = unweighted(n()),
    est_n = survey_total(),
    est_pct = survey_prop(proportion = TRUE)
  ) %>%
  arrange(-est_n)

saveRDS(tbi_vehicle_orig_fuel_age, "_transportation/data-raw/tbi/tbi_vehicle_orig_fuel_age.RDS")



# median vehicle year by fuel type -----
tbi_vehicle_fuel_age <- veh21 %>%
  select(hh_id, veh_id, fuel, year, hh_weight) %>%
  droplevels() %>%
  # get weights from households
  # as survey
  as_survey(ids = hh_id, weights = hh_weight) %>%
  group_by(fuel) %>%
  # get mean
  summarize(
    year_median = survey_median(year, vartype = "se"),
    n = unweighted(n()),
    est_n = survey_total(),
    est_pct = survey_prop(proportion = TRUE)
  ) %>%
  arrange(-est_n)

saveRDS(tbi_vehicle_fuel_age, "_transportation/data-raw/tbi/tbi_vehicle_fuel_age.RDS")
saveRDS(veh21, "_transportation/data-raw/tbi/veh21.RDS")

# remove these large datasets so they don't slow down
# your session
remove(veh21)
remove(hh21)
remove(tbi21)
