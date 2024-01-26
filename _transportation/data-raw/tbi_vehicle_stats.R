# find average vehicle age in the region from the most recent Travel Behavior Inventory
source("R/_load_pkgs.R")
cprg_county <- readRDS("R/data/cprg_county.RDS")

library(srvyr)

load(url(paste0(
  "https://github.com/Metropolitan-Council/metc.tbi.helper/raw/main/data/",
  "tbi21.rda"
)))

hh21 <- tbi21$household %>% 
  # filter to households in the CPRG counties
  filter(home_county %in% cprg_county$NAME)

veh21 <- tbi21$vehicle %>% 
  filter(hh_id %in% hh21$hh_id) %>% 
  mutate(
    # these entries are from cars that were entered by hand by the survey respondent
    fuel = recode_factor(fuel,
                         "Missing: Skip logic" = "Other/Not Provided",
                         "Missing: Non-response" = "Other/Not Provided",
                         "Other" = "Other/Not Provided",
                         "Hybrid (HEV)" = "Hybrid, Flex, or Electric",
                         "Electric (EV)" = "Hybrid, Flex, or Electric",
                         "Flex fuel (FFV)" = "Hybrid, Flex, or Electric",
                         "Plug-in hybrid (PHEV)" = "Hybrid, Flex, or Electric")
  ) %>%
  filter(!fuel == "Other/Not Provided",
         !year == 1980) %>% # this is actually NA.
  mutate(veh_year_bin = cut(year, breaks = c(1980, 1990, 2000, 2010, 2020, 2030)))



# vehicle age binned by fuel type -----
veh21 %>%
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
    est_pct = survey_prop()
  ) %>% 
  arrange(-est_n)



# median vehicle year by fuel type -----
veh21 %>%
  select(hh_id, veh_id, fuel, year, hh_weight) %>%
  droplevels() %>%
  # get weights from households
  # as survey
  as_survey(ids = hh_id, weights = hh_weight) %>%
  group_by(fuel) %>%
  # get mean 
  summarize(
    year_median = survey_median(year, vartype = "se"),
    year_mean = survey_mean(year, vartype = "se"),
    n = unweighted(n()),
    est_n = survey_total(),
    est_pct = survey_prop()
  )    

# gasoline/diesel/other distribution within passenger vehicles -----
veh21 %>%
  as_survey_design(ids = veh_id, weights = hh_weight) %>%
  group_by(fuel) %>%
  summarize(
    n = unweighted(n()),
    est_n = survey_total(),
    est_pct = survey_prop()
  ) %>% 
  arrange(-est_pct)
