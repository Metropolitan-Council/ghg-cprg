source("R/_load_pkgs.R")
library(srvyr, warn.conflicts = FALSE)

cprg_county <- readRDS("_meta/data/cprg_county.RDS")

cprg_tbi_counties <- cprg_county %>%
  mutate(tbi_county = paste0(NAME, " ", STATE_ABB)) %>%
  magrittr::extract2("tbi_county")

load(url(paste0(
  "https://github.com/Metropolitan-Council/metc.tbi.helper/raw/main/data/",
  "tbi21.rda"
)))

hh21 <- tbi21$household %>%
  # filter to households in the CPRG counties
  filter(home_county %in% cprg_county$NAME)

summary(tbi21$trip$distance)

trip21 <- tbi21$trip %>%
  filter(
    hh_id %in% hh21$hh_id,
    mode_type %in% c(
      "Household Vehicle",
      "Other Vehicle",
      "For-Hire Vehicle"
    ),
    trip_o_county %in% cprg_tbi_counties,
    trip_d_county %in% cprg_tbi_counties,
    !is.na(distance),
    distance < 720,
    distance > 0
  ) %>%
  select(
    hh_id,
    person_id,
    trip_id,
    trip_weight,
    distance,
    trip_o_county,
    trip_d_county
  ) %>%
  droplevels() %>%
  mutate(
    trip_o_county = stringr::str_remove(trip_o_county, " MN"),
    trip_d_county = stringr::str_remove(trip_d_county, " MN")
  ) %>%
  mutate(origin_dest_county_pair = paste0(trip_o_county, "-", trip_d_county))


regional_trip_length_avg <-
  trip21 %>%
  as_survey(
    ids = trip_id,
    weights = trip_weight
  ) %>%
  summarize(
    mean_trip_dist = survey_mean(distance, vartype = "se"),
    estimate_n = survey_total(),
    n = unweighted(n())
  )


# find unique, unordered origin-destination pairs
v1 <- do.call(paste, as.data.frame(t(apply(trip21[, 6:7], 1, sort))))
trip21$od_pair <- match(v1, unique(v1))

od_pair_index <- trip21 %>%
  select(od_pair, trip_o_county, trip_d_county, origin_dest_county_pair) %>%
  unique() %>%
  mutate(pair_type = ifelse(trip_o_county == trip_d_county, "Same county", "Different county"))


tbi_mean_trip_length <- trip21 %>%
  as_survey(
    ids = trip_id,
    weights = trip_weight
  ) %>%
  group_by(od_pair) %>%
  summarize(
    n = unweighted(n()),
    mean_trip_dist = survey_mean(distance, vartype = "se"),
    estimate_n = survey_total()
  ) %>%
  left_join(od_pair_index)



tbi_od_ordered_trip_length <- trip21 %>%
  as_survey(
    ids = trip_id,
    weights = trip_weight
  ) %>%
  group_by(origin_dest_county_pair) %>%
  summarize(
    n = unweighted(n()),
    mean_trip_dist = survey_mean(distance, vartype = "se"),
    estimate_n = survey_total()
  ) %>%
  left_join(od_pair_index %>%
    select(
      trip_o_county, trip_d_county,
      origin_dest_county_pair
    ) %>%
    unique())

saveRDS(tbi_mean_trip_length, "_transportation/data-raw/tbi/tbi_mean_trip_length.RDS")
saveRDS(tbi_od_ordered_trip_length, "_transportation/data-raw/tbi/tbi_od_ordered_trip_length.RDS")
saveRDS(regional_trip_length_avg, "_transportation/data-raw/tbi/tbi_regional_trip_length_avg.RDS")

saveRDS(trip21, "_transportation/data-raw/tbi/trip21.RDS")
# remove these large datasets so they don't slow down
# your session
remove(hh21)
remove(tbi21)
remove(trip21)
