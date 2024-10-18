source("R/_load_pkgs.R")
library(srvyr, warn.conflicts = FALSE)
# 2023 Travel Behavior Inventory (TBI) to estimate proportion of regional
# passenger/non-freight trips made by given modes on an average day in 2023,
#  trips taken by households in the 7-county metro area.
#  Only includes trips beginning OR ending in 7-county.

cprg_county <- readRDS("_meta/data/cprg_county.RDS")

load(url(paste0(
  "https://github.com/Metropolitan-Council/metc.tbi.helper/raw/main/data/",
  "tbi.rda"
)))

cdp_tbi_hh_counties <- c(
  "Anoka MN",
  "Carver MN",
  "Dakota MN",
  "Hennepin MN",
  "Ramsey MN",
  "Scott MN",
  "Washington MN",
  "Anoka, MN",
  "Carver, MN",
  "Dakota, MN",
  "Hennepin, MN",
  "Ramsey, MN",
  "Scott, MN",
  "Washington, MN",
  "Anoka County, MN",
  "Carver County, MN",
  "Dakota County, MN",
  "Hennepin County, MN",
  "Ramsey County, MN",
  "Scott County, MN",
  "Washington County, MN"
)


hh <- tbi_rmPII$hh %>%
  # filter to households in the needed counties
  filter(home_county %in% cdp_tbi_hh_counties)

tbi <- tbi_rmPII %>%
  purrr::map(
    filter,
    survey_year == "2023",
    hh_id %in% hh$hh_id
  )

# Map CDP modes to TBI modes -----

# Walking
# Cycling
# Micromobility (including e-scooters)
# Buses (including Bus Rapid Transit)
# Rail/Metro/Tram
# Ferries/River boats
# Taxis or shared vehicles (e.g. hire vehicles)
# Private motorized transport
# Informal/paratransit/popular transit systems
# Other

cdp_modes <- c(
  "Airplane/helicopter" = "Other",
  "ATV or snowmobile" = "Other",
  "Golf cart" = "Other",
  "Boat/ferry" = "Other",
  "Moped-share (e.g., Scoot)" = "Other",
  "Other" = "Other",
  "Other boat (e.g., kayak)" = "Other",
  "Scooter-share (e.g., Bird, Lime)" = "Other",
  "Skateboard or rollerblade" = "Other",
  "Vehicle ferry (took vehicle on board)" = "Other",
  "Other scooter or moped" = "Other",
  "Personal scooter or moped (not shared)" = "Other",
  "Bike-share - electric bicycle" = "Cycling",
  "Bike-share - standard bicycle" = "Cycling",
  "Borrowed bicycle (e.g., a friend's)" = "Cycling",
  "Other rented bicycle" = "Cycling",
  "Standard bicycle (my household's)" = "Cycling",
  "Electric bicycle (my household's)" = "Cycling",
  "Medical transportation service" = "Private motorized transport",
  "Household vehicle 1" = "Private motorized transport",
  "Household vehicle 2" = "Private motorized transport",
  "Household vehicle 3" = "Private motorized transport",
  "Household vehicle 4" = "Private motorized transport",
  "Household vehicle 5" = "Private motorized transport",
  "Household vehicle 6" = "Private motorized transport",
  "Household vehicle 7" = "Private motorized transport",
  "Household vehicle 8" = "Private motorized transport",
  "Car rental" = "Taxis or shared vehicles (e.g. hire vehicles)",
  "Carpool match (e.g., Waze Carpool)" = "Taxis or shared vehicles (e.g. hire vehicles)",
  "Carshare service (e.g., Zipcar)" = "Taxis or shared vehicles (e.g. hire vehicles)",
  "Friend's vehicle" = "Private motorized transport",
  "Other car" = "Private motorized transport",
  "Other vehicle in household" = "Private motorized transport",
  "Regular taxi (e.g., Yellow Cab)" = "Taxis or shared vehicles (e.g. hire vehicles)",
  "Uber, Lyft, or other smartphone-app ride service" = "Taxis or shared vehicles (e.g. hire vehicles)",
  "Lyft Line, Uberpool, or other shared-ride" = "Taxis or shared vehicles (e.g. hire vehicles)",
  "Vanpool" = "Taxis or shared vehicles (e.g. hire vehicles)",
  "Peer-to-peer car rental (e.g., Turo)" = "Taxis or shared vehicles (e.g. hire vehicles)",
  "Electric vehicle carshare (e.g., Evie)" = "Taxis or shared vehicles (e.g. hire vehicles)",
  "Work vehicle" = "Private motorized transport",
  "Other hired car service (e.g., black car, limo)" = "Taxis or shared vehicles (e.g. hire vehicles)",
  "Other motorcycle" = "Private motorized transport",
  "Bus rapid transit (e.g., A Line, C Line, Red Line)" = "Buses",
  "Employer-provided shuttle/bus" = "Buses",
  "Express/commuter bus" = "Buses",
  "Intercity bus (e.g., BoltBus, Greyhound)" = "Buses",
  "Intercity rail (e.g., Amtrak)" = "Rail/Metro/Tram",
  "Light rail" = "Rail/Metro/Tram",
  "Local fixed-route bus" = "Buses",
  "Metro Mobility" = "Buses",
  "Northstar" = "Rail/Metro/Tram",
  "Other bus" = "Buses",
  "Other private shuttle/bus (e.g., Bellair Charters, Airporter Shuttle)" = "Buses",
  "Other rail" = "Rail/Metro/Tram",
  "Paratransit/Dial-A-Ride" = "Buses",
  "School bus" = "Buses",
  "SouthWest Prime or MVTA Connect" = "Buses",
  "University/college shuttle/bus" = "Buses",
  "Walk (or jog/wheelchair)" = "Walking"
)


cdp_mode_mapping <- data.table(
  mode_type_detailed = names(cdp_modes),
  cdp_mode = cdp_modes
)


# match trips with CDP mode mapping

trip_mode <- tbi$trip %>%
  filter(
    trip_o_county %in% cdp_tbi_hh_counties | trip_d_county %in% cdp_tbi_hh_counties,
    !is.na(trip_weight) & trip_weight > 0,
    # o_zone != "Not in study region",
    # d_zone != "Not in study region",
    mode_type != "Missing",
    mode_type_detailed != "Missing"
  ) %>%
  left_join(cdp_mode_mapping) %>%
  left_join(hh %>%
    select(hh_id, hh_weight, sample_segment) %>%
    unique())


passenger_mode_share <- trip_mode %>%
  as_survey_design(
    ids = trip_id,
    weights = trip_weight,
    strata = sample_segment
  ) %>%
  group_by(cdp_mode) %>%
  summarize(
    mode_prop = survey_prop(),
    n_trips = survey_total(),
    sample_trips = n(),
    modes_included = paste0(unique(mode_type_detailed), collapse = ", ")
  ) %>%
  mutate(across(where(is.numeric), \(x)round(x, digits = 4))) %>%
  ungroup()

passenger_mode_share %>%
  select(-modes_included) %>%
  kable(format = "markdown")
