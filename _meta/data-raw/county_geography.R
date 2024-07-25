source("R/_load_pkgs.R")

# fetch county geographies for all counties in
# Minnesota and Wisconsin
# This will give us GEOID, NAME, STATE, and other identifying columns
county_geography <- bind_rows(
  tigris::counties(state = "MN") %>%
    mutate(
      STATE = "Minnesota",
      STATE_ABB = "MN"
    ),
  tigris::counties(state = "WI") %>%
    mutate(
      STATE = "Wisconsin",
      STATE_ABB = "WI"
    )
) %>%
  sf::st_drop_geometry()
