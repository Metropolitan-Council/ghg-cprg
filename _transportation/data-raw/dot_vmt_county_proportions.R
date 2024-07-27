# find the proportion of state VMT each county makes up
# specific to each state
source("_transportation/data-raw/wisdot_vmt_county.R")
source("_transportation/data-raw/mndot_vmt_county.R")
source("_meta/data-raw/cprg_county_proportions.R")
cprg_county_meta <- readRDS("_meta/data/cprg_county_meta.RDS")
dot_vmt <- readRDS("_transportation/data/dot_vmt.RDS")

state_vmt <-
  # find WI state annual VMT
  wisconsin_vmt %>%
  group_by(year) %>%
  summarize(
    state_daily_vmt = sum(daily_vmt),
    state_annual_vmt = sum(annual_vmt)
  ) %>%
  mutate(
    state = "Wisconsin",
    data_source = "WisDOT"
  ) %>%
  bind_rows(
    # bind with MN annual VMT
    vmt_county_raw_interp %>%
      group_by(year) %>%
      summarise(
        state_daily_vmt = sum(daily_vmt),
        state_annual_vmt = sum(annual_vmt)
      ) %>%
      mutate(
        state = "Minnesota",
        data_source = "MnDOT"
      )
  )

dot_vmt_county_proportions <- dot_vmt %>%
  left_join(state_vmt, by = c(
    "year",
    "data_source"
  )) %>%
  mutate(county_proportion_annual_vmt = (annual_vmt / state_annual_vmt) %>%
    round(digits = 6)) %>%
  left_join(
    cprg_county %>%
      select(
        state_name = STATE,
        county = NAME,
        GEOID,
        county_fips = COUNTYFP
      ) %>%
      sf::st_drop_geometry(),
    by = join_by(county)
  ) %>%
  select(
    year, GEOID,
    county, cprg_area, state, daily_vmt, annual_vmt, state_daily_vmt,
    state_annual_vmt, county_proportion_annual_vmt, data_source
  )



dot_vmt_county_proportions_meta <-
  bind_rows(
    cprg_county_meta,
    tibble::tribble(
      ~"Column", ~"Class", ~"Description",
      "year", class(dot_vmt_county_proportions$year), "VMT estimation year",
      "county", class(dot_vmt_county_proportions$county), "County name",
      "cprg_area", class(dot_vmt_county_proportions$cprg_area), "Whether county is included in the CPRG area",
      "state", class(dot_dot_vmt_county_proportions$state), "County state",
      "daily_vmt", class(dot_vmt_county_proportions$daily_vmt), "County vehicle miles traveled on an average day",
      "annual_vmt", class(dot_vmt_county_proportions$annual_vmt), "County annual vehicle miles traveled",
      "state_daily_vmt", class(dot_vmt_county_proportions$state_daily_vmt), "Statewide annual vehicle miles traveled",
      "state_annual_vmt", class(dot_vmt_county_proportions$state_annual_vmt), "Statewide vehicle miles traveled on an average day",
      "county_proportion_annual_vmt", class(dot_vmt_county_proportions$county_proportion_annual_vmt), "County annual vehicle miles traveled relative to statewide total",
      "data_source", class(dot_vmt_county_proportions$data_source), "State DOT. Either \"MnDOT\" or \"WisDOT\""
    )
  ) %>%
  filter(`Column` %in% names(dot_vmt_county_proportions))


saveRDS(dot_vmt_county_proportions, "_transportation/data/dot_vmt_county_proportions.RDS")
saveRDS(dot_vmt_county_proportions_meta, "_transportation/data/dot_vmt_county_proportions_meta.RDS")


# plots -----
dot_vmt_county_proportions %>%
  plot_ly(
    type = "scatter",
    mode = "lines+markers",
    x = ~year,
    y = ~county_proportion_annual_vmt,
    color = ~county
  ) %>%
  layout(yaxis = list(
    tickformat = "1%"
  ))

# check the overall spread
# the IQR for every county, except Hennepin and Ramsey,
# is very narrow
# suggesting consistent proportions over time!
dot_vmt_county_proportions %>%
  plot_ly(
    type = "box",
    x = ~county_proportion_annual_vmt,
    y = ~county,
    color = ~county
  ) %>%
  plotly_layout(
    main_title = "Variation in county proportion of state VMT",
    x_title = "County proportion of state VMT",
    y_title = "County",
    subtitle = "2001 to 2022"
  ) %>%
  layout(xaxis = list(
    tickformat = "1%"
  ))


county_proportions %>%
  plot_ly(
    type = "box",
    x = ~county_proportion_of_state_pop,
    y = ~name,
    color = ~name
  ) %>%
  plotly_layout(
    main_title = "Variation in county proportion of state population",
    x_title = "County proportion of state population",
    y_title = "County",
    subtitle = "2000 to 2022"
  ) %>%
  layout(xaxis = list(
    tickformat = "1%"
  ))
