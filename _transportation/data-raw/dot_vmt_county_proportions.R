# find the proportion of state VMT each county makes up
# specific to each state
# source("_transportation/data-raw/wisdot_vmt_county.R")
source("R/_load_pkgs.R")

wisconsin_vmt <- read_rds("_transportation/data-raw/wisdot/wisdot_vmt_county.RDS")
source("_transportation/data-raw/mndot_vmt_county.R")
source("_meta/data-raw/cprg_county_proportions.R")
cprg_county_meta <- readRDS("_meta/data/cprg_county_meta.RDS")
dot_vmt <- readRDS("_transportation/data/dot_vmt.RDS")
dot_vmt_meta <- readRDS("_transportation/data/dot_vmt_meta.RDS")

state_vmt <-
  # find WI state annual VMT
  wisconsin_vmt %>%
  mutate(vmt_year = year) %>%
  group_by(vmt_year) %>%
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
      mutate(vmt_year = year) %>%
      group_by(vmt_year) %>%
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
    "vmt_year",
    "data_source"
  )) %>%
  mutate(county_proportion_annual_vmt = (annual_vmt / state_annual_vmt) %>%
    round(digits = 6)) %>%
  left_join(
    cprg_county %>%
      sf::st_drop_geometry()
  ) %>%
  select(
    vmt_year, geoid,
    county_name, cprg_area, state, daily_vmt, annual_vmt, state_daily_vmt,
    state_annual_vmt, county_proportion_annual_vmt, data_source
  )



dot_vmt_county_proportions_meta <-
  bind_rows(
    cprg_county_meta,
    dot_vmt_meta,
    tibble::tribble(
      ~"Column", ~"Class", ~"Description",
      "state_daily_vmt", class(dot_vmt_county_proportions$state_daily_vmt), "Statewide annual vehicle miles traveled",
      "state_annual_vmt", class(dot_vmt_county_proportions$state_annual_vmt), "Statewide vehicle miles traveled on an average day",
      "county_proportion_annual_vmt", class(dot_vmt_county_proportions$county_proportion_annual_vmt), "County annual vehicle miles traveled relative to statewide total",
    )
  ) %>%
  filter(`Column` %in% names(dot_vmt_county_proportions)) %>%
  unique()


saveRDS(dot_vmt_county_proportions, "_transportation/data/dot_vmt_county_proportions.RDS")
saveRDS(dot_vmt_county_proportions_meta, "_transportation/data/dot_vmt_county_proportions_meta.RDS")


# plots -----
dot_vmt_county_proportions %>%
  plot_ly(
    type = "scatter",
    mode = "lines+markers",
    x = ~vmt_year,
    y = ~county_proportion_annual_vmt,
    color = ~county_name
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
    y = ~county_name,
    color = ~county_name
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


cprg_county_proportions %>%
  plot_ly(
    type = "box",
    x = ~county_proportion_of_state_pop,
    y = ~county_name,
    color = ~county_name
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
