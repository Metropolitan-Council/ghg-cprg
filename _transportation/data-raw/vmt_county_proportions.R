# find the proportion of state VMT each county makes up
# specific to each state
source("_transportation/data-raw/wisdot_vmt_county.R")
source("_transportation/data-raw/mndot_vmt_county.R")
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

vmt_county_proportions <- dot_vmt %>%
  left_join(state_vmt, by = c(
    "year",
    "data_source"
  )) %>%
  mutate(county_proportion_vmt = (annual_vmt / state_annual_vmt) %>%
    round(digits = 6))

vmt_county_proportions %>%
  plot_ly(
    type = "scatter",
    mode = "lines+markers",
    x = ~year,
    y = ~county_proportion_vmt,
    color = ~county
  ) %>%
  layout(yaxis = list(
    tickformat = "1%"
  ))

# check the overall spread
# the IQR for every county, except Hennepin and Ramsey,
# is very narrow
# suggesting consistent proportions over time!
vmt_county_proportions %>%
  plot_ly(
    type = "box",
    x = ~county_proportion_vmt,
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
