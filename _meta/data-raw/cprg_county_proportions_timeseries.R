
state_population_timeseries <- readRDS("_meta/data/state_population_timeseries.RDS")
cprg_population_timeseries <- readRDS("_meta/data/cprg_population_timeseries.RDS")

cprg_county_proportions_timeseries <- cprg_population_timeseries %>% 
  ungroup() %>% 
  left_join(state_population_timeseries %>% 
              select(state, population_year, statewide_population = population) %>% 
              ungroup(),
            by = c("population_year",
                   "STATE" = "state")) %>% 
  rowwise() %>% 
  mutate(county_proportion_population = population/statewide_population %>% 
           round(digits = 6)) %>% 
  arrange(population_year, NAME)


cprg_county_proportions_timeseries %>% 
  plot_ly(
    type = "scatter",
    mode = "lines+markers",
    x = ~population_year,
    y = ~county_proportion_population,
    color = ~NAME
  ) %>% 
  layout(yaxis = list(
    tickformat = "1%"
  ))


# same with 
cprg_county_proportions_timeseries %>% 
  plot_ly(
    type = "box",
    x = ~county_proportion_population,
    y  = ~NAME,
    color = ~NAME
  ) %>% 
  plotly_layout(
    main_title = "Variation in county proportion of statewide population",
    x_title = "County proportion of statewide population",
    y_title = "County"
  ) %>% 
  layout(xaxis = list(
    tickformat = "1%"
  ))
