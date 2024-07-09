
state_population_timeseries <- readRDS("_meta/data/state_population_timeseries.RDS")
cprg_population_timeseries <- readRDS("_meta/data/cprg_population_timeseries.RDS")

cprg_county_proportions_timeseries <- cprg_population_timeseries %>% 
  mutate(county_population = population) %>% 
  ungroup() %>% 
  left_join(state_population_timeseries %>% 
              select(state, population_year, state_population = population) %>% 
              ungroup(),
            by = c("population_year",
                   "STATE" = "state")) %>% 
  rowwise() %>% 
  mutate(county_proportion_population = county_population/state_population %>% 
           round(digits = 6)) %>% 
  arrange(population_year, NAME)


# plots checking trends -----
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

# finish and save -----

cprg_county_proportions_timeseries_meta <- bind_rows(
  cprg_county_meta,
  tribble(
    ~Column, ~Class, ~Description,
    "year", class(cprg_county_proportions_timeseries$year), "Estimate year",
    "county_population", class(cprg_county_proportions_timeseries$county_population), "Total county population estimate",
    "state_population", class(cprg_county_proportions_timeseries$state_population), "Total state population estimate",
    "county_proportion_of_state_pop", class(cprg_county_proportions_timeseries$county_proportion_of_state_pop), "Proportion of the county population relative to the total state population",
    "population_data_source", class(cprg_county_proportions_timeseries$population_data_source), "Population estimate data source"
  )
) %>%
  filter(Column %in% names(cprg_county_proportions_timeseries))



