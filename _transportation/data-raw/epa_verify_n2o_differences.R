# investigate scale of difference in CO2e with and without 
# N2O emissions accounted for
# EQUATES is missing N2O
# how big a deal is it?
# 
# the difference with and without n2o is generally low
# and mostly effects trucks, buses, motor homes, larger vehicles,
# which makes sense, because these emit more n2o than passenger
# cars and smaller vehicles, both because more fuel usage, but
# also because these larger vehicles are more likely to run on diesel
# and diesel emits more n2o than gasoline
source("_transportation/data-raw/epa_onroad_emissions_compile.R")


epa_emismod_n2o <- epa_emismod_summary %>%
  group_by(calc_year, geoid, county_name, scc6_desc, scc6) %>%
  summarize(
    emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e),
    emissions_metric_tons_co2e_n2o = sum(emissions_metric_tons_co2e_n2o)
  ) %>%
  mutate(
    pct_diff = round(
      (emissions_metric_tons_co2e_n2o - emissions_metric_tons_co2e) /
        emissions_metric_tons_co2e,
      digits = 3)) %>% 
  arrange(-pct_diff) %>% 
  left_join(scc_combine) %>% 
  filter(calc_year %in% c("2021", "2022"))

epa_nei_n2o <- epa_nei_onroad_summary %>% 
  group_by(calc_year, geoid, county_name, scc6_desc, scc6) %>%
  summarize(
    emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e),
    emissions_metric_tons_co2e_n2o = sum(emissions_metric_tons_co2e_n2o)
  ) %>%
  mutate(
    pct_diff = round(
      (emissions_metric_tons_co2e_n2o - emissions_metric_tons_co2e) /
        emissions_metric_tons_co2e,
      digits = 3)) %>% 
  arrange(-pct_diff) %>% 
  left_join(scc_combine) %>% 
  filter(calc_year %in% c("2020"))

subplot(
  epa_emismod_n2o %>% 
    plot_ly(
      x = ~ pct_diff,
      color = ~vehicle_type,
      type = "box"
    ) %>% 
    plotly_layout(
      subtitle = "Air Emissions Modeling",
      x_title = "% difference with, without N<sub>2</sub>O"
    ) %>% 
    layout(xaxis = list(tickformat = ".1%",
                        range = c(0, 0.1))),
  epa_nei_n2o %>% 
    plot_ly(
      x = ~ pct_diff,
      color = ~vehicle_type,
      type = "box"
    ) %>% 
    plotly_layout(
      subtitle = "National Emissions Inventory",
      x_title = "% difference with, without N<sub>2</sub>O"
    ) %>% 
    layout(xaxis = list(tickformat = ".1%",
                        range = c(0, 0.1))),
  shareY = TRUE
)


subplot(
  epa_emismod_n2o %>% 
    plot_ly(
      x = ~ pct_diff,
      color = ~fuel_type,
      type = "box"
    ) %>% 
    plotly_layout(
      subtitle = "Air Emissions Modeling",
      x_title = "% difference with, without N<sub>2</sub>O"
    ) %>% 
    layout(xaxis = list(tickformat = ".1%",
                        range = c(0, 0.1))),
  epa_nei_n2o %>% 
    plot_ly(
      x = ~ pct_diff,
      color = ~fuel_type,
      type = "box"
    ) %>% 
    plotly_layout(
      subtitle = "National Emissions Inventory",
      x_title = "% difference with, without N<sub>2</sub>O"
    ) %>% 
    layout(xaxis = list(tickformat = ".1%",
                        range = c(0, 0.1))),
  shareY = TRUE
)
