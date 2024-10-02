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
#
# First, re-run the compilation script to get all the relevant data pieces
source("_transportation/data-raw/epa_onroad_emissions_compile.R")

# for each data source, summarize by vehicle type, fuel type, county, year
epa_emismod_n2o <- epa_emismod_summary %>%
  group_by(calc_year, geoid, county_name, vehicle_type, fuel_type) %>%
  summarize(
    emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e),
    emissions_metric_tons_co2e_exclude_n2o = sum(emissions_metric_tons_co2e_exclude_n2o)
  ) %>%
  mutate(
    pct_diff = round(
      (emissions_metric_tons_co2e - emissions_metric_tons_co2e_exclude_n2o) /
        emissions_metric_tons_co2e_exclude_n2o,
      digits = 3
    )
  ) %>%
  arrange(-pct_diff) %>%
  left_join(scc_combine) %>%
  mutate(data_source = "Air Emissions Modeling") %>%
  unique()
# filter(calc_year %in% c("2021", "2022"))

epa_nei_n2o <- epa_nei_onroad_summary %>%
  group_by(calc_year, geoid, county_name, vehicle_type, fuel_type) %>%
  summarize(
    emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e),
    emissions_metric_tons_co2e_exclude_n2o = sum(emissions_metric_tons_co2e_exclude_n2o)
  ) %>%
  mutate(
    pct_diff = round(
      (emissions_metric_tons_co2e - emissions_metric_tons_co2e_exclude_n2o) /
        emissions_metric_tons_co2e_exclude_n2o,
      digits = 3
    )
  ) %>%
  arrange(-pct_diff) %>%
  left_join(scc_combine) %>%
  mutate(data_source = "National Emissions Inventory") %>%
  unique()
# filter(calc_year %in% c("2020"))



epa_equates_n2o <- epa_equates_summary %>%
  group_by(calc_year, geoid, county_name, vehicle_type, fuel_type) %>%
  summarize(
    emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e),
    emissions_metric_tons_co2e_exclude_n2o = sum(emissions_metric_tons_co2e_exclude_n2o)
  ) %>%
  mutate(
    pct_diff = round(
      (emissions_metric_tons_co2e - emissions_metric_tons_co2e_exclude_n2o) /
        emissions_metric_tons_co2e_exclude_n2o,
      digits = 3
    ) %>%
      replace_na(0)
  ) %>%
  arrange(-pct_diff) %>%
  left_join(scc_combine) %>%
  mutate(data_source = "EQUATES") %>%
  unique()
# filter(calc_year %in% c("2018", "2019"))


# plot the differences and compare side by side
subplot(
  epa_emismod_n2o %>%
    plot_ly(
      x = ~pct_diff,
      color = ~vehicle_type,
      type = "box",
      legendgroup = ~vehicle_type,
      showlegend = FALSE,
      boxpoints = "all",
      jitter = 0.4,
      hoverinfo = "text",
      hovertext = ~ paste0(
        data_source, " ", calc_year, "<br>",
        vehicle_type, "; ", fuel_type, "<br>",
        county_name, " County", "<br>",
        scales::percent(pct_diff)
      ),
      marker = list(
        size = 9,
        opacity = 0.6,
        line = list(
          width = 1
        )
      )
    ) %>%
    plotly_layout(
      subtitle = "Air Emissions Modeling"
    ) %>%
    layout(xaxis = list(
      tickformat = ".1%",
      range = c(0, 0.035)
    )),
  epa_nei_n2o %>%
    plot_ly(
      x = ~pct_diff,
      color = ~vehicle_type,
      type = "box",
      legendgroup = ~vehicle_type,
      showlegend = FALSE,
      boxpoints = "all",
      jitter = 0.4,
      hoverinfo = "text",
      hovertext = ~ paste0(
        data_source, " ", calc_year, "<br>",
        vehicle_type, "; ", fuel_type, "<br>",
        county_name, " County", "<br>",
        scales::percent(pct_diff)
      ),
      marker = list(
        size = 9,
        opacity = 0.6,
        line = list(
          width = 1
        )
      )
    ) %>%
    plotly_layout(
      subtitle = "National Emissions Inventory"
    ) %>%
    layout(xaxis = list(
      tickformat = ".1%",
      range = c(0, 0.035)
    )),
  epa_equates_n2o %>%
    plot_ly(
      x = ~pct_diff,
      color = ~vehicle_type,
      type = "box",
      legendgroup = ~vehicle_type,
      showlegend = FALSE,
      boxpoints = "all",
      jitter = 0.4,
      hoverinfo = "text",
      hovertext = ~ paste0(
        data_source, " ", calc_year, "<br>",
        vehicle_type, "; ", fuel_type, "<br>",
        county_name, " County", "<br>",
        scales::percent(pct_diff)
      ),
      marker = list(
        size = 9,
        opacity = 0.6,
        line = list(
          width = 1
        )
      )
    ) %>%
    plotly_layout(
      subtitle = "EQUATES"
    ) %>%
    layout(xaxis = list(
      tickformat = ".1%",
      range = c(0, 0.035)
    )),
  shareY = TRUE
) %>%
  plotly_layout(
    # main_title = "N<sub>2</sub>O inclusion results in differences up to 3%",
    x_title = "% difference including N<sub>2</sub>O in CO<sub>2</sub>e"
  ) %>%
  layout(
    legend = list(orientation = "h")
  )
