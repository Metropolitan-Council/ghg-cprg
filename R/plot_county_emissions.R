#' Plot county level emissions for any given sector
#'
#' @param county_emissions data frame, compiled emissions. Must have columns
#'   "sector", "geog_name", "emissions_metric_tons_co2e", "source", "category",
#'   and "geog_level"
#' @param sector character, one of "Transportation", ....
#' @param plotly_source character, passed to source paramter in `plotly::plot_ly()`
#'
#' @return Plotly object
#' @export
#'
#' @importFrom plotly plot_ly layout
#' @importFrom dplyr filter
#' @importFrom stringr str_to_title
#' @importFrom councilR plotly_layout
plot_county_emissions <- function(county_emissions,
                                  .sector,
                                  .plotly_source) {
  plot_data <- county_emissions %>%
    dplyr::filter(sector == .sector)

  if (nrow(plot_data) == 0) {
    return(plot_ly(
      source = .plotly_source,
      type = "bar"
    ))
  }

  plot_ly(
    data = plot_data,
    type = "bar",
    source = .plotly_source,
    x = ~geog_name,
    y = ~emissions_metric_tons_co2e,
    color = ~source,
    split = ~source,
    marker = list(
      line = list(
        color = "lightgray",
        width = 1.3
      )
    ),
    hovertemplate = ~ paste0(
      geog_name, " County", "<br>",
      sector, " - ", category, ", ", source, "<br>",
      round(emissions_metric_tons_co2e * 1e-6, digits = 2), " million metric tons CO<sub>2</sub>e", "<br>",
      "<extra></extra>"
    )
  ) %>%
    councilR::plotly_layout(
      main_title = paste0(.sector, " emissions"),
      subtitle = "",
      x_title = stringr::str_to_title(unique(plot_data$geog_level)),
      y_title = "Metric tons CO<sub>2</sub>e",
      legend_title = "Category"
    ) %>%
    plotly::layout(barmode = "stack")
}
