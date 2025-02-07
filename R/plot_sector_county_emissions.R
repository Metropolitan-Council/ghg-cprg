#' Plot county level emissions for any given sector, no colors, ordered by total
#'
#' @param county_emissions data frame, compiled emissions. Must have columns
#'   "sector", "geog_name", "emissions_metric_tons_co2e", "source", "category",
#'   and "geog_level"
#' @param sector character, one of "Transportation", ....
#' @param plotly_source character, passed to source parameter in `plotly::plot_ly()`
#'
#' @return Plotly object
#' @export
#'
#' @importFrom plotly plot_ly layout
#' @importFrom dplyr filter
#' @importFrom stringr str_to_title
#' @importFrom councilR plotly_layout
#' @importFrom cli cli_alert_warning
plot_county_sector_emissions <- function(county_emissions,
                                         .sector,
                                         .plotly_source) {
  plot_data <- county_emissions %>%
    dplyr::filter(sector == .sector) %>%
    rowwise()

  if ("emissions_year" %in% names(plot_data)) {
    if (length(unique(plot_data$emissions_year)) > 1) {
      cli::cli_alert_warning("Plotting more than one year of data")
    }
  }

  if (nrow(plot_data) == 0) {
    return(plot_ly(
      source = .plotly_source,
      type = "bar"
    ))
  }

  plot_data <- plot_data %>%
    mutate(rounded_tons = round_emissions_metric_tons_co2e(value_emissions))

  plot_ly(
    data = plot_data,
    type = "bar",
    source = .plotly_source,
    x = ~value_emissions,
    y = ~ reorder(county_name, value_emissions),
    color = ~source,
    colors = unlist(source_colors),
   # marker_line=dict(width=2, color='black'),
    marker = list(
      line = list(
        width = 0
      )
    ),
    hovertemplate = ~ paste0(
      county_name, " County", "<br>",
      sector, " - ", category, ", ", source, "<br>",
      rounded_tons,
      "<extra></extra>"
    )
  ) %>%
    councilR::plotly_layout(
      main_title = if_else(.sector == "Nature", "Natural systems sequestration", paste0(.sector, " emissions")),
      subtitle = "",
      y_title = stringr::str_to_title(unique(plot_data$geog_level)),
      x_title = "Metric tons CO<sub>2</sub>e",
      legend_title = ""
    ) %>%
    plotly::layout(
      barmode = "stack",
      legend = list(
        traceorder = "reversed"
      ),
      yaxis = list(categoryorder = "total ascending")
    )
}
