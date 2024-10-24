#' Plot county level emissions for any given sector, no colors, ordered by total
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
plot_county_sector_emissions <- function(county_emissions,
                                         .sector,
                                         .plotly_source) {
  plot_data <- county_emissions %>%
    dplyr::filter(sector == .sector) %>%
    rowwise()

  if (nrow(plot_data) == 0) {
    return(plot_ly(
      source = .plotly_source,
      type = "bar"
    ))
  }

  plot_data <- plot_data %>%
    mutate(
      rounded_tons = ifelse(
        max(emissions_metric_tons_co2e) > 1000000,
        paste0(round(emissions_metric_tons_co2e / 1000000, digits = 2), " million metric tons CO<sub>2</sub>e", "<br>"),
        paste0(round(emissions_metric_tons_co2e / 1000, digits = 0), " thousand metric tons CO<sub>2</sub>e", "<br>")
      )
    )


  plot_ly(
    data = plot_data,
    type = "bar",
    source = .plotly_source,
    x = ~emissions_metric_tons_co2e,
    y = ~ reorder(geog_name, emissions_metric_tons_co2e),
    color = ~sector,
    colors = unlist(sector_colors),
    marker = list(
      line = list(
        width = 0
      )
    ),
    hovertemplate = ~ paste0(
      geog_name, " County", "<br>",
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
