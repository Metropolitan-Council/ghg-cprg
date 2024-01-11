if (exists("metc_bbx") == FALSE) {
  metc_bbx <- list(
    lat1 = -93.521749,
    lon1 = 44.854051,
    lat2 = -92.899649,
    lon2 = 45.081892
  )


  council_leaflet <- function() {
    leaflet::leaflet() %>%
      leaflet::addMapPane(name = "Carto Positron", zIndex = 430) %>%
      leaflet::addProviderTiles("CartoDB.PositronOnlyLabels",
        options = leaflet::leafletOptions(pane = "Carto Positron"),
        group = "Carto Positron"
      ) %>%
      leaflet::addProviderTiles("CartoDB.PositronNoLabels",
        group = "Carto Positron"
      )
    # %>%
      # leaflet::fitBounds(metc_bbx$lat1, metc_bbx$lon1, metc_bbx$lat2, metc_bbx$lon2)
  }

  cli::cli_inform(
    c("v" = "leaflet helpers\n"),
    .frequency = "once",
    .frequency_id = "leaflet_helpers"
  )
}
