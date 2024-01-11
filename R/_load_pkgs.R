if (exists("load_packages") == FALSE) {
  # Data Manipulation ------
  suppressMessages(library(dplyr, quietly = TRUE))
  suppressMessages(library(tidyr, quietly = TRUE))
  suppressMessages(library(janitor, quietly = TRUE))
  suppressMessages(library(purrr, quietly = TRUE))
  suppressMessages(library(furrr, quietly = TRUE))
  # suppressMessages(library(future, quietly = TRUE))
  suppressMessages(library(stringr, quietly = TRUE))
  suppressMessages(library(sf, quietly = TRUE))
  suppressMessages(library(tidycensus, quietly = TRUE))
  suppressMessages(library(councilR, quietly = TRUE))
  suppressMessages(library(lubridate, quietly = TRUE))
  
  # Workflow ------
  suppressMessages(library(keyring, quietly = TRUE))
  suppressMessages(library(here, quietly = TRUE))
  suppressMessages(library(fs, quietly = TRUE))


  # Plotting, Mapping ----
  suppressMessages(library(ggplot2, quietly = TRUE))
  suppressMessages(library(plotly, quietly = TRUE))
  suppressMessages(library(RColorBrewer, quietly = TRUE))
  suppressMessages(library(leaflet, quietly = TRUE))
  suppressMessages(library(scales, quietly = TRUE))
  suppressMessages(library(webshot, quietly = TRUE))
  

  # tables -----
  suppressMessages(library(knitr, quietly = TRUE))
  suppressMessages(library(kableExtra, quietly = TRUE))

  # Specialty -----
  suppressMessages(library(osrm, quietly = TRUE))

  # require(Hmisc)
  # getRs("reptools.r") # Loads reptools.r from Github

  load_packages <- TRUE
  cli::cli_inform(
    c("v" = "Packages\n"),
    .frequency = "once",
    .frequency_id = "load_packages"
  )
}
