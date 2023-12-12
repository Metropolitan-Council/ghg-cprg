if (exists("load_packages") == FALSE) {
  # Data Manipulation ------
  suppressMessages(library(data.table, quietly = T))
  suppressMessages(library(bit64, quietly = T))
  suppressMessages(library(openxlsx, quietly = T))
  suppressMessages(library(dplyr, quietly = T))
  suppressMessages(library(tidyr, quietly = T))
  suppressMessages(library(janitor, quietly = T))
  suppressMessages(library(purrr, quietly = T))
  suppressMessages(library(furrr, quietly = T))
  suppressMessages(library(future, quietly = T))
  suppressMessages(library(stringr, quietly = T))
  suppressMessages(library(srvyr, quietly = T))
  suppressMessages(library(sf, quietly = T))
  suppressMessages(library(tidycensus, quietly = T))
  suppressMessages(library(fuzzyjoin, quietly = T))
  suppressMessages(library(councilR, quietly = T))

  # Databases -----
  # suppressMessages(library(ROracle, quietly = T))
  # suppressMessages(library(DBI, quietly = T))
  # suppressMessages(library(odbc, quietly = T))

  # Workflow ------
  suppressMessages(library(keyring, quietly = T))
  suppressMessages(library(here, quietly = T))
  suppressMessages(library(fs, quietly = T))
  suppressMessages(library(webshot, quietly = T))


  # Plotting, Mapping ----
  suppressMessages(library(ggplot2, quietly = T))
  suppressMessages(library(plotly, quietly = T))
  suppressMessages(library(janitor, quietly = T))
  suppressMessages(library(lubridate, quietly = T))
  suppressMessages(library(RColorBrewer, quietly = T))
  suppressMessages(library(leaflet, quietly = T))
  suppressMessages(library(htmlwidgets, quietly = T))
  suppressMessages(library(ggpattern, quietly = T))
  suppressMessages(library(scales, quietly = T))

  # tables -----
  suppressMessages(library(knitr, quietly = T))
  suppressMessages(library(kableExtra, quietly = T))


  # Specialty -----
  # devtools::install_github("byu-transpolab/nhts2017") # will not install
  suppressMessages(library(haven, quietly = T))
  suppressMessages(library(osrm, quietly = T))

  # require(Hmisc)
  # getRs("reptools.r") # Loads reptools.r from Github

  load_packages <- TRUE
  cli::cli_inform(
    c("v" = "Packages\n"),
    .frequency = "once",
    .frequency_id = "load_packages"
  )
}
