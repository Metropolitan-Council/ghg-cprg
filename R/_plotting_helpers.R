# plotting helpers!
source(file.path(here::here(), "R/cprg_colors.R"))
source(file.path(here::here(), "R/plot_county_emissions.R"))
source(file.path(here::here(), "R/plot_sector_county_emissions.R"))

round_emissions_metric_tons_co2e <- function(x) {
  ifelse(
    max(x) > 1000000,
    paste0(round(x / 1000000, digits = 2), " million metric tons CO<sub>2</sub>e", "<br>"),
    paste0(round(x / 1000, digits = 0), " thousand metric tons CO<sub>2</sub>e", "<br>")
  )
}
