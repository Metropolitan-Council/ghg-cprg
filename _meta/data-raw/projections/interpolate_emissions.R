interpolate_emissions <- function(df) {
  df %>%
    mutate(emissions_year = as.numeric(emissions_year)) %>%
    group_by(scenario) %>%
    # complete sequence of years from min to max
    complete(emissions_year = seq(min(emissions_year), max(emissions_year), by = 1)) %>%
    # interpolate missing values linearly
    mutate(value_emissions = approx(emissions_year, value_emissions,
      xout = emissions_year, rule = 1
    )$y) %>%
    ungroup()
}
