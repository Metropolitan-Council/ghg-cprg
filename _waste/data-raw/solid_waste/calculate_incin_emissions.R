#' Calculate GHG emissions from municipal solid waste sent to landfills using waste composition.
#' 
#' @param solid_waste_data table, waste activity data
#' @return a data table with geoid, source, inventory_year, value_activity, 
#' units_activity, value_emissions, and units_emissions
#' 
calculate_incin_emissions <- function(solid_waste_data){
  # assign factors
  fcc <- .4 # fraction of carbon content in MSW, IPCC default
  ffc <- .4 # fraction of fossil carbon in MSW, IPCC default
  co2_factor <- fcc * ffc * 44 / 12 # atomic weight of CO2:C
  co2_efficiency_wte <- .95 # efficiency of combustion for incineration, IPCC default
  co2_efficiency_onsite <- .71 # efficiency of combustion for onsite burning, GHG Protocol default (IPCC does not provide one)
  n2o_emissions_factor_wte <- 50 # aggregate emissions factor for incineration, g N2O/metric tons waste, GHG Protocol default
  n2o_emissions_factor_onsite <- 150 # aggregate emissions factor for open burning, g N2O/metric tons waste, GHG Protocol default
  
  incin_factors <- tibble::tibble(
    source = c("Waste to energy", "Onsite"),
    co2 = co2_factor * c(co2_efficiency_wte, co2_efficiency_onsite),
    n2o = c(n2o_emissions_factor_wte, n2o_emissions_factor_onsite) *
      units::as_units("gram") %>%
      units::set_units("metric_ton") %>%
      as.numeric()
  )
  
  incineration_emissions <- solid_waste_data %>%
    dplyr::filter(source %in% c("Waste to energy", "Onsite")) %>%
    dplyr::left_join(incin_factors, by = dplyr::join_by(source)) %>%
    dplyr::mutate(
      "Metric tons CO2" = value_activity * co2,
      "Metric tons N2O" = value_activity * n2o
    ) %>%
    tidyr::pivot_longer(
      c("Metric tons CO2", "Metric tons N2O"),
      names_to = "units_emissions",
      values_to = "value_emissions"
    ) %>%
    dplyr::select(
      -c(state_total, co2, n2o)
    )
}
