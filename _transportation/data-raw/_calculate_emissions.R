#' Calculate GHG emissions from vehicle miles traveled.
#'
#' @param vmt_data table, output of `calculate_vmt()`
#' @param emissions_factors table, with columns c("moves_year", "vehicle_weight",
#'    "co2", "co2_co2_equivalent", "ch4", "n2o")
#' @return a data table with columns 
#'   c("analysis_name", "mode_of_travel", "year", "vehicle_type",
#'   "vehicle_weight", "vehicle_weight_label", "zone", "vmt_same",
#'   "vmt_origin", "vmt_destination", "vmt_total", "moves_year", "co2",
#'   "co2_co2_equivalent", "ch4", "n2o", "total_co2", "total_ch4",
#'   "total_n2o", "total_co2_w_equiv", "emissions_metric_tons_co2e"
#'   )
#'
calculate_emissions <- function(vmt_data,
                                emissions_factors) {
  # browser()

  efficiency <- vmt_data %>%
    dplyr::ungroup() %>%
    dplyr::left_join(emissions_factors, by = "vehicle_weight") %>%
    dplyr::group_by(zone, vehicle_weight) %>%
    # calculate each GHG
    dplyr::mutate(
      vehicle_type = ifelse(vehicle_weight == "Passenger", "passenger", "commercial"),
      total_co2 = vmt_total * co2,
      total_ch4 = vmt_total * ch4,
      total_n2o = vmt_total * n2o,
      total_co2_w_equiv = vmt_total * co2_co2_equivalent,
      emissions_metric_tons_co2e = total_co2_w_equiv / 1000000
    ) %>%
    dplyr::ungroup()

  return(efficiency)
}
