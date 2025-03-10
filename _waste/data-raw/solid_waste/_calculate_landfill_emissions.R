#' Calculate GHG emissions from municipal solid waste sent to landfills using waste composition.
#' 
#' @param solid_waste_data table, waste activity data
#' @param waste_characterization table, output of 01_mpca_waste_characterization.R
#' @param methane_recovery single value, percentage of landfills using methane recovery
#' @return a data table with geoid, source, inventory_year, value_activity, 
#' units_activity, value_emissions, and units_emissions
#' 
calculate_landfill_emissions <- function(solid_waste_data, waste_characterization,
                                         methane_recovery = 0) {
  # create empty methane recovery df
  inventory_year = unique(solid_waste_data$inventory_year)
  methane_recovery_table = tibble::tibble(
    inventory_year, percent_recovered = rep(methane_recovery, length(inventory_year))
    )

  # methane correction factor (MCF)
  # assuming landfills managed well, semi-aerobic (see GHG Protocol)
  mcf <- 0.5
  # fraction of degradable organic carbon degraded (DOC_f)
  doc_f <- 0.6
  # fraction of methane in landfill gas (F)
  f <- 0.5
  # oxidation factor (OX)
  ox <- 0.1 # for well-managed landfills
  
  
  # Calculate DOC using IPCC equation (see documentation)
  # waste composition from MPCA report https://www.pca.state.mn.us/sites/default/files/w-sw1-60.pdf
  # cleaned in _waste/data-raw/clean_tabula_tables.R
  
  ipcc_doc_factors <- tibble::tibble(
    Category = c("Paper", "Textiles", "Organics (Non-Food)", "Organics (Food)", "Wood"),
    Factor = c(0.4, 0.4, 0.17, 0.15, 0.3)
  )
  
  doc_sum <- waste_characterization %>%
    dplyr::inner_join(ipcc_doc_factors, by = dplyr::join_by(Category)) %>%
    dplyr::mutate(doc_content = Mean * Factor) %>%
    dplyr::summarize(doc_total = sum(doc_content), degradable_fraction = sum(Mean))
  
  doc <- doc_sum$doc_total
  
  # methane generation potential
  l_0 <- mcf * doc * doc_f * f * 16 / 12
  
  landfill_emissions <- solid_waste_data %>%
    dplyr::filter(source == "Landfill") %>%
    dplyr::left_join(methane_recovery_table, by = dplyr::join_by(inventory_year)) %>%
    dplyr::mutate(
      value_emissions = (value_activity * l_0) * (1 - ox) * (1-percent_recovered),
      units_emissions = "Metric tons CH4"
    ) %>%
    dplyr::select(
      -c(state_total, percent_recovered)
    )
  
  return(landfill_emissions)
}
