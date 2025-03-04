testthat::test_that("WI gas breakdown adds up", {
  wi_emissions <- readRDS(file.path(here::here(), 
                                    "_waste/data/solid_waste_WI_allyrs.RDS"))%>% 
    group_by(
      geoid,
      inventory_year
      ) %>% 
    summarize(
      value_emissions = sum(value_emissions)
    )
  
  wi_emissions_gas <- readRDS(file.path(here::here(),
    "_waste/data/solid_waste_gas_WI_allyrs.RDS"))%>% 
    mutate(
      value_emissions = case_when(
        units_emissions == "Metric tons CH4" ~ value_emissions * gwp$ch4,
        units_emissions == "Metric tons N2O" ~ value_emissions * gwp$n2o,
        units_emissions == "Metric tons CO2" ~ value_emissions * gwp$co2
      )
    ) %>% 
    group_by(
      geoid,
      inventory_year
    ) %>% 
    summarize(
      value_emissions = sum(value_emissions)
    )
  
  testthat::expect_equal(wi_emissions$value_emissions, wi_emissions_gas$value_emissions)
})
