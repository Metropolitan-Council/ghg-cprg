testthat::test_that("Compiled agriculture emissions", {
  ag_emissions <- readRDS(file.path(here::here(), "_agriculture/data/_agricultural_emissions.RDS"))

  testthat::expect_equal(
    unique(ag_emissions$data_source),
    c(
      "USDA livestock census",
      "USDA crop production survey",
      "USDA fertilizer purchase census and EPA SIT fertilizer application data"
    )
  )

  # check match with PDFs
  testthat::expect_equal(
    unique(ag_emissions$source),
    c(
      "Enteric fermentation",
      "Manure management",
      "Direct manure soil emissions",
      "Indirect manure runoff emissions",
      "Soil residue emissions",
      "Onsite fertilizer emissions",
      "Runoff fertilizer emissions"
    )
  )

  # enteric fermentation is the biggest emitter
  ag_emissions %>%
    filter(inventory_year == 2021) %>%
    group_by(source) %>%
    summarize(mt_co2e = sum(mt_co2e)) %>%
    ungroup() %>%
    filter(mt_co2e == max(mt_co2e)) %>%
    magrittr::extract2("source") %>%
    testthat::expect_equal("Enteric fermentation")
})
