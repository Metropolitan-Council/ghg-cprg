testthat::test_that("Sectored, expanded energy data is as expected", {
  cprg_county <- readRDS(file.path(here::here(), "_meta/data/cprg_county.RDS"))

  nrel_slope_proportions <- readRDS(file.path(here::here(), "_energy/data-raw/nrel_slope/nrel_slope_proportions.RDS"))

  electric_natgas_nrel_proportioned_expanded <- readRDS(file.path(here::here(), "_energy/data/electric_natgas_nrel_proportioned_expanded.RDS"))


  testthat::expect_equal(
    sort(unique(electric_natgas_nrel_proportioned_expanded$county_name)),
    sort(c(
      "Scott", "Chisago", "Dakota", "Anoka", "Ramsey", "Washington",
      "Sherburne", "Hennepin", "Carver", "Pierce", "St. Croix"
    ))
  )

  testthat::expect_equal(
    sort(unique(electric_natgas_nrel_proportioned_expanded$source)),
    sort(c("Electricity", "Natural gas"))
  )


  testthat::expect_equal(
    sort(unique(electric_natgas_nrel_proportioned_expanded$sector)),
    sort(c("commercial", "residential", "industrial"))
  )

  # test that the sector proportions total up to 1, as expected
  electric_natgas_nrel_proportioned_expanded %>%
    group_by(county_name, year, source) %>%
    summarize(sector_proportion = round(sum(sector_proportion)), .groups = "keep") %>%
    filter(sector_proportion != 1) %>%
    nrow() %>%
    testthat::expect_equal(0)

  # DEPRECATED -----

  # energy_calculated <- bind_rows(
  #   readRDS(file.path(here::here(), "_energy/data/minnesota_county_ElecEmissions.RDS")) %>%
  #     bind_rows(readRDS(file.path(here::here(), "_energy/data/wisconsin_county_ElecEmissions.RDS")) %>%
  #       rename(county = county_name)),
  #   readRDS(file.path(here::here(), "_energy/data/minnesota_county_GasEmissions.RDS")) %>%
  #     bind_rows(readRDS(file.path(here::here(), "_energy/data/wisconsin_county_GasEmissions.RDS")) %>%
  #       rename(county = county_name))
  # )
  #
  # electric_natgas_nrel_proportioned %>%
  #   group_by(county, source) %>%
  #   summarize(prop_emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e), .groups = "keep") %>%
  #   left_join(
  #     energy_calculated %>%
  #       group_by(county, sector) %>%
  #       summarize(emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e), .groups = "keep"),
  #     by = c("county",
  #       "source" = "sector"
  #     )
  #   ) %>%
  #   filter(round(emissions_metric_tons_co2e) != round(prop_emissions_metric_tons_co2e)) %>%
  #   nrow() %>%
  #   # temporarily equal to 11  -- since only NG gets "total energy" for 11 counties
  #   testthat::expect_equal(11)
  #
  #
  # testthat::expect_equal(nrow(electric_natgas_nrel_proportioned), 132)
})
