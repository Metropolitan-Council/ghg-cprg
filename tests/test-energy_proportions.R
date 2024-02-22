testthat::test_that("Sectored energy data totals to measurements", {
  cprg_county <- readRDS(file.path(here::here(), "_meta/data/cprg_county.RDS"))

  nrel_slope_proportions <- readRDS(file.path(here::here(), "_energy/data-raw/nrel_slope/nrel_slope_proportions.RDS"))

  electric_natgas_nrel_proportioned <- readRDS(file.path(here::here(), "_energy/data/electric_natgas_nrel_proportioned.RDS"))

  energy_calculated <- bind_rows(
    readRDS(file.path(here::here(), "_energy/data/minnesota_county_ElecEmissions.RDS")) %>%
      bind_rows(readRDS(file.path(here::here(), "_energy/data/wisconsin_county_ElecEmissions.RDS")) %>%
        rename(county = county_name)),
    readRDS(file.path(here::here(), "_energy/data/minnesota_county_GasEmissions.RDS")) %>%
      bind_rows(readRDS(file.path(here::here(), "_energy/data/wisconsin_county_GasEmissions.RDS")) %>%
        rename(county = county_name))
  )

  electric_natgas_nrel_proportioned %>%
    group_by(county, source) %>%
    summarize(prop_emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e), .groups = "keep") %>%
    left_join(
      energy_calculated %>%
        group_by(county, sector) %>%
        summarize(emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e), .groups = "keep"),
      by = c("county",
        "source" = "sector"
      )
    ) %>%
    filter(round(emissions_metric_tons_co2e) != round(prop_emissions_metric_tons_co2e)) %>%
    nrow() %>%
    testthat::expect_equal(0)


  testthat::expect_equal(nrow(electric_natgas_nrel_proportioned), 66)
})
