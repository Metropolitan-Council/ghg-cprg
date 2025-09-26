testthat::test_that("Gross emissions rates make sense", {
  gross_emissions_rates <- readRDS(file.path(here::here(), "_transportation/data/gross_emissions_rates.RDS"))
  epa_moves <- readRDS(file.path(here::here(), "_transportation/data/epa_moves.RDS"))

  # correct number of counties
  testthat::expect_equal(
    gross_emissions_rates$county_name %>% unique() %>% length(),
    11
  )

  n_years <- length(unique(gross_emissions_rates$vmt_year))

  # expect all counties to have an observation for each year
  testthat::expect_equal(
    gross_emissions_rates %>%
      group_by(county_name) %>%
      count() %>%
      pull(n) %>%
      sum(),
    n_years * 11
  )

  # expect that the maximum gross is less than the maximum of EPA MOVES rates
  testthat::expect_lt(
    max(gross_emissions_rates$emissions_per_mile),
    epa_moves %>%
      ungroup() %>%
      filter(co2_co2_equivalent == max(co2_co2_equivalent)) %>%
      pull(co2_co2_equivalent)
  )

  # the mean of gross and EPA MOVES rates should be somewhat similar
  testthat::expect_equal(
    gross_emissions_rates %>%
      filter(vmt_year == 2022) %>%
      pull(emissions_per_mile) %>%
      mean(),
    mean(epa_moves$co2_co2_equivalent),
    tolerance = 150
  )

  # expect historic emissions rates to be greater
  # than future emissions rates
  testthat::expect_gt(
    gross_emissions_rates %>%
      filter(vmt_year == min(vmt_year)) %>%
      pull(emissions_per_mile) %>%
      mean(),
    gross_emissions_rates %>%
      filter(vmt_year == max(vmt_year)) %>%
      pull(emissions_per_mile) %>%
      mean()
  )


  # expect rates in the more rural counties to be greater than
  # rates in the more urbanized counties
  testthat::expect_gt(
    gross_emissions_rates %>%
      filter(
        vmt_year == min(vmt_year),
        county_name %in% c("Carver", "Scott", "St. Croix", "Chisago")
      ) %>%
      pull(emissions_per_mile) %>%
      mean(),
    gross_emissions_rates %>%
      filter(
        vmt_year == min(vmt_year),
        county_name %in% c("Hennepin", "Ramsey")
      ) %>%
      pull(emissions_per_mile) %>%
      mean()
  )


  testthat::expect_gt(
    gross_emissions_rates %>%
      filter(
        vmt_year == 2022,
        county_name %in% c("Carver", "Scott", "St. Croix", "Chisago")
      ) %>%
      pull(emissions_per_mile) %>%
      mean(),
    gross_emissions_rates %>%
      filter(
        vmt_year == 2022,
        county_name %in% c("Hennepin", "Ramsey")
      ) %>%
      pull(emissions_per_mile) %>%
      mean()
  )
})
