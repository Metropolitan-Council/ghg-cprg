testthat::test_that("CTU population is as expected", {
  ctu_population <- readRDS(file.path(here::here(), "_meta/data/ctu_population.RDS"))

  testthat::expect_equal(
    unique(ctu_population$geoid),
    c("27003", "27019", "27037", "27053", "27123", "27139", "27163")
  )

  cprg_ctu <- readRDS(file.path(here::here(), "_meta/data/cprg_ctu.RDS")) %>%
    filter(county_name %in% c(
      "Anoka", "Dakota", "Ramsey", "Hennepin", "Washington",
      "Scott", "Carver"
    )) %>%
    arrange(ctu_name)

  testthat::expect_equal(
    unique(cprg_ctu$ctu_name), arrange(ctu_population, ctu_name) %>% pull(ctu_name) %>% unique()
  )
})

testthat::test_that("CTU population matches county totals", {
  census_county_population <- readRDS(file.path(here::here(), "_meta/data/census_county_population.RDS"))

  census_cprg_counties <- census_county_population %>%
    filter(
      geoid %in% c("27003", "27019", "27037", "27053", "27123", "27139", "27163")
    ) %>%
    arrange(
      geoid,
      population_year
    )

  ctu_counties <- ctu_population %>%
    select(geoid, county_population, inventory_year) %>%
    filter(inventory_year %in% 2000:2022) %>%
    unique() %>%
    arrange(
      geoid,
      inventory_year
    )

  for (county in unique(ctu_counties$geoid)) {
    ctu_county <- filter(ctu_counties, geoid == county)
    census_county <- filter(census_cprg_counties, geoid == county)
    tol <- 0.03 * max(ctu_county$county_population)

    testthat::expect_equal(
      census_county$population,
      ctu_county$county_population,
      tolerance = tol
    )
  }
})
