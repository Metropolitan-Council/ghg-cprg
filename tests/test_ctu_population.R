testthat::test_that("CTU population is as expected", {
  ctu_population <- readRDS(file.path(here::here(), "_meta/data/ctu_population.RDS"))
  
  testthat::expect_equal(
    unique(ctu_population$geoid),
    c("27003", "27019", "27037", "27053", "27123", "27139", "27163")
    )
  
  
  # within 10,000 of ACS estimates
  # ask data team about this
  cprg_population <- readRDS(file.path(here::here(), "_meta/data/cprg_population.RDS"))
  
  ctu_population_2021 <- ctu_population %>% 
    dplyr::filter(inventory_year == 2021) %>% 
    dplyr::select(
      geoid,
      county_population
    ) %>% 
    unique() %>% 
    dplyr::inner_join(cprg_population, by = join_by(geoid))
  
  census_county_population <- readRDS(file.path(here::here(), "_meta/data/census_county_population.RDS"))
  
  # census_2010_2020 <- census_county_population %>% 
  #   filter(
  #     geoid %in% c("27003", "27019", "27037", "27053", "27123", "27139", "27163"),
  #     population_year %in% c("2010", "2020")
  #   ) %>% 
  #   mutate(
  #     population_year = as.numeric(population_year)
  #   ) %>% 
  #   dplyr::inner_join(ctu_population %>% 
  #                       select(geoid, county_population, inventory_year) %>% 
  #                       unique(), 
  #                     by = join_by(
  #                       geoid, 
  #                       population_year == inventory_year
  #                       )
  #                     )
  
  # plot census v ctu_pop
  census_ctu_joined <- census_county_population %>%
    filter(
      geoid %in% c("27003", "27019", "27037", "27053", "27123", "27139", "27163")
      ) %>%
    mutate(
        population_year = as.numeric(population_year)
      ) %>%
    dplyr::inner_join(ctu_population %>%
                        select(geoid, county_population, inventory_year) %>%
                        unique(),
                      by = join_by(
                        geoid,
                        population_year == inventory_year
                      )
    )
  # 
  # ggplot(census_ctu_joined, aes(x = population_year, y = population, color = geoid)) +
  #   geom_line(linetype = 2) +
  #   geom_line(aes(y = county_population))
  
  census_ctu <- census_ctu_joined %>% 
    filter(
      population_year != 2010
    ) %>% 
    mutate(
      percent_difference = (population - county_population)/population
    ) %>% 
    filter(
      percent_difference > 0.03
    )
})
