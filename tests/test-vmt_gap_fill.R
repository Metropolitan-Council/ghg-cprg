testthat::test_that("VMT gap fill model input data quality", {
  ctu_pop_jobs_vmt <- readRDS(file.path(here::here(), "_transportation/data/vmt_model_data.RDS"))

  # Test 1: County VMT should be >= sum of COCTU VMT
  vmt_consistency <- ctu_pop_jobs_vmt %>%
    dplyr::filter(
      !is.na(daily_vmt),
      !is.na(county_daily_vmt),
      inventory_year <= 2022
    ) %>%
    dplyr::group_by(county_name, geoid, inventory_year, county_daily_vmt) %>%
    dplyr::summarize(
      sum_coctu_vmt = sum(daily_vmt, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      vmt_diff = county_daily_vmt - sum_coctu_vmt
    )

  # No county-year should have negative marginal VMT
  testthat::expect_true(
    all(vmt_consistency$vmt_diff >= 0),
    info = paste0(
      "Counties with sum(COCTU VMT) > County VMT: ",
      paste(
        vmt_consistency %>%
          dplyr::filter(vmt_diff < 0) %>%
          dplyr::mutate(label = paste0(county_name, " ", inventory_year)) %>%
          dplyr::pull(label),
        collapse = ", "
      )
    )
  )
})


testthat::test_that("VMT gap fill output - no negative values", {
  mndot_vmt_ctu_gap_filled <- readRDS(file.path(here::here(), "_transportation/data/mndot_vmt_ctu_gap_filled.RDS"))

  # Test 1: No COCTU should have negative VMT
  negative_vmt <- mndot_vmt_ctu_gap_filled %>%
    dplyr::filter(final_city_vmt < 0)

  testthat::expect_equal(
    nrow(negative_vmt),
    0,
    info = paste0(
      "Found ", nrow(negative_vmt), " COCTU-year observations with negative VMT. ",
      "Affected COCTUs: ",
      paste(unique(negative_vmt$ctu_name_full_county), collapse = ", ")
    )
  )

  # Test 2: All VMT values should be numeric and finite
  testthat::expect_true(
    all(is.finite(mndot_vmt_ctu_gap_filled$final_city_vmt)),
    info = "Some VMT values are non-finite (NA, Inf, -Inf)"
  )
})


testthat::test_that("Blaine Ramsey County - never has negative VMT", {
  mndot_vmt_ctu_gap_filled <- readRDS(file.path(here::here(), "_transportation/data/mndot_vmt_ctu_gap_filled.RDS"))

  # Filter for Blaine in Ramsey County specifically
  blaine_ramsey <- mndot_vmt_ctu_gap_filled %>%
    dplyr::filter(
      stringr::str_detect(ctu_name_full_county, "Blaine.*Ramsey")
    )

  # Blaine in Ramsey County should exist in the data
  testthat::expect_true(
    nrow(blaine_ramsey) > 0,
    info = "Blaine, Ramsey County not found in gap-filled data"
  )

  # All VMT values for Blaine Ramsey should be non-negative
  testthat::expect_true(
    all(blaine_ramsey$final_city_vmt >= 0),
    info = paste0(
      "Blaine, Ramsey County has negative VMT in years: ",
      paste(
        blaine_ramsey %>%
          dplyr::filter(final_city_vmt < 0) %>%
          dplyr::pull(inventory_year),
        collapse = ", "
      )
    )
  )

  # Minimum VMT should be >= 0
  testthat::expect_gte(
    min(blaine_ramsey$final_city_vmt),
    0,
    label = "Minimum VMT for Blaine, Ramsey County"
  )

  # Check that Blaine has reasonable VMT values (not zero for all years)
  testthat::expect_true(
    any(blaine_ramsey$final_city_vmt > 0),
    info = "Blaine, Ramsey County has zero VMT for all years"
  )
})



testthat::test_that("County scaling factors - reasonable values", {
  mndot_vmt_ctu_gap_filled <- readRDS(file.path(here::here(), "_transportation/data/mndot_vmt_ctu_gap_filled.RDS"))

  # Filter to only modeled COCTUs (those with scaling factors != 1)
  modeled_coctus <- mndot_vmt_ctu_gap_filled %>%
    dplyr::filter(
      final_vmt_source == "MetC Modeled",
      !is.na(county_ctu_scaling_factor)
    )

  # Test 1: All scaling factors should be non-negative
  testthat::expect_true(
    all(modeled_coctus$county_ctu_scaling_factor >= 0, na.rm = TRUE),
    info = "Some COCTUs have negative scaling factors"
  )

  # Test 2: Scaling factors should be finite
  testthat::expect_true(
    all(is.finite(modeled_coctus$county_ctu_scaling_factor)),
    info = "Some scaling factors are non-finite (NA, Inf, -Inf)"
  )

  # Test 3: Check for extremely large scaling factors (> 100)
  extreme_scales <- modeled_coctus %>%
    dplyr::filter(county_ctu_scaling_factor > 100)

  testthat::expect_equal(
    nrow(extreme_scales),
    0,
    info = paste0(
      "Found ", nrow(extreme_scales),
      " COCTUs with extreme scaling factors (>100)"
    )
  )
})


testthat::test_that("Gap-filled data structure", {
  mndot_vmt_ctu_gap_filled <- readRDS(file.path(here::here(), "_transportation/data/mndot_vmt_ctu_gap_filled.RDS"))

  # Expected columns
  expected_cols <- c(
    "inventory_year", "coctu_id_gnis", "geoid", "gnis",
    "ctu_name_full_county", "county_ctu_scaling_factor",
    "final_city_vmt", "final_vmt_source"
  )

  testthat::expect_true(
    all(expected_cols %in% names(mndot_vmt_ctu_gap_filled)),
    info = paste0(
      "Missing columns: ",
      paste(setdiff(expected_cols, names(mndot_vmt_ctu_gap_filled)), collapse = ", ")
    )
  )

  # Check expected number of COCTUs (193 in metro area)
  testthat::expect_equal(
    length(unique(mndot_vmt_ctu_gap_filled$coctu_id_gnis)),
    193,
    label = "Number of unique COCTUs"
  )

  # Check that we have data for expected years (2010-2050)
  testthat::expect_true(
    all(2010:2050 %in% unique(mndot_vmt_ctu_gap_filled$inventory_year)),
    info = "Missing expected years in gap-filled data"
  )

  # Check data sources are valid
  valid_sources <- c("MnDOT VMT Reports", "MetC Modeled", "Regional Travel Demand Model")
  testthat::expect_true(
    all(mndot_vmt_ctu_gap_filled$final_vmt_source %in% valid_sources),
    info = "Invalid data sources found"
  )
})


testthat::test_that("Consistency between input and output data", {
  ctu_pop_jobs_vmt <- readRDS(file.path(here::here(), "_transportation/data/vmt_model_data.RDS"))
  mndot_vmt_ctu_gap_filled <- readRDS(file.path(here::here(), "_transportation/data/mndot_vmt_ctu_gap_filled.RDS"))

  # Test: COCTUs with reported VMT in input should have "MnDOT VMT Reports"
  # or "Regional Travel Demand Model" in output for years <= 2022
  input_reported <- ctu_pop_jobs_vmt %>%
    dplyr::filter(
      !is.na(daily_vmt),
      inventory_year <= 2022
    ) %>%
    dplyr::select(coctu_id_gnis, inventory_year, daily_vmt)

  output_reported <- mndot_vmt_ctu_gap_filled %>%
    dplyr::filter(inventory_year <= 2022) %>%
    dplyr::inner_join(
      input_reported,
      by = c("coctu_id_gnis", "inventory_year")
    )

  # All joined records should have consistent VMT values
  # (allowing for small differences due to rounding)
  vmt_differences <- output_reported %>%
    dplyr::mutate(
      diff = abs(final_city_vmt - daily_vmt),
      pct_diff = diff / daily_vmt * 100
    ) %>%
    dplyr::filter(pct_diff > 0.1) # More than 0.1% difference

  testthat::expect_equal(
    nrow(vmt_differences),
    0,
    info = paste0(
      "Found ", nrow(vmt_differences),
      " COCTUs where reported VMT differs from gap-filled VMT by >0.1%"
    )
  )
})
