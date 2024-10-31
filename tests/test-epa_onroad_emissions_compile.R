testthat::test_that("Compiled onroad emissions", {
  epa_onroad_emissions_compile <- readRDS(file.path(here::here(), "_transportation/data/epa_onroad_emissions_compile.RDS"))

  # test for values in factor/character columns

  testthat::expect_equal(
    unique(epa_onroad_emissions_compile$data_source),
    c("EQUATES", "National Emissions Inventory", "Air Emissions Modeling")
  )

  testthat::expect_equal(
    unique(epa_onroad_emissions_compile$pollutant),
    c(
      "emissions_metric_tons_co2e", "co2", "ch4", "n2o", "co", "no",
      "nox", "so2", "nh3", "pm10_pri", "pm25_pri"
    )
  )

  testthat::expect_equal(
    unique(epa_onroad_emissions_compile$vehicle_type),
    c(
      "Motorcycles", "Passenger cars", "Passenger trucks", "Light commercial trucks",
      "Intercity buses", "Transit buses", "School buses", "Refuse trucks",
      "Single unit short-haul trucks", "Single unit long-haul trucks",
      "Motor homes", "Combination short-haul trucks", "Combination long-haul trucks",
      "Trucks and buses", "Gas stations"
    )
  )

  testthat::expect_equal(
    sort(unique(epa_onroad_emissions_compile$fuel_type)),
    c(
      "Compressed natural gas (CNG)", "Diesel", "Electric", "Ethanol (E-85)",
      "Gasoline"
    )
  )
})


testthat::test_that("Emissions have increased since including all emissions processes", {
  # skip this test if offline
  testthat::skip_if_offline()

  # download specific version of epa_onroad_emissions_compile from GitHub
  # Commit ID 17b4349fc5874562befaa56a85ef3f740bb1708a
  file_name <- tempfile()
  download.file("https://github.com/Metropolitan-Council/ghg-cprg/raw/17b4349fc5874562befaa56a85ef3f740bb1708a/_transportation/data/epa_onroad_emissions_compile.RDS",
    destfile = file_name, quiet = TRUE
  )

  # read in and create previous version aggregation
  prev_epa_onroad_emissions_compile <- readRDS(file_name)
  prev_onroad_emissions <- prev_epa_onroad_emissions_compile %>%
    filter(
      pollutant == "emissions_metric_tons_co2e",
      !vehicle_type %in% c("Gas stations", "Trucks and buses")
    ) %>%
    unique() %>%
    group_by(
      emissions_year, data_source, geoid, county_name,
      vehicle_weight_label,
      vehicle_fuel_label,
      category
    ) %>%
    # group and summarize emissions
    summarize(
      emissions_metric_tons_co2e = sum(emissions, na.rm = TRUE),
      vehicle_types_included = paste0(unique(vehicle_type), collapse = ", "),
      fuel_types_included = paste0(unique(fuel_type), collapse = ", "),
      scc6_included = paste(unique(scc6), collapse = ", "),
      .groups = "keep"
    ) %>%
    ungroup()

  # pull onroad emissions compile from directory
  epa_onroad_emissions_compile <- readRDS(file.path(here::here(), "_transportation/data/epa_onroad_emissions_compile.RDS"))
  onroad_emissions <- epa_onroad_emissions_compile %>%
    filter(
      pollutant == "emissions_metric_tons_co2e",
      !vehicle_type %in% c("Gas stations", "Trucks and buses")
    ) %>%
    unique() %>%
    group_by(
      emissions_year, data_source, geoid, county_name,
      vehicle_weight_label,
      vehicle_fuel_label,
      category
    ) %>%
    summarize(
      emissions_metric_tons_co2e = sum(emissions, na.rm = TRUE),
      vehicle_types_included = paste0(unique(vehicle_type), collapse = ", "),
      fuel_types_included = paste0(unique(fuel_type), collapse = ", "),
      scc6_included = paste(unique(scc6), collapse = ", "),
      .groups = "keep"
    ) %>%
    ungroup()

  # create comparison dataset
  onroad_compare <- full_join(
    onroad_emissions,
    prev_onroad_emissions,
    # join by all columns EXCEPT emissions_metric_tons_co2e
    join_by(
      emissions_year, data_source, geoid, county_name, vehicle_weight_label,
      vehicle_fuel_label, category,
      vehicle_types_included, fuel_types_included, scc6_included
    ),
    suffix = c("_new", "_old")
  ) %>%
    # find difference and percent difference
    mutate(
      emissions_diff = emissions_metric_tons_co2e_new - emissions_metric_tons_co2e_old,
      emissions_pct_diff = emissions_diff / emissions_metric_tons_co2e_old
    )

  # the minimum value should be 0, because nothing decreased
  testthat::expect_equal(min(onroad_compare$emissions_pct_diff, na.rm = TRUE), 0)
  #
  testthat::expect_lte(max(onroad_compare$emissions_pct_diff, na.rm = TRUE), 0.25)
})
