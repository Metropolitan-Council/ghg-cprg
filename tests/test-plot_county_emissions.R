testthat::test_that("Correct plotly configurations", {
  source(file.path(here::here(), "R/plot_county_emissions.R"))
  source(file.path(here::here(), "R/_plotting_helpers.R"))
  

  county_emissions <- readRDS(file.path(here::here(), "_meta/data/cprg_county_emissions.RDS"))

  # transportation
  transpo_plot <- plot_county_emissions(county_emissions,
    .sector = "Transportation",
    .plotly_source = "testing"
  )

  testthat::expect_equal(class(transpo_plot)[[1]], "plotly")
  # check title
  testthat::expect_equal(
    transpo_plot[["x"]][["layoutAttrs"]][[1]][["title"]][["text"]],
    "Transportation emissions"
  )
  # check xaxis title
  testthat::expect_equal(
    transpo_plot[["x"]][["layoutAttrs"]][[1]][["xaxis"]][["title"]][["text"]],
    "County"
  )
  # check yaxis title
  testthat::expect_equal(
    transpo_plot[["x"]][["layoutAttrs"]][[1]][["yaxis"]][["title"]][["text"]],
    "Metric tons CO<sub>2</sub>e"
  )


  # missing data plot
  spec_transpo_plot <- plot_county_emissions(county_emissions,
    .sector = "Special Transportation",
    .plotly_source = "testing"
  )

  testthat::expect_equal(class(spec_transpo_plot)[[1]], "plotly")
  # check title
  testthat::expect_equal(
    spec_transpo_plot[["x"]][["layoutAttrs"]][[1]][["title"]][["text"]],
    NULL
  )
  # check xaxis title
  testthat::expect_equal(
    spec_transpo_plot[["x"]][["layoutAttrs"]][[1]][["xaxis"]][["title"]][["text"]],
    NULL
  )
  # check yaxis title
  testthat::expect_equal(
    spec_transpo_plot[["x"]][["layoutAttrs"]][[1]][["yaxis"]][["title"]][["text"]],
    NULL
  )
})
