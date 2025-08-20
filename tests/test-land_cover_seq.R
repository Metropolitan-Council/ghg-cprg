testthat::test_that("land cover", {
  land_cover_c <- readRDS(file.path(here::here(), "_nature/data/land_cover_carbon.rds"))

  testthat::expect_equal(nrow(land_cover_c), 5)

  testthat::expect_equal(
    names(land_cover_c),
    c("land_cover_type", "seq_mtco2e_sqkm", "stock_mtco2e_sqkm")
  )

  land_cover_c %>%
    filter(land_cover_type == "Tree") %>%
    magrittr::extract2("seq_mtco2e_sqkm") %>%
    testthat::expect_equal(-329.0748, tolerance = 0.001)
})


testthat::test_that("sequestration totals", {
  wc_county_c <- readRDS(file.path(here::here(), "_nature/data/nlcd_county_landcover_sequestration_allyrs.rds")) %>%
    filter(inventory_year == 2021)


  county_seq_total <- summarize(wc_county_c, seq_total = sum(sequestration_potential))
  county_stock_total <- summarize(wc_county_c, stock_total = sum(stock_potential))

  testthat::expect_equal(sum(county_stock_total$stock_total), -269008720)

  sum(county_seq_total$seq_total) #-2180841
  sum(county_stock_total$stock_total) #-257,462,245
})
