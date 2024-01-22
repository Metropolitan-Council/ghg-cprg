testthat::test_that("state DOT VMT", {
  dot_vmt <- readRDS(file.path(here::here(), "_transportation/data/dot_vmt.RDS"))

  testthat::expect_equal(nrow(dot_vmt), 46)


  # check match with PDFs
  dot_vmt %>%
    filter(county == "Pierce") %>%
    magrittr::extract2("daily_vmt") %>%
    testthat::expect_equal(c(1067675, 1074726, 949687, 1099233, 1068300))

  # hennepin has highest annual
  dot_vmt %>%
    filter(annual_vmt == max(annual_vmt)) %>%
    magrittr::extract2("county") %>%
    testthat::expect_equal("Hennepin")
})
