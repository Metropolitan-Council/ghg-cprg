testthat::test_that("output matches reported values", {
  dot_vmt <- readRDS(file.path(here::here(), "_transportation/data/dot_vmt.RDS"))

  testthat::expect_equal(nrow(dot_vmt), 46)


  dot_vmt %>%
    filter(county == "Pierce") %>%
    magrittr::extract2("daily_vmt") %>%
    testthat::expect_equal(c(1067675, 1074726, 949687, 1099233, 1068300))
})
