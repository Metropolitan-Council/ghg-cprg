testthat::test_that("state DOT VMT", {
  dot_vmt <- readRDS(file.path(here::here(), "_transportation/data/dot_vmt.RDS"))

  testthat::expect_equal(nrow(dot_vmt), 237)

  # ensure we have all available years
  testthat::expect_equal(
    unique(dot_vmt$vmt_year),
    c(
      "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008",
      "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016",
      "2017", "2018", "2019", "2020", "2021", "2022", "2023"
    )
  )

  # check match with PDFs
  dot_vmt %>%
    filter(
      county_name == "Pierce",
      vmt_year >= 2018
    ) %>%
    arrange(desc(vmt_year)) %>%
    magrittr::extract2("daily_vmt") %>%
    testthat::expect_equal(c(1103323, 1067675, 1074726, 949687, 1099233, 1068300))

  # Hennepin has highest annual
  dot_vmt %>%
    filter(annual_vmt == max(annual_vmt)) %>%
    magrittr::extract2("county_name") %>%
    testthat::expect_equal("Hennepin")
})
