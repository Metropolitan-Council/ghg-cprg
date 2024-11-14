testthat::test_that("state DOT VMT", {
  mndot_vmt_ctu <- readRDS(file.path(here::here(), "_transportation/data-raw/mndot/mndot_vmt_ctu.RDS"))
  
  testthat::expect_equal(nrow(mndot_vmt_ctu), 2442)
  
  # ensure we have all available years
  testthat::expect_equal(
    unique(dot_vmt$vmt_year),
    c("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", 
      "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", 
      "2017", "2018", "2019", "2020", "2021", "2022", "2023"))
  
  # Hennepin has highest annual
  mndot_vmt_ctu %>%
    filter(annual_vmt == max(annual_vmt)) %>%
    magrittr::extract2("ctu_name") %>%
    unique() %>% 
    testthat::expect_equal("Minneapolis")
  
  # expect that these CTUs are not included
  testthat::expect_false(
    unique(unique(mndot_vmt_ctu$ctu_name) %in% c("Nowthen", 
                                                 "Fort Snelling",
                                                 "May",
                                                 "Empire",
                                                 "Sunfish Lake",
                                                 "Independence",
                                                 "Andover",
                                                 "New Market",
                                                 "Mendota Heights",
                                                 "Shoreview",
                                                 "Independence",
                                                 "Mound",
                                                 "White Bear",
                                                 "Mayer",
                                                 "Hilltop",
                                                 "Scandia"))
  )
  
})


