testthat::test_that("Compiled and summarized onroad emissions", {
  scc_combine <- readRDS(file.path(here::here(), "_transportation/data/scc_combine.RDS"))
  
  
  scc_combine %>% 
    filter(scc6 == 220100) %>% 
    pull(fuel_type) %>% testthat::expect_equal("Gasoline")
  
  scc_combine %>% 
    filter(scc6 == 223007) %>% 
    pull(fuel_type) %>% testthat::expect_equal("Diesel")
  
  
  scc_combine %>% 
    filter(scc6 == 227000) %>% 
    pull(fuel_type) %>% testthat::expect_equal("Diesel")
  
  
  
  scc_combine %>% 
    filter(scc6 == 226500) %>% 
    pull(fuel_type) %>% testthat::expect_equal("Gasoline")
  
  
  testthat::expect_equal(
    length(unique(scc_combine$scc6)),
    nrow(scc_combine))
  
})
