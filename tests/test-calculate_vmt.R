testthat::test_that("Correct number of calibration zone sets", {
  source(file.path(here::here(), "_transportation/data-raw/_calculate_vmt.R"))

  county21_data <- readRDS(file.path(here::here(), "_transportation/data-raw/analysis_runs/county21_data.RDS"))
  county21_truck <- readRDS(file.path(here::here(), "_transportation/data-raw/analysis_runs/county21_truck_calib_data.RDS"))
  

  
  passenger <- calculate_vmt(county21_data, class = "passenger")  
  commercial <- calculate_vmt(county21_truck, class = "commercial")

  testthat::expect_equal(unique(names(c(passenger, commercial))),
                         c("analysis_name", "mode_of_travel", "zone", "vehicle_weight", 
                           "vmt_same", "vmt_origin", "vmt_destination", "vmt_total"))
  
  testthat::expect_equal(unique(c(commercial$zone, passenger$zone)),
                         c("Anoka", "Carver", "Chisago", "Dakota", "Hennepin", "Pierce", 
                           "Ramsey", "Scott", "Sherburne", "St. Croix", "Washington")  
  )
  
  testthat::expect_equal(unique(c(passenger$vehicle_weight, commercial$vehicle_weight)),
                         c("Passenger", "Medium", "Heavy"),
  )
  
  
  # expect heavy duty out-boundary trips to be 0
  commercial %>% 
    filter(vehicle_weight == "Heavy") %>% 
    summarize(vmt_out_boundary = sum(vmt_origin, vmt_destination)) %>% 
    magrittr::extract2("vmt_out_boundary") %>% 
    testthat::expect_equal(0)

  # expect heavy duty in-boundary trips to be 0
  commercial %>% 
    filter(vehicle_weight == "Heavy") %>% 
    summarize(vmt_same = sum(vmt_same)) %>% 
    magrittr::extract2("vmt_same") %>% 
    testthat::expect_gt(0)
  
  # expect Hennepin county to have the highest VMT
  passenger %>% 
    filter(vmt_total == max(vmt_total)) %>% 
    magrittr::extract2("zone") %>% 
    testthat::expect_equal("Hennepin")
  
  commercial %>% 
    filter(vmt_total == max(vmt_total)) %>% 
    magrittr::extract2("zone") %>% 
    testthat::expect_equal("Hennepin")
  
  
  
  # expect Pierce county to have the *lowest* VMT
  passenger %>% 
    filter(vmt_total == min(vmt_total)) %>% 
    magrittr::extract2("zone") %>% 
    testthat::expect_equal("Pierce")
  
  commercial %>% 
    filter(vmt_total == min(vmt_total)) %>% 
    magrittr::extract2("zone") %>% 
    testthat::expect_equal("Pierce")
})
