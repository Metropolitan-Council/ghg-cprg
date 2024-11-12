testthat::test_that("state DOT VMT", {
  dot_vmt <- readRDS(file.path(here::here(), "_transportation/data/dot_vmt.RDS"))

  testthat::expect_equal(nrow(dot_vmt), 235)

  # check match with PDFs
  dot_vmt %>%
    filter(
      county_name == "Pierce",
      vmt_year >= 2018
    ) %>%
    arrange(desc(vmt_year)) %>%
    magrittr::extract2("daily_vmt") %>%
    testthat::expect_equal(c(1067675, 1074726, 949687, 1099233, 1068300))

  # hennepin has highest annual
  dot_vmt %>%
    filter(annual_vmt == max(annual_vmt)) %>%
    magrittr::extract2("county_name") %>%
    testthat::expect_equal("Hennepin")
})


testthat::test_that("VMT have decreased since including only major categories", {
  # skip this test if offline
  testthat::skip_if_offline()
  
  # download specific version of epa_onroad_emissions_compile from GitHub
  # Commit ID 17b4349fc5874562befaa56a85ef3f740bb1708a
  file_name <- tempfile()
  download.file("https://github.com/Metropolitan-Council/ghg-cprg/raw/17b4349fc5874562befaa56a85ef3f740bb1708a/_transportation/data/dot_vmt.RDS",
                destfile = file_name, quiet = TRUE, mode = "wb"
  )
  
  
  prev_dot_vmt <- readRDS(file_name)
  dot_vmt <- readRDS(file.path(here::here(), "_transportation/data/dot_vmt.RDS"))
  
  left_join(prev_dot_vmt, dot_vmt,
            by = c("vmt_year", "geoid", "county_name", "cprg_area", "data_source")) %>% View
  
  waldo::compare(prev_dot_vmt, dot_vmt)
  
  
})
