testthat::test_that("state DOT VMT", {
  mndot_vmt_ctu <- readRDS(file.path(here::here(), "_transportation/data/mndot_vmt_ctu.RDS"))

  testthat::expect_equal(nrow(mndot_vmt_ctu), 2589)

  # ensure we have all available years
  testthat::expect_equal(
    unique(mndot_vmt_ctu$vmt_year),
    c(
      "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008",
      "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016",
      "2017", "2018", "2019", "2020", "2021", "2022", "2023"
    )
  )

  # Hennepin has highest annual
  mndot_vmt_ctu %>%
    filter(annual_vmt == max(annual_vmt)) %>%
    magrittr::extract2("ctu_name") %>%
    unique() %>%
    testthat::expect_equal("Minneapolis")

  # expect that these CTUs are not included
  testthat::expect_false(
    unique(unique(mndot_vmt_ctu$ctu_name) %in%
      c(
        "Baytown", "Benton", "Bethel", "Birchwood Village", "Blakeley",
        "Camden", "Castle Rock", "Cedar Lake", "Cologne", "Credit River",
        "Dahlgren", "Denmark", "Douglas", "Elko New Market", "Empire",
        "Eureka", "Fort Snelling", "Greenfield", "Greenvale", "Grey Cloud Island",
        "Hampton", "Hancock", "Hanover", "Helena", "Hilltop", "Hollywood",
        "Independence", "Jackson", "Lake Saint Croix Beach", "Lakeland Shores",
        "Laketown", "Landfall", "Lexington", "Lilydale", "Linwood", "Loretto",
        "Louisville", "Maple Plain", "Marshan", "May", "Mayer", "Miesville",
        "Minnetonka Beach", "New Germany", "New Market", "New Trier",
        "Nininger", "North Oaks", "Norwood Young America", "Nowthen",
        "Pine Springs", "Randolph", "Ravenna", "Saint Lawrence", "Saint Marys Point",
        "San Francisco", "Sand Creek", "Sciota", "Spring Lake", "Spring Park",
        "Sunfish Lake", "Tonka Bay", "Vermillion", "Waterford", "Watertown",
        "West Lakeland", "White Bear", "Woodland", "Young America"
      ))
  )

  testthat::expect_equal(
    mndot_vmt_ctu %>%
      select(geoid, ctuid, ctu_name) %>%
      unique() %>%
      group_by(ctu_name) %>%
      count() %>%
      filter(n >= 2) %>%
      extract2("ctu_name"),
    c(
      # "Blaine",
      "Chanhassen",
      # "Hastings",
      "Saint Anthony",
      # "Shorewood",
      "Spring Lake Park"
      # "White Bear Lake"
    )
  )
})
