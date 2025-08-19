testthat::test_that("state DOT VMT", {
  mndot_vmt_ctu <- readRDS(file.path(here::here(), "_transportation/data/mndot_vmt_ctu.RDS"))

  testthat::expect_equal(nrow(mndot_vmt_ctu), 3575)

  # ensure we have all available years
  testthat::expect_equal(
    unique(mndot_vmt_ctu$vmt_year),
    c(
      "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008",
      "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016",
      "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024"
    )
  )

  # Hennepin has highest annual
  mndot_vmt_ctu %>%
    filter(annual_vmt == max(annual_vmt)) %>%
    magrittr::extract2("ctu_name") %>%
    unique() %>%
    testthat::expect_equal("Minneapolis")

  # expect that these CTUs are included
  testthat::expect_true(
    unique(
      unique(mndot_vmt_ctu$ctu_name) %in%
        unique(c(
          "Afton", "Andover", "Anoka", "Apple Valley", "Arden Hills",
          "Bayport", "Belle Plaine", "Bethel", "Birchwood Village", "Blaine",
          "Bloomington", "Brooklyn Center", "Brooklyn Park", "Burnsville",
          "Carver", "Centerville", "Champlin", "Chanhassen", "Chaska",
          "Circle Pines", "Coates", "Cologne", "Columbia Heights", "Columbus",
          "Coon Rapids", "Corcoran", "Cottage Grove", "Crystal", "Dayton",
          "Deephaven", "Dellwood", "Eagan", "East Bethel", "Eden Prairie",
          "Edina", "Elko New Market", "Excelsior", "Falcon Heights", "Farmington",
          "Forest Lake", "Fridley", "Gem Lake", "Golden Valley", "Grant",
          "Greenfield", "Greenwood", "Ham Lake", "Hamburg", "Hampton",
          "Hanover", "Hastings", "Hilltop", "Hopkins", "Hugo", "Independence",
          "Inver Grove Heights", "Jordan", "Lake Elmo", "Lake Saint Croix Beach",
          "Lakeland Shores", "Lakeland", "Lakeville", "Landfall", "Lauderdale",
          "Lexington", "Lilydale", "Lino Lakes", "Little Canada", "Long Lake",
          "Loretto", "Mahtomedi", "Maple Grove", "Maple Plain", "Maplewood",
          "Marine on Saint Croix", "Mayer", "Medicine Lake", "Medina",
          "Mendota Heights", "Mendota", "Miesville", "Minneapolis", "Minnetonka Beach",
          "Minnetonka", "Minnetrista", "Mound", "Mounds View", "New Brighton",
          "New Germany", "New Hope", "New Prague", "New Trier", "Newport",
          "North Oaks", "North Saint Paul", "Northfield", "Norwood Young America",
          "Nowthen", "Oak Grove", "Oak Park Heights", "Oakdale", "Orono",
          "Osseo", "Pine Springs", "Plymouth", "Prior Lake", "Ramsey",
          "Randolph", "Richfield", "Robbinsdale", "Rockford", "Rogers",
          "Rosemount", "Roseville", "Saint Anthony", "Saint Bonifacius",
          "Saint Francis", "Saint Louis Park", "Saint Marys Point", "Saint Paul Park",
          "Saint Paul", "Savage", "Scandia", "Shakopee", "Shoreview", "Shorewood",
          "South Saint Paul", "Spring Lake Park", "Spring Park", "Stillwater",
          "Sunfish Lake", "Tonka Bay", "Vadnais Heights", "Vermillion",
          "Victoria", "Waconia", "Watertown", "Wayzata", "West Saint Paul",
          "White Bear Lake", "Willernie", "Woodbury", "Woodland", "Credit River",
          "Empire"
        ))
    )
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
      "Blaine",
      "Chanhassen",
      "Hastings",
      "Saint Anthony",
      "Shorewood",
      "Spring Lake Park",
      "White Bear Lake"
    )
  )
})
