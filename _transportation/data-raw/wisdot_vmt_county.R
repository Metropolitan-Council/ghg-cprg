source("R/_load_pkgs.R")
# https://wisconsindot.gov/Pages/projects/data-plan/veh-miles/default.aspx
# NOTE: this is a run-once script. It should not be regularly re-run

# NOTE
# you may need to install a Java binary to work with tabulapdf
# I suggest installing the Adoptium distribution: https://adoptium.net/
# then, go to your terminal and run `R CMD javareconf` and check for errors
# install.package("tabulapdf")
library(tabulapdf)
# each of these calls launch a shiny app that has you manually select the table
# areas with your mouse
# wi_vmt23 <- tabulapdf::extract_areas("_transportation/data-raw/wisdot/vmt_by_county/vmt2023-c.pdf",
#                                      output = "csv",
#                                      outdir = "_transportation/data-raw/wisdot/vmt_by_county/"
# )
# wi_vmt22 <- tabulapdf::extract_areas("_transportation/data-raw/wisdot/vmt_by_county/vmt2022-c.pdf",
#                                      output = "csv",
#                                      outdir = "_transportation/data-raw/wisdot/vmt_by_county/"
# )
#
# wi_vmt21 <- tabulapdf::extract_areas("_transportation/data-raw/wisdot/vmt_by_county/vmt2021-c.pdf",
#                                      output = "csv",
#                                      outdir = "_transportation/data-raw/wisdot/vmt_by_county/")
#
# wi_vmt20 <- tabulapdf::extract_areas("_transportation/data-raw/wisdot/vmt_by_county/vmt2020-c.pdf",
#                                      output = "csv",
#                                      outdir = "_transportation/data-raw/wisdot/vmt_by_county/")
#
# wi_vmt19 <- tabulapdf::extract_areas("_transportation/data-raw/wisdot/vmt_by_county/vmt2019-c.pdf",
#                                      output = "csv",
#                                      outdir = "_transportation/data-raw/wisdot/vmt_by_county/")
#
# wi_vmt18 <- tabulapdf::extract_areas("_transportation/data-raw/wisdot/vmt_by_county/vmt2018-c.pdf",
#                                      output = "csv",
#                                      outdir = "_transportation/data-raw/wisdot/vmt_by_county/")
#
# wi_vmt17 <- tabulapdf::extract_areas("_transportation/data-raw/wisdot/vmt_by_county/vmt2017-c.pdf",
#                                      output = "csv",
#                                      outdir = "_transportation/data-raw/wisdot/vmt_by_county/")
#
# wi_vmt16 <- tabulapdf::extract_areas("_transportation/data-raw/wisdot/vmt_by_county/vmt2016-c.pdf",
#                                      output = "csv",
#                                      outdir = "_transportation/data-raw/wisdot/vmt_by_county/")
#
# wi_vmt15 <- tabulapdf::extract_areas("_transportation/data-raw/wisdot/vmt_by_county/vmt2015-c.pdf",
#                                      output = "csv",
#                                      outdir = "_transportation/data-raw/wisdot/vmt_by_county/")
#
# wi_vmt14 <- tabulapdf::extract_areas("_transportation/data-raw/wisdot/vmt_by_county/vmt2014-c.pdf",
#                                      output = "csv",
#                                      outdir = "_transportation/data-raw/wisdot/vmt_by_county/")
#
# wi_vmt13 <- tabulapdf::extract_areas("_transportation/data-raw/wisdot/vmt_by_county/vmt2013-c.pdf",
#                                      output = "csv",
#                                      outdir = "_transportation/data-raw/wisdot/vmt_by_county/")
#
# wi_vmt12 <- tabulapdf::extract_areas("_transportation/data-raw/wisdot/vmt_by_county/vmt2012-c.pdf",
#                                      output = "csv",
#                                      outdir = "_transportation/data-raw/wisdot/vmt_by_county/")
#
# wi_vmt11 <- tabulapdf::extract_areas("_transportation/data-raw/wisdot/vmt_by_county/vmt2011-c.pdf",
#                                      output = "csv",
#                                      outdir = "_transportation/data-raw/wisdot/vmt_by_county/")
#
# wi_vmt10 <- tabulapdf::extract_areas("_transportation/data-raw/wisdot/vmt_by_county/vmt2010-c.pdf",
#                                      output = "csv",
#                                      outdir = "_transportation/data-raw/wisdot/vmt_by_county/")
#
# wi_vmt09 <- tabulapdf::extract_areas("_transportation/data-raw/wisdot/vmt_by_county/vmt2009-c.pdf",
#                                      output = "csv",
#                                      outdir = "_transportation/data-raw/wisdot/vmt_by_county/")



# combine all tables
wisconsin_vmt_raw <- purrr::map_dfr(
  list.files("_transportation/data-raw/wisdot/vmt_by_county/",
    pattern = "*.csv",
    full.names = TRUE
  ),
  function(x) {
    read_csv(x,
      col_names = c("COUNTY", "Daily", "ANNUAL"),
      skip_empty_rows = TRUE,
      n_max = 45,
      col_types = "ccc"
    ) %>%
      mutate(year = x %>%
        stringr::str_remove_all("_transportation/data-raw/wisdot/vmt_by_county//vmt") %>%
        stringr::str_remove_all("-c-2.csv") %>%
        stringr::str_remove_all("-c-1.csv")) %>%
      filter(
        stringr::str_detect(COUNTY, "COUNTY", negate = TRUE),
        stringr::str_detect(COUNTY, "Statewide", negate = TRUE),
        stringr::str_detect(COUNTY, "Notes", negate = TRUE),
        stringr::str_detect(COUNTY, "Source", negate = TRUE),
        stringr::str_detect(COUNTY, "/", negate = TRUE)
      )
  }
)

wisconsin_vmt <- wisconsin_vmt_raw %>%
  filter(!is.na(ANNUAL)) %>%
  bind_rows(
    # the second page of 2009 reads in oddly
    wisconsin_vmt_raw %>%
      filter(is.na(ANNUAL)) %>%
      mutate(
        ANNUAL = Daily,
        Daily = stringr::str_remove_all(COUNTY, "[:alpha:]") %>%
          str_trim(),
        COUNTY = stringr::str_remove_all(COUNTY, "[:digit:]") %>%
          stringr::str_remove_all(",") %>%
          str_trim()
      )
  ) %>%
  mutate(
    Daily = str_remove_all(Daily, "[:punct:]") %>%
      str_remove_all(" ") %>%
      as.numeric(),
    ANNUAL = str_remove_all(ANNUAL, ",") %>% as.numeric()
  ) %>%
  clean_names() %>%
  mutate(
    county = stringr::str_to_title(county),
    county = ifelse(county == "St.croix", "St. Croix", county),
    year = as.character(year)
  ) %>%
  select(year,
    county,
    daily_vmt = daily,
    annual_vmt = annual
  ) %>%
  mutate(
    cprg_area = ifelse(county %in% c(
      "St. Croix",
      "Pierce"
    ), TRUE, FALSE)
  )




wi_vmt_county <- wisconsin_vmt %>%
  filter(cprg_area == TRUE)

# double check that there are the right number of observations
# 2 counties for each year of data
nrow(wi_vmt_county) == (2 * length(unique(wi_vmt_county$year)))

saveRDS(wi_vmt_county, "_transportation/data-raw/wisdot/wisdot_vmt_county.RDS")
