source("R/_load_pkgs.R")

# NOTE
# you may need to install a Java binary to work with tabulizer
# I suggest installing the Adoptium distribution: https://adoptium.net/
# then, go to your terminal and run `R CMD javareconf` and check for errors

library(tabulizer)
# each of these calls launch a shiny app that has you manually select the table
# areas with your mouse
wi_vmt22 <- tabulizer::extract_areas("_transportation/data-raw/wisdot/vmt_by_county/vmt2022-c.pdf",
  output = "data.frame"
)

wi_vmt21 <- tabulizer::extract_areas("_transportation/data-raw/wisdot/vmt_by_county/vmt2021-c.pdf",
  output = "data.frame"
)

wi_vmt20 <- tabulizer::extract_areas("_transportation/data-raw/wisdot/vmt_by_county/vmt2020-c.pdf",
  output = "data.frame"
)

wi_vmt19 <- tabulizer::extract_areas("_transportation/data-raw/wisdot/vmt_by_county/vmt2019-c.pdf",
  output = "data.frame"
)

wi_vmt18 <- tabulizer::extract_areas("_transportation/data-raw/wisdot/vmt_by_county/vmt2018-c.pdf",
  output = "data.frame"
)





# the second page 2019 table does not have column headers
# and read in a row as the column names
# take the column names and add it to the table as a row
wi_vmt19[[2]][32, ] <- names(wi_vmt19[[2]])
wi_vmt18[[2]][32, ] <- names(wi_vmt18[[2]])


# change column names of second table to be same as first
names(wi_vmt19[[2]]) <- names(wi_vmt19[[1]])
names(wi_vmt18[[2]]) <- names(wi_vmt18[[1]])


# combine all tables

wi_vmt_county <- bind_rows(wi_vmt22) %>%
  clean_names() %>%
  mutate(
    year = 2022
  ) %>%
  bind_rows(
    bind_rows(wi_vmt21) %>%
      clean_names() %>%
      mutate(year = 2021),
    bind_rows(wi_vmt20) %>%
      clean_names() %>%
      mutate(year = 2020),
    bind_rows(wi_vmt19) %>%
      clean_names() %>%
      # clean data to remove extra characters
      mutate(
        year = 2019,
        county = stringr::str_remove(county, "[:digit:]"),
        annual = stringr::str_remove(annual, "X") %>%
          stringr::str_remove_all("\\."),
        daily = stringr::str_remove(daily, "X") %>%
          stringr::str_remove_all("\\.")
      ),
    bind_rows(wi_vmt18) %>%
      clean_names() %>%
      # clean data to remove extra characters
      mutate(
        year = 2018,
        county = stringr::str_remove(county, "[:digit:]"),
        annual = stringr::str_remove(annual, "X") %>%
          stringr::str_remove_all("\\."),
        daily = stringr::str_remove(daily, "X") %>%
          stringr::str_remove_all("\\.")
      )
  ) %>%
  mutate(
    daily = stringr::str_remove_all(daily, ",") %>% as.numeric(),
    annual = stringr::str_remove_all(annual, ",") %>% as.numeric(),
    county = stringr::str_to_title(county)
  ) %>%
  select(year,
    county,
    daily_vmt = daily,
    annual_vmt = annual
  ) %>%
  filter(county %in% c("Pierce", "St.croix")) %>%
  # fix St. Croix name
  mutate(
    county = ifelse(county == "St.croix", "St. Croix", county),
    year = as.character(year)
  )


saveRDS(wi_vmt_county, "_transportation/data-raw/wisdot/wisdot_vmt_county.RDS")
