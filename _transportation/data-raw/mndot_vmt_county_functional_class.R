source("R/_load_pkgs.R")
# Adapted from https://github.com/Metropolitan-Council/tspe-quarto/blob/main/R/d_vmt_county.R
# Pulls and processes county VMT at the functional class level
# not currently used for any calculations

# check for needed files
if (file.exists("_transportation/data-raw/mndot/county_functional_class/14_cfc.xlsx") == FALSE) {
  cli::cli_abort(c(
    "Required datasets unavailable",
    "*" = "Download VMT by county and route system Excel tables from MnDOT",
    "*" = "{.url https://www.dot.state.mn.us/roadway/data/data-products.html }"
  ))
}


# load -----
# read county level data and make column names consistent
dat_ls <- list()

dat_ls[["2014"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/county_functional_class/14_cfc.xlsx",
  sheet = 2,
  col_types = c(
    "skip",
    "text",
    "text",
    "text",
    "numeric",
    "numeric",
    "numeric",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip"
  ),
  skip = 1
) %>%
  janitor::clean_names() %>%
  rename_with(~ gsub("x2014_", "", .)) %>%
  rename(functional_class = f_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )



dat_ls[["2016"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/county_functional_class/16_cfc.xlsx",
  sheet = 2,
  col_types = c(
    "text",
    "text",
    "text",
    "numeric",
    "numeric",
    "numeric"
  ),
  skip = 1
) %>%
  janitor::clean_names() %>%
  rename_with(~ gsub("x2016_", "", .)) %>%
  rename(functional_class = f_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )

dat_ls[["2017"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/county_functional_class/17_cfc.xlsx",
  sheet = 2,
  col_types = c(
    "text",
    "text",
    "text",
    "numeric",
    "numeric",
    "numeric"
  ),
  skip = 1
) %>%
  janitor::clean_names() %>%
  rename_with(~ gsub("x2017_", "", .)) %>%
  rename(functional_class = f_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )


dat_ls[["2018"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/county_functional_class/18_cfc.xlsx",
  sheet = 2,
  col_types = c(
    "text",
    "text",
    "text",
    "numeric",
    "numeric",
    "numeric"
  ),
  skip = 1
) %>%
  janitor::clean_names() %>%
  rename_with(~ gsub("x2018_", "", .)) %>%
  rename(functional_class = f_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )

dat_ls[["2019"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/county_functional_class/19_cfc.xlsx",
  sheet = 1,
  skip = 2
) %>%
  janitor::clean_names() %>%
  rename_with(~ gsub("x2019_", "", .)) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )


dat_ls[["2020"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/county_functional_class/20_cfc.xlsx",
  sheet = 1,
  skip = 1
) %>%
  janitor::clean_names()


dat_ls[["2021"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/county_functional_class/21_cfc.xlsx",
  sheet = 2,
  skip = 2
) %>%
  janitor::clean_names()


dat_ls[["2022"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/county_functional_class/22_cfc.xlsx",
  sheet = 2,
  skip = 2
) %>%
  janitor::clean_names()

dat_ls[["2023"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/county_functional_class/23_cfc.xlsx",
  sheet = 2,
  skip = 2
) %>%
  janitor::clean_names()

# compile ------

# bind all datasets together
vmt_county_fc_raw <- data.table::rbindlist(dat_ls,
  fill = TRUE,
  idcol = "year"
) %>%
  # remove extra columns
  select(-percent_sampled, -x100_percent_due_to_rounding) %>%
  # remove any grand total rows
  filter(
    functional_class != "Grand Totals",
    !is.na(functional_class)
  ) %>%
  mutate(
    # remove extra codes from county names
    county = gsub("[0-9][0-9] - ", "", county),
    county = stringr::str_to_title(county)
  ) %>%
  mutate(
    cprg_area = ifelse(county %in% c(
      "Hennepin",
      "Dakota",
      "Carver",
      "Ramsey",
      "Anoka",
      "Scott",
      "Washington",
      "Sherburne",
      "Chisago"
    ), TRUE, FALSE)
  ) %>%
  mutate(
    # fix St. Louis county
    county = case_when(
      county %in% c(
        "Saint Louis",
        "St Louis"
      ) ~ "St. Louis",
      TRUE ~ county
    ),
    functional_class_label = stringr::str_remove_all(
      functional_class,
      "[:digit:] -"
    ) %>%
      str_trim()
  )

# better define route systems
# as labels have changed over time

functional_class_reference <- vmt_county_fc_raw %>%
  select(functional_class, urban_rural, year) %>%
  filter(year == 2023) %>%
  select(functional_class, urban_rural) %>%
  unique() %>%
  tidyr::separate_wider_delim(functional_class,
    delim = "-",
    names = c(
      "functional_class_id",
      "functional_class_desc"
    ),
    too_many = "merge"
  ) %>%
  mutate(functional_class_id = str_pad(
    functional_class_id %>%
      str_trim(),
    side = "left",
    pad = "0",
    width = 2
  )) %>%
  arrange(functional_class_id) %>%
  mutate(moves_road_type = case_when(
    urban_rural == "R" & functional_class_id %in% c("01", "02") ~ "Rural Restricted Access",
    urban_rural == "U" & functional_class_id %in% c("01", "02") ~ "Urban Restricted Access",
    urban_rural == "U" & functional_class_id %in% c("03", "04", "05", "06", "07") ~ "Urban Unrestricted Access",
    urban_rural == "R" & functional_class_id %in% c("03", "04", "05", "06", "07") ~ "Rural Unrestricted Access",
  ))

functional_class_year_all <- vmt_county_fc_raw %>%
  select(functional_class, year, urban_rural) %>%
  unique() %>%
  tidyr::separate_wider_delim(functional_class,
    delim = "-",
    names = c(
      "functional_class_id",
      "functional_class"
    ),
    too_many = "merge",
    too_few = "align_start",
    cols_remove = FALSE
  ) %>%
  mutate(
    functional_class_id = str_pad(
      functional_class_id %>%
        str_trim(),
      side = "left",
      pad = "0",
      width = 2
    ),
    functional_class_id = case_when(
      functional_class_id == "Local" ~ "07",
      functional_class_id == "Major Collector" ~ "05",
      functional_class_id == "Minor Collector" ~ "06",
      functional_class_id == "Minor Arterial" ~ "04",
      functional_class_id == "Principal Arterial" & functional_class == " Interstate" ~ "01",
      functional_class_id == "Principal Arterial" & functional_class == " Other" ~ "03",
      functional_class_id == "Principal Arterial" & functional_class == " Other Freeways and Expressways" ~ "02",
      TRUE ~ functional_class_id
    )
  ) %>%
  left_join(functional_class_reference, by = c(
    "functional_class_id",
    "urban_rural"
  ))

# join back with VMT
vmt_county_raw_functional_class <- vmt_county_fc_raw %>%
  left_join(functional_class_year_all,
    by = c("year", "functional_class", "urban_rural")
  )

# remove from environment
rm(functional_class_reference, functional_class_year_all, dat_ls)

vmt_county_raw_functional_class %>%
  group_by(year, county, moves_road_type) %>%
  summarize(annual_vmt = sum(annual_vmt)) %>%
  filter(county == "Anoka")
vmt_county_raw_functional_class
# summarize by year and county only
# removing the route system distinction
vmt_county_raw_fc_summary <-
  vmt_county_raw_functional_class %>%
  group_by(year, county, cprg_area) %>%
  summarize(
    daily_vmt = sum(daily_vmt),
    annual_vmt = sum(annual_vmt),
    centerline_miles = sum(centerline_miles),
    .groups = "keep"
  )
