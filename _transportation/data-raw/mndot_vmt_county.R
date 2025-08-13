source("R/_load_pkgs.R")
# Adapted from https://github.com/Metropolitan-Council/tspe-quarto/blob/main/R/d_vmt_county.R

mndot_route_system <- read_rds("_transportation/data-raw/mndot/mndot_route_system.RDS")
# check for needed files
if (file.exists("_transportation/data-raw/mndot/county_route_system/2024_VMT_County_Route_System-38921397-v1.XLSX") == FALSE) {
  cli::cli_abort(c(
    "Required datasets unavailable",
    "*" = "Download VMT by county and route system Excel tables from MnDOT",
    "*" = "{.url https://www.dot.state.mn.us/roadway/data/data-products.html }"
  ))
}


# load -----
# read county level data and make column names consistent
county_crs <- list()

county_crs[["2001"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/county_route_system/2001_VMT_County_Route_System-38670207-v1.XLS",
  sheet = 2,
  col_types = c(
    "skip",
    "text",
    "text",
    "numeric",
    "numeric",
    "numeric"
  ),
  skip = 1
) %>%
  janitor::clean_names() %>%
  rename_with(~ gsub("x2001_", "", .)) %>%
  rename_with(~ gsub("x2002_", "", .)) %>%
  rename(route_system = route_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )


county_crs[["2002"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/county_route_system/2002_VMT_County_Route_System-38670208-v1.XLS",
  sheet = 2,
  col_types = c(
    "skip",
    "text",
    "text",
    "numeric",
    "numeric",
    "numeric"
  ),
  skip = 1
) %>%
  janitor::clean_names() %>%
  rename_with(~ gsub("x2002_", "", .)) %>%
  rename_with(~ gsub("x2003_", "", .)) %>%
  rename(route_system = route_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )


county_crs[["2003"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/county_route_system/2003_VMT_County_Route_System-38670197-v1.XLS",
  sheet = 2,
  col_types = c(
    "skip",
    "text",
    "text",
    "numeric",
    "numeric",
    "numeric"
  ),
  skip = 1
) %>%
  janitor::clean_names() %>%
  rename_with(~ gsub("x2003_", "", .)) %>%
  rename_with(~ gsub("x2004_", "", .)) %>%
  rename(route_system = route_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )


county_crs[["2004"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/county_route_system/2004_VMT_County_Route_System-38670198-v1.XLS",
  sheet = 2,
  col_types = c(
    "skip",
    "text",
    "text",
    "numeric",
    "numeric",
    "numeric"
  ),
  skip = 1
) %>%
  janitor::clean_names() %>%
  rename_with(~ gsub("x2004_", "", .)) %>%
  rename_with(~ gsub("x2005_", "", .)) %>%
  rename(route_system = route_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )


county_crs[["2005"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/county_route_system/2005_VMT_County_Route_System-38670200-v1.XLS",
  sheet = 2,
  col_types = c(
    "skip",
    "text",
    "text",
    "numeric",
    "numeric",
    "numeric"
  ),
  skip = 1
) %>%
  janitor::clean_names() %>%
  rename_with(~ gsub("x2005_", "", .)) %>%
  rename_with(~ gsub("x2006_", "", .)) %>%
  rename(route_system = route_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )


county_crs[["2006"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/county_route_system/2006_VMT_County_Route_System-38670201-v1.XLS",
  sheet = 2,
  col_types = c(
    "skip",
    "text",
    "text",
    "numeric",
    "numeric",
    "numeric"
  ),
  skip = 1
) %>%
  janitor::clean_names() %>%
  rename_with(~ gsub("x2006_", "", .)) %>%
  rename_with(~ gsub("x2007_", "", .)) %>%
  rename(route_system = route_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )


county_crs[["2007"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/county_route_system/2007_VMT_County_Route_System-38670202-v1.XLS",
  sheet = 2,
  col_types = c(
    "skip",
    "text",
    "text",
    "numeric",
    "numeric",
    "numeric"
  ),
  skip = 1
) %>%
  janitor::clean_names() %>%
  rename_with(~ gsub("x2007_", "", .)) %>%
  rename_with(~ gsub("x2008_", "", .)) %>%
  rename(route_system = route_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )


county_crs[["2008"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/county_route_system/2008_VMT_County_Route_System-38670203-v1.XLS",
  sheet = 2,
  col_types = c(
    "skip",
    "text",
    "text",
    "numeric",
    "numeric",
    "numeric"
  ),
  skip = 2
) %>%
  janitor::clean_names() %>%
  rename_with(~ gsub("x2009_", "", .)) %>%
  rename_with(~ gsub("x2008_", "", .)) %>%
  rename(route_system = route_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )

county_crs[["2009"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/county_route_system/2009_VMT_County_Route_System-38670193-v1.XLS",
  sheet = 2,
  col_types = c(
    "skip",
    "text",
    "text",
    "numeric",
    "numeric",
    "numeric"
  ),
  skip = 2
) %>%
  janitor::clean_names() %>%
  rename_with(~ gsub("x2009_", "", .)) %>%
  rename_with(~ gsub("x2010_", "", .)) %>%
  rename(route_system = route_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )

county_crs[["2010"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/county_route_system/2010_VMT_County_Route_System-38670194-v1.XLS",
  sheet = 2,
  col_types = c(
    "skip",
    "text",
    "text",
    "numeric",
    "numeric",
    "numeric"
  ),
  skip = 2
) %>%
  janitor::clean_names() %>%
  rename_with(~ gsub("x2011_", "", .)) %>%
  rename_with(~ gsub("x2010_", "", .)) %>%
  rename(route_system = route_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )

county_crs[["2011"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/county_route_system/2011_VMT_County_Route_System-38670213-v1.XLSX",
  sheet = 2,
  col_types = c(
    "skip",
    "text",
    "text",
    "numeric",
    "numeric",
    "numeric"
  ),
  skip = 2
) %>%
  janitor::clean_names() %>%
  rename_with(~ gsub("x2012_", "", .)) %>%
  rename_with(~ gsub("x2011_", "", .)) %>%
  rename(route_system = route_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )

county_crs[["2012"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/county_route_system/2012_VMT_County_Route_System-38670214-v1.XLSX",
  sheet = 1,
  col_types = c(
    "skip",
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
  skip = 2
) %>%
  janitor::clean_names() %>%
  rename_with(~ gsub("x2013_", "", .)) %>%
  rename_with(~ gsub("x2012_", "", .)) %>%
  rename(route_system = route_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )

county_crs[["2013"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/county_route_system/2013_VMT_County_Route_System-38670215-v1.XLSX",
  sheet = 2,
  col_types = c(
    "skip",
    "text",
    "text",
    "numeric",
    "numeric",
    "numeric",
    "skip",
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
  rename_with(~ gsub("x2013_", "", .)) %>%
  rename(route_system = route_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )


county_crs[["2014"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/county_route_system/2014_VMT_County_Route_System-38670216-v1.XLSX",
  sheet = 2,
  col_types = c(
    "skip",
    "text",
    "text",
    "numeric",
    "numeric",
    "numeric",
    "skip",
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
  rename(route_system = route_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )



county_crs[["2016"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/county_route_system/2016_VMT_County_Route_System-38670218-v1.XLSX",
  sheet = 2,
  col_types = c(
    "text",
    "text",
    "numeric",
    "numeric",
    "numeric",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip"
  ),
  skip = 1
) %>%
  janitor::clean_names() %>%
  rename_with(~ gsub("x2016_", "", .)) %>%
  rename(route_system = route_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )

county_crs[["2017"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/county_route_system/17_crs.xlsx",
  sheet = 2,
  col_types = c(
    "text",
    "text",
    "numeric",
    "numeric",
    "numeric",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip"
  ),
  skip = 1
) %>%
  janitor::clean_names() %>%
  rename_with(~ gsub("x2017_", "", .)) %>%
  rename(route_system = route_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )


county_crs[["2018"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/county_route_system/2018_VMT_County_Route_System-38670220-v1.XLSX",
  sheet = 2,
  col_types = c(
    "text",
    "text",
    "numeric",
    "numeric",
    "numeric",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip"
  ),
  skip = 1
) %>%
  janitor::clean_names() %>%
  rename_with(~ gsub("x2018_", "", .)) %>%
  rename(route_system = route_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )

county_crs[["2019"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/county_route_system/2019_VMT_County_Route_System-38670222-v1.XLSX",
  sheet = 1,
  skip = 2
) %>%
  janitor::clean_names() %>%
  rename_with(~ gsub("x2019_", "", .)) %>%
  rename(route_system = route_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )


county_crs[["2020"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/county_route_system/2020_VMT_County_Route_System-38670223-v1.XLSX",
  sheet = 1,
  skip = 1
) %>%
  janitor::clean_names()


county_crs[["2021"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/county_route_system/2021_VMT_County_Route_System-38670224-v1.XLSX",
  sheet = 2,
  skip = 2
) %>%
  janitor::clean_names()


county_crs[["2022"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/county_route_system/2022_VMT_County_Route_System-38670211-v1.XLSX",
  sheet = 2,
  skip = 2
) %>%
  janitor::clean_names()

county_crs[["2023"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/county_route_system/2023_VMT_County_Route_System-38670212-v1.XLSX",
  sheet = 2,
  skip = 2
) %>%
  janitor::clean_names()

# !IMPORTANT note that 2024 download had a misaligned row, which
# threw off the first column (county name)
# this was fixed manually
county_crs[["2024"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/county_route_system/2024_VMT_County_Route_System-38921397-v1.XLSX",
  sheet = 2,
  skip = 1
) %>%
  janitor::clean_names()

# compile ------

# bind all datasets together
vmt_county_raw <- data.table::rbindlist(county_crs,
  fill = TRUE,
  idcol = "year"
) %>%
  # remove extra columns
  select(-x100_percent_due_to_rounding) %>%
  # remove any grand total rows
  filter(route_system != "Grand Totals") %>%
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
    )
  )

vmt_county_raw_route_system <- vmt_county_raw %>%
  left_join(mndot_route_system,
    by = "route_system"
  )

# remove from environment
# rm(county_crs)

# summarize by year and county only
# removing the route system distinction
vmt_county_raw_summary <-
  vmt_county_raw_route_system %>%
  group_by(year, county, cprg_area) %>%
  summarize(
    daily_vmt = sum(daily_vmt),
    annual_vmt = sum(annual_vmt),
    centerline_miles = sum(centerline_miles),
    .groups = "keep"
  )

# attempt interpolation of 2015 data

vmt_interp <- vmt_county_raw_summary %>%
  # first create an NA 2015 dataset
  ungroup() %>%
  select(county, cprg_area) %>%
  unique() %>%
  mutate(
    year = "2015",
    daily_vmt = NA,
    annual_vmt = NA
  ) %>%
  # bind with original
  bind_rows(vmt_county_raw_summary) %>%
  arrange(year) %>%
  group_by(county) %>%
  # interpolate using midpoint method
  # for missing values
  # grouped by county
  mutate(
    annual_approx = zoo::na.approx(annual_vmt),
    daily_approx = zoo::na.approx(daily_vmt),
    centerline_approx = zoo::na.approx(centerline_miles)
  )

# review and check that values make sense for all counties

# re-assign column values to match original data
vmt_county_raw_interp <- vmt_interp %>%
  mutate(
    daily_vmt = daily_approx,
    annual_vmt = annual_approx,
    centerline_miles = centerline_approx
  ) %>%
  select(-daily_approx, -annual_approx, -centerline_approx)


# save county data for our CPRG counties only -----
vmt_county <- vmt_county_raw_interp %>%
  # filter to only the 7-county metro
  filter(cprg_area == TRUE) %>%
  group_by(year, county, cprg_area) %>%
  # calculate daily, annual vmt and centerline miles
  # grouped by year
  dplyr::summarize(
    daily_vmt = sum(daily_vmt),
    annual_vmt = sum(annual_vmt),
    centerline_miles = sum(centerline_miles),
    .groups = "keep"
  ) %>%
  ungroup()


saveRDS(vmt_county, "_transportation/data-raw/mndot/mndot_vmt_county.RDS")

write.csv(vmt_county, "_transportation/data-raw/mndot/vmt_county.CSV",
          row.names = FALSE)
