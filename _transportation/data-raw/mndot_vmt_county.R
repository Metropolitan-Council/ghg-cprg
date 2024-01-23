source("R/_load_pkgs.R")
# Adapted from https://github.com/Metropolitan-Council/tspe-quarto/blob/main/R/d_vmt_county.R

# check for needed files
if (file.exists("_transportation/data-raw/mndot/county_route_system/18_crs.xlsx") == FALSE) {
  cli::cli_abort(c(
    "Required datasets unavailable",
    "*" = "Download VMT by county and route system Excel tables from MnDOT",
    "*" = "{.url https://www.dot.state.mn.us/roadway/data/data-products.html }"
  ))
}


# load -----
# read county level data and make column names consistent
dat_ls <- list()

dat_ls[["2018"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/county_route_system//18_crs.xlsx",
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

dat_ls[["2019"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/county_route_system//19_crs.xlsx",
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


dat_ls[["2020"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/county_route_system//20_crs.xlsx",
  sheet = 1,
  skip = 1
) %>%
  janitor::clean_names()


dat_ls[["2021"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/county_route_system//21_crs.xlsx",
  sheet = 2,
  skip = 2
) %>%
  janitor::clean_names()


# compile ------

# bind all datasets together
vmt_county_raw <- rbindlist(dat_ls, fill = T, idcol = "year") %>%
  # remove extra columns
  select(-percent_sampled, -x100_percent_due_to_rounding) %>%
  # remove any grand total rows
  filter(route_system != "Grand Totals") %>%
  mutate(
    # remove extra codes from county names
    county = gsub("[0-9][0-9] - ", "", county),
    county = stringr::str_to_title(county)
  )

vmt_county <- vmt_county_raw %>%
  # filter to only the 7-county metro
  filter(county %in% c(
    "Hennepin",
    "Dakota",
    "Carver",
    "Ramsey",
    "Anoka",
    "Scott",
    "Washington",
    "Sherburne",
    "Chisago"
  )) %>%
  group_by(year, county) %>%
  # calculate daily, annual vmt and centerline miles
  # grouped by year
  dplyr::summarize(
    daily_vmt = sum(daily_vmt),
    annual_vmt = sum(annual_vmt),
    centerline_miles = sum(centerline_miles),
    .groups = "keep"
  )

saveRDS(vmt_county, "_transportation/data-raw/mndot/mndot_vmt_county.RDS")
