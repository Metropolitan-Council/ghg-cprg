source("R/_load_pkgs.R")
source("R/download_read_table.R")
source("_meta/data-raw/ctu_saint_names.R")
source("R/_quarto_helpers.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS") %>% 
  sf::st_drop_geometry()
ctu_population <- readRDS("_meta/data/ctu_population.RDS") %>% 
  left_join(cprg_county)
mndot_vmt_county <- readRDS("_transportation/data-raw/mndot/mndot_vmt_county.RDS")
mndot_route_system <- readRDS("_transportation/data-raw/mndot/mndot_route_system.RDS")

# TODO bring in the percent sampled column, which is present for 2019 onward
# check for needed files
if (file.exists("_transportation/data-raw/mndot/city_route_system/23_ccr.xlsx") == FALSE) {
  cli::cli_abort(c(
    "Required datasets unavailable",
    "*" = "Download VMT by city and route system Excel tables from MnDOT",
    "*" = "{.url https://www.dot.state.mn.us/roadway/data/data-products.html }"
  ))
}

# load -----
# read city level data and make column names consistent
city_ccr <- list()

city_ccr[["2001"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/city_route_system/01_ccr.xls",
  sheet = 2,
  col_types = c(
    "skip",
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
  rename_with(~ gsub("x2001_", "", .)) %>%
  rename_with(~ gsub("x2002_", "", .)) %>%
  rename(route_system = route_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )


city_ccr[["2002"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/city_route_system/02_ccr.xls",
  sheet = 2,
  col_types = c(
    "skip",
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
  rename_with(~ gsub("x2002_", "", .)) %>%
  rename_with(~ gsub("x2003_", "", .)) %>%
  rename(route_system = route_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )


city_ccr[["2003"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/city_route_system/03_ccr.xls",
  sheet = 2,
  col_types = c(
    "skip",
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
  rename_with(~ gsub("x2003_", "", .)) %>%
  rename_with(~ gsub("x2004_", "", .)) %>%
  rename(route_system = route_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )


city_ccr[["2004"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/city_route_system/04_ccr.xls",
  sheet = 2,
  col_types = c(
    "skip",
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
  rename_with(~ gsub("x2004_", "", .)) %>%
  rename_with(~ gsub("x2005_", "", .)) %>%
  rename(route_system = route_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )


city_ccr[["2005"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/city_route_system/05_ccr.xls",
  sheet = 2,
  col_types = c(
    "skip",
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
  rename_with(~ gsub("x2005_", "", .)) %>%
  rename_with(~ gsub("x2006_", "", .)) %>%
  rename(route_system = route_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )


city_ccr[["2006"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/city_route_system/06_ccr.xls",
  sheet = 2,
  col_types = c(
    "skip",
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
  rename_with(~ gsub("x2006_", "", .)) %>%
  rename_with(~ gsub("x2007_", "", .)) %>%
  rename(route_system = route_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )


city_ccr[["2007"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/city_route_system/07_ccr.xls",
  sheet = 2,
  col_types = c(
    "skip",
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
  rename_with(~ gsub("x2007_", "", .)) %>%
  rename_with(~ gsub("x2008_", "", .)) %>%
  rename(route_system = route_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )


city_ccr[["2008"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/city_route_system/08_ccr.xls",
  sheet = 2,
  col_types = c(
    "skip",
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
  rename_with(~ gsub("x2009_", "", .)) %>%
  rename_with(~ gsub("x2008_", "", .)) %>%
  rename(route_system = route_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )

city_ccr[["2009"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/city_route_system/09_ccr.xls",
  sheet = 2,
  col_types = c(
    "skip",
    "text",
    "text",
    "text",
    "numeric",
    "numeric",
    "numeric"
  ),
  skip = 7
) %>%
  janitor::clean_names() %>% 
  rename_with(~ gsub("x2009_", "", .)) %>%
  rename_with(~ gsub("x2010_", "", .)) %>% 
  rename(
    daily_vmt = daily_average_vehicle_m_iles,
    annual_vmt = annual_total_vehicle_miles
  )

city_ccr[["2010"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/city_route_system/10_ccr.xls",
  sheet = 2,
  col_types = c(
    "skip",
    "text",
    "text",
    "text",
    "numeric",
    "numeric",
    "numeric"
  ),
  skip = 7
) %>%
  janitor::clean_names() %>%
  rename_with(~ gsub("x2011_", "", .)) %>%
  rename_with(~ gsub("x2010_", "", .)) %>%
  rename(route_system = route_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )

city_ccr[["2011"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/city_route_system/11_ccr.xlsx",
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
    "skip"
  ),
  skip = 6
) %>%
  janitor::clean_names() %>%
  rename_with(~ gsub("x2012_", "", .)) %>%
  rename_with(~ gsub("x2011_", "", .)) %>%
  rename(route_system = route_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )

city_ccr[["2012"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/city_route_system/12_ccr.xlsx",
  sheet = 2,
  col_types = c(
    "skip",
    "text",
    "text",
    "text",
    "numeric",
    "numeric",
    "numeric"
  ),
  skip = 6
) %>%
  janitor::clean_names() %>%
  rename_with(~ gsub("x2013_", "", .)) %>%
  rename_with(~ gsub("x2012_", "", .)) %>%
  rename(route_system = route_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )

city_ccr[["2013"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/city_route_system/13_ccr.xlsx",
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


city_ccr[["2014"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/city_route_system/14_ccr.xlsx",
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



city_ccr[["2016"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/city_route_system/16_ccr.xlsx",
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
  rename(route_system = route_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )

# 2017-2023 data were corrected, and provided by MnDOT staff directly
# see raw data on OneDrive/MS Teams
city_ccr[["2017"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/updated_VMT_County_City_Route_System/updated_2017_VMT_County_City_Route_System.xlsx",
  sheet = 2,
  skip = 2) %>% 
  clean_names()


city_ccr[["2018"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/updated_VMT_County_City_Route_System/updated_2018_VMT_County_City_Route_System.xlsx",
  sheet = 2,
  skip = 2
) %>%
  janitor::clean_names()

city_ccr[["2019"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/updated_VMT_County_City_Route_System/updated_2019_VMT_County_City_Route_System.xlsx",
  sheet = 2,
  skip = 2
) %>%
  janitor::clean_names()


city_ccr[["2020"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/updated_VMT_County_City_Route_System/updated_2020_VMT_County_City_Route_System.xlsx",
  sheet = 1,
  skip = 2
) %>%
  janitor::clean_names()


city_ccr[["2021"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/updated_VMT_County_City_Route_System/updated_2021_VMT_County_City_Route_System.xlsx",
  sheet = 2,
  skip = 2
) %>%
  janitor::clean_names()


city_ccr[["2022"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/updated_VMT_County_City_Route_System/updated_2022_VMT_County_City_Route_System.xlsx",
  sheet = 2,
  skip = 2
) %>%
  janitor::clean_names()

city_ccr[["2023"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/updated_VMT_County_City_Route_System/updated_2023_VMT_County_City_Route_System.xlsx",
  sheet = 2,
  skip = 2
) %>%
  janitor::clean_names()

# compile ------

# bind all datasets together
vmt_city_raw <- data.table::rbindlist(city_ccr,
                                      fill = TRUE,
                                      idcol = "year"
) %>%
  # remove extra columns
  select(-x100_percent_due_to_rounding,
         -official_statistics_provided_by_the_office_of_transportation_system_management,
         -x2) %>% 
  # remove any grand total rows
  filter(route_system != "Grand Totals") %>% 
  mutate(
    # remove extra codes from city names
    # keep the original column for debugging
    ctu_name = gsub("[0-9][0-9][0-9][0-9]-", "", city) %>% 
      stringr::str_remove_all("[0-9][0-9][0-9][0-9] - ") %>% 
      stringr::str_remove_all("\\(BLANKS\\)-") %>% 
      stringr::str_to_title(),
    # clean county names
    county_name = gsub("[0-9][0-9] - ", "", county) %>% 
      stringr::str_to_title()
  ) %>% 
  # remedy ctu_name differences
  mutate(ctu_name  = case_when(
    ctu_name %in% c(saint_alternates) ~ stringr::str_replace(ctu_name,
                                                             "St ", "Saint "),
    ctu_name == "Mc Grath" ~ "McGrath",
    ctu_name == "Mc Gregor" ~ "McGregor",
    # note that "New Market" is included in VMT data for years 2000-2005
    # "Elko" is included for years 2001-2006
    # "Elko New Market" is included for years 2007-onward
    ctu_name %in% c("Elko-New Market",
                    "Elko",
                    "New Market") ~ "Elko New Market",
    ctu_name %in% c("Marine On St Croix",
                    "Marine On Saint Croix")  ~ "Marine on Saint Croix",
    TRUE ~ ctu_name
  )) %>% 
  mutate(
    cprg_area = ifelse(county_name %in% c(
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
  left_join(mndot_route_system,
            by = "route_system")


# find reliable cities ----- 
# CTUs are reliable if they have
# - a full time series from 2001 to 2023
# - a minimum percent sample across route systems

# find the ctus that have the full time series
# about 157 CTUs
ctu_n_years <- vmt_city_raw %>% 
  filter(cprg_area == TRUE) %>% 
  select(ctu_name, year) %>% 
  unique() %>% 
  group_by(ctu_name) %>% 
  count(name = "n_years") %>% 
  arrange(n_years) %>% 
  ungroup() %>% 
  # filter to only CTUs that have all years of data
  filter(n_years == max(n_years))

# we only have % sampled for years 2017 onward
# A percent sampled of 0 indicates that there has  never been a submitted count for the  given route system in the CTU.
# This indicates that the reported values are default values.
# Default values will not respond to actual changes in VMT, unlike sampled data.
# Though the defaults take into account things like rural/urban and route system,
# they are standardized values that have not been regularly updated 
# (per conversations with MnDOT staff)

# A percent sampled greater than 0 indicates that there has been
# a count/sample for the given route system at some point. 

# There is generally no issue with the trunk highway system,
# but the lower level systems are more likely to have default values
# or to not be sampled at all

# overall mean
mean(vmt_city_raw$percent_sampled, na.rm = T) 


# which route systems have never been sampled in our region?
routes_never_sampled <- vmt_city_raw %>%
  filter(cprg_area == TRUE,
         centerline_miles > 0,
         !is.na(centerline_miles),
         !is.na(percent_sampled)) %>% 
  group_by(route_system_id, route_system_level, route_system_desc) %>% 
  summarize(percent_sampled = round(mean(percent_sampled), 2),
            centerline_miles = sum(centerline_miles),
            centerline_weight = sum(centerline_miles)/n(),
            min_sampled = min(percent_sampled)) %>%
  filter(percent_sampled == 0) %>% 
  extract2("route_system_desc") 


ctu_pct_sampled_route <- vmt_city_raw %>% 
  ungroup() %>% 
  filter(cprg_area == TRUE,
         !is.na(centerline_miles),
         !is.na(percent_sampled),
         # remove routes that were never sampled or don't exist in our region
         # so we don't remove a CTU based on these never-sampled systems
         !route_system_desc %in% routes_never_sampled) %>% 
  # group by ctu and route system level
  group_by(ctu_name, route_system_level) %>% 
  summarize(percent_sampled = round(mean(percent_sampled), 2),
            centerline_miles = sum(centerline_miles),
            centerline_weight = sum(centerline_miles)/n(),
            min_sampled = min(percent_sampled),
            systems = paste0(unique(route_system_desc), collapse = ", ")) %>% 
  # group by CTU only to get the CTU average 
  group_by(ctu_name) %>% 
  mutate(ctu_mean = weighted.mean(percent_sampled, centerline_miles))

# find CTUs where local systems have a 
# minimum sample on local systems greater than 0
# indicating that there is sampled data on their local systems
# at some point in time 
ctu_sampled <- ctu_pct_sampled_route %>% 
  ungroup() %>% 
  filter(route_system_level == "Local systems",
         min_sampled > 0) %>% 
  unique()

# now, filter to get only the CTUs that are reliable
reliable_ctu <- 
  vmt_city_raw %>% 
  filter(cprg_area == TRUE) %>% 
  select(ctu_name) %>% 
  unique() %>% 
  # ensure we have a full time series
  filter(ctu_name %in% ctu_n_years$ctu_name) %>% 
  # ensure we have CTUs with sampled local routes
  filter(ctu_name %in% ctu_sampled$ctu_name)

# summarize ------
# remove the route system distinction to get County/CTU level summary
vmt_city_raw_summary <-
  vmt_city_raw %>%
  # filter to only reliable CTUs
  filter(cprg_area == TRUE,
         ctu_name %in% reliable_ctu$ctu_name) %>% 
  group_by(year, county_name, ctu_name, cprg_area) %>%
  summarize(
    daily_vmt = sum(daily_vmt, na.rm = TRUE),
    annual_vmt = sum(annual_vmt, na.rm = TRUE),
    centerline_miles = sum(centerline_miles, na.rm = TRUE),
    .groups = "keep"
  ) %>% 
  ungroup() %>% 
  filter(county_name %in% cprg_county$county_name)

# get city only summary
vmt_city_alone <- vmt_city_raw %>% 
  # filter to only reliable CTUs
  filter(cprg_area == TRUE,
         ctu_name %in% reliable_ctu$ctu_name) %>% 
  group_by(year, ctu_name) %>% 
  summarize(
    daily_vmt = sum(daily_vmt, na.rm = TRUE),
    annual_vmt = sum(annual_vmt, na.rm = TRUE),
    centerline_miles = sum(centerline_miles, na.rm = TRUE),
    .groups = "keep"
  ) 

# get county only summary
vmt_county_alone <- vmt_city_raw %>% 
  filter(cprg_area == TRUE) %>% 
  group_by(county_name, year) %>% 
  summarize(
    daily_vmt = sum(daily_vmt, na.rm = TRUE),
    annual_vmt = sum(annual_vmt, na.rm = TRUE),
    centerline_miles = sum(centerline_miles, na.rm = TRUE),
    .groups = "keep"
  ) 

# join with county level data and check for differences
vmt_county_alone %>% 
  left_join(mndot_vmt_county,
            by = c("year", "county_name" = "county"),
            suffix = c(".city", ".county")) %>% 
  mutate(daily_diff = round(daily_vmt.city - daily_vmt.county),
         annual_diff = round(annual_vmt.city - annual_vmt.county),
         centerline_diff = round(centerline_miles.city - centerline_miles.county))


# merge with ctu population 
vmt_ctu_pop <- ctu_population %>%
  select(county_name, ctu_name, inventory_year, ctu_population) %>%
  unique() %>%
  filter(inventory_year %in% vmt_city_raw_summary$year) %>%
  full_join(
    vmt_city_alone %>%
      ungroup() %>%
      filter(
        ctu_name %in% ctu_population$ctu_name) %>%
      select(ctu_name, year, centerline_miles,
             annual_vmt, daily_vmt) %>%
      mutate(year = as.numeric(year)) %>%
      filter(year %in% unique(ctu_population$inventory_year),
             !ctu_name %in% c("Nonmunicpal",
                              "Nonmunicipal")) %>%
      unique(),
    by = c("ctu_name",
           "inventory_year" = "year")
  )

# basic plotting -----
vmt_ctu_pop %>% 
  plot_ly(
    type = "scatter",
    mode = "markers",
    x = ~ctu_population,
    y = ~annual_vmt,
    color = ~inventory_year,
    hoverinfo = "text",
    hovertext = ~paste0(
      inventory_year, "<br>",
      ctu_name, ", ", county_name, " County",  "<br>",
      "Pop: ", scales::comma(ctu_population), "<br>",
      "Annual VMT: ", scales::comma(annual_vmt/1000000, accuracy = 1), "M"
    )
  )

vmt_ctu_pop %>% 
  # get higher population CTUs
  filter(ctu_name %in% c(vmt_ctu_pop %>% 
                           filter(ctu_population >= 65000) %>% 
                           select(ctu_name) %>% 
                           unique() %>% 
                           extract2("ctu_name"))) %>% 
  plot_ly(
    type = "scatter",
    mode = "lines+markers",
    x = ~inventory_year,
    y = ~annual_vmt,
    color = ~ctu_name,
    hoverinfo = "text",
    hovertext = ~paste0(
      inventory_year, "<br>",
      ctu_name, ", ", county_name, " County",  "<br>",
      "Pop: ", scales::comma(ctu_population), "<br>",
      "Annual VMT: ", scales::comma(annual_vmt/1000000, accuracy = 1), "M"
    )
  )

# centerline miles
vmt_ctu_pop %>% 
  # get higher population CTUs
  filter(ctu_name %in% c(vmt_ctu_pop %>% 
                           filter(ctu_population >= 65000) %>% 
                           select(ctu_name) %>% 
                           unique() %>% 
                           extract2("ctu_name"))) %>% 
  plot_ly(
    type = "scatter",
    mode = "lines+markers",
    x = ~inventory_year,
    y = ~centerline_miles,
    color = ~ctu_name,
    hoverinfo = "text",
    hovertext = ~paste0(
      inventory_year, "<br>",
      ctu_name, ", ", county_name, " County",  "<br>",
      "Pop: ", scales::comma(ctu_population), "<br>",
      "Annual VMT: ", scales::comma(annual_vmt/1000000, accuracy = 1), "M"
    )
  )

vmt_city_raw %>% 
  filter(county_name %in% mndot_vmt_county$county) %>% 
  group_by(county_name, year) %>% 
  summarize(
    city_daily_vmt = sum(daily_vmt, na.rm = TRUE),
    city_annual_vmt = sum(annual_vmt, na.rm = TRUE),
    city_centerline_miles = sum(centerline_miles, na.rm = TRUE),
    .groups = "keep"
  ) %>% 
  left_join(
    mndot_vmt_county,
    by = c("county_name" = "county",
           "year")) %>% 
  mutate(
    diff_daily_vmt = round(city_daily_vmt -  daily_vmt),
    diff_annual_vmt = round(city_annual_vmt - annual_vmt),
    diff_centerline_miles = round(city_centerline_miles - centerline_miles)
  )

# interpolate 2015 data -----
vmt_interp <- vmt_city_alone %>%
  # first create an NA 2015 dataset
  ungroup() %>%
  select(ctu_name) %>%
  unique() %>%
  mutate(
    year = "2015",
    daily_vmt = NA,
    annual_vmt = NA
  ) %>%
  # bind with original
  bind_rows(vmt_city_alone) %>%
  arrange(year) %>%
  group_by(ctu_name) %>%
  # interpolate using midpoint method
  # for missing values
  # grouped by county
  mutate(
    annual_approx = zoo::na.approx(annual_vmt),
    daily_approx = zoo::na.approx(daily_vmt),
    centerline_approx = zoo::na.approx(centerline_miles)
  )

# review and check that values make sense for all ctus
# re-assign column values to match original data
vmt_ctu <- vmt_interp %>%
  mutate(
    daily_vmt = daily_approx,
    annual_vmt = annual_approx,
    centerline_miles = centerline_approx
  ) %>%
  select(-daily_approx, -annual_approx, -centerline_approx) %>% 
  ungroup()

saveRDS(vmt_ctu, "_transportation/data-raw/mndot/mndot_vmt_ctu.RDS")
