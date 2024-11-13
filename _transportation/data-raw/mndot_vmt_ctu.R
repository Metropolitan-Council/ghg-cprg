source("R/_load_pkgs.R")
source("R/download_read_table.R")
source("_meta/data-raw/ctu_saint_names.R")

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

ctu_nov <- c("Coon Rapids",
             "Carver",
             "Eagan",
             "Bloomington",
             "Saint Paul",
             "Mahtomedi",
             "Minneapolis",
             "Savage")

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

city_ccr[["2017"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/city_route_system/17_ccr.xlsx",
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
  rename(route_system = route_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )


city_ccr[["2018"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/city_route_system/18_ccr.xlsx",
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
  rename(route_system = route_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles
  )

city_ccr[["2019"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/city_route_system/19_ccr.xlsx",
  sheet = 1,
  skip = 2
) %>%
  janitor::clean_names() %>%
  rename_with(~ gsub("x2019_", "", .)) %>%
  rename(route_system = route_system) %>%
  rename(
    daily_vmt = daily_average_vehicle_miles,
    annual_vmt = annual_total_vehicle_miles,
  )


city_ccr[["2020"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/city_route_system/20_ccr.xlsx",
  sheet = 1,
  skip = 2
) %>%
  janitor::clean_names()


city_ccr[["2021"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/city_route_system/21_ccr.xlsx",
  sheet = 2,
  skip = 2
) %>%
  janitor::clean_names()


city_ccr[["2022"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/city_route_system/22_ccr.xlsx",
  sheet = 2,
  skip = 2
) %>%
  janitor::clean_names()

city_ccr[["2023"]] <- readxl::read_excel(
  "_transportation/data-raw/mndot/city_route_system/23_ccr.xlsx",
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
  select(-x100_percent_due_to_rounding) %>% 
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
  # create correct county names for each CTU
  # mutate(
  #   correct_county_name =
  #     case_when(
  #       ctu_name %in% c("Columbia Heights",
  #                       "Columbus",
  #                       "Lino Lakes",
  #                       "Fridley",
  #                       "Nowthen",
  #                       "Ramsey",
  #                       "Saint Francis"
  #       ) ~ "Anoka",
  #       ctu_name %in% c("Victoria"
  #       ) ~ "Carver",
  #       ctu_name %in% c("Mendota Heights",
  #                       "South Saint Paul",
  #                       "Burnsville",
  #                       "Lakeville",
  #                       "South Saint Paul"
  #       ) ~ "Dakota",
  #       ctu_name %in% c(
  #         "Saint Paul",
  #         "Maplewood",
  #         "Mounds View",
  #         "New Brighton",
  #         "North Oaks",
  #         "North Saint Paul",
  #         "Roseville"
  #       ) ~ "Ramsey",
  #       ctu_name %in% c(
  #         "Minneapolis",
  #         "Bloomington",
  #         "Champlin",
  #         "Eden Prairie",
  #         "Independence",
  #         "Minnetonka",
  #         "Minnetrista"
  #       ) ~ "Hennepin",
  #       ctu_name %in% c("Savage",
  #                       "Credit River"
  #       ) ~ "Scott",
  #       ctu_name %in% c("Forest Lake",
  #                       "Hugo",
  #                       "Birchwood Village",
  #                       "Dellwood",
  #                       "Grant",
  #                       "Mahtomedi",
  #                       "Newport",
  #                       "Oakdale",
  #                       "Scandia",
  #                       "Woodbury"
  #       ) ~ "Washington",
  #       ctu_name %in% c("Otsego") ~ "Wright",
  #       ctu_name %in% c("Wyoming") ~ "Chisago",
  #       ctu_name == "Saint Anthony" & county_name == "Anoka" ~ "Hennepin",
  #       # expected splits
  #       # ctu_name %in% c("Blaine",
  #       #                 "Chanhassen",
  #       #                 "Hastings",
  #       #                 "Saint Anthony",
  #       #                 "Shorewood",
  #       #                 "Spring Lake Park",
  #       #                 "White Bear Lake") ~ county_name,
  #       TRUE ~ county_name)) %>%
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


vmt_city_raw %>% 
  filter(cprg_area ==  TRUE) %>% 
  plot_ly(
    type = "histogram",
    x = ~percent_sampled,
    color = ~county_name
  )

vmt_city_raw_route_system <- vmt_city_raw %>% 
  left_join(mndot_route_system,
            by = "route_system")

# summarize by year and county only ------
# removing the route system distinction
vmt_city_raw_summary <-
  vmt_city_raw_route_system %>%
  filter(route_system_id %in% c("01", "02", "03", "04", "05",
                                "06", "07")) %>%
  group_by(year, county_name, ctu_name, cprg_area) %>%
  summarize(
    daily_vmt = sum(daily_vmt, na.rm = TRUE),
    annual_vmt = sum(annual_vmt, na.rm = TRUE),
    centerline_miles = sum(centerline_miles, na.rm = TRUE),
    .groups = "keep"
  ) %>% 
  ungroup() %>% 
  filter(county_name %in% cprg_county$county_name)

vmt_city_alone <- vmt_city_raw_route_system %>% 
  filter(cprg_area == TRUE) %>% 
  group_by(year, ctu_name) %>% 
  summarize(
    daily_vmt = sum(daily_vmt, na.rm = TRUE),
    annual_vmt = sum(annual_vmt, na.rm = TRUE),
    centerline_miles = sum(centerline_miles, na.rm = TRUE),
    .groups = "keep"
  ) 

vmt_county_alone <- vmt_city_raw_route_system %>% 
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
            suffix = c(".city", ".county"))


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


ctu_n_years <- vmt_city_raw_summary %>% 
  select(ctu_name, year) %>% 
  unique() %>% 
  group_by(ctu_name) %>% 
  count(name = "n_years") %>% 
  arrange(n_years)

# basic plotting
# note that note very CTU has data for the full time series
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
  filter(
    # ctu_population >= 65000,
    ctu_name %in% ctu_nov
  ) %>% 
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
  filter(ctu_population >= 65000) %>% 
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
  ) %>% View

# TODO interpolate 2015 data
# we can't interpolate for CTUs that don't have a full time series
# TODO verify that CTU-county totals equal vmt_county totals
# TODO final save
# TODO testing!
# interpolate 2015 data -----

interp_ctus <- ctu_n_years %>% 
  filter(n_years >= 22)  

vmt_interp <- vmt_city_alone %>%
  filter(ctu_name %in% interp_ctus$ctu_name) %>% 
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
  bind_rows(vmt_city_alone %>% 
              filter(ctu_name %in% interp_ctus$ctu_name)) %>%
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
  select(-daily_approx, -annual_approx, -centerline_approx)

# this saves a version with only the CTUs that have a full time series

saveRDS(vmt_ctu, "_transportation/data-raw/mndot/mndot_vmt_ctu.RDS")

# note that "New Market" is included in VMT data for years 2000-2005
# "Elko" is included for years 2001-2006
# "Elko New Market" is included for years 2007-onward
