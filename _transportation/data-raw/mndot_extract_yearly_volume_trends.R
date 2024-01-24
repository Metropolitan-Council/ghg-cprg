# Import and clean MnDOT vehicle classification data
## MnDOT reports the percentage of vehicles by vehicle class at specific
## traffic counters across the state. These are stored in HTML reports and
## downloaded from MnDOT's website. We will use {htmltab} to pull the
## relevant tables we need.

source("R/_load_pkgs.R")
library(htmltab)


get_yearly_vol_percentage <- function(report_name, report_number) {
  htmltab::htmltab(
    doc = report_name,
    which = 2 # pull the second table
  ) %>%
    mutate(station_id = report_number) %>% # add a column with the station ID
    select(station_id, everything()) # put the station id column first
}


# list all the reports
report_list <- list.files("_transportation/data-raw/mndot/yearly_volume_trends_with_distribution_reports/",
  full.names = TRUE
)

# get station numbers
station_nums <- stringr::str_remove(report_list,
  pattern = "_transportation/data-raw/mndot/yearly_volume_trends_with_distribution_reports//"
) %>%
  stringr::str_split(pattern = "-") %>%
  lapply(`[[`, 1) %>%
  stringr::str_remove_all("[:alpha:]") %>%
  stringr::str_remove_all("[:punct:]")


# map to get_yearly_vol_percentage() using the list of report files and station numbers
percentages <- purrr::map2_df(report_list, station_nums, get_yearly_vol_percentage) %>%
  mutate(across(3:17, as.numeric)) %>% # convert percentages to numeric
  mutate(across(3:17, ~ . / 100)) %>% # divide percentage by 100 to get ratio
  unique() # double check for duplicates

# Define passenger, medium, and heavy duty percentages
# using classifications from MnDOT classification scheme
# https://www.dot.state.mn.us/traffic/data/reports/vc/Vehicle_Classification_Scheme.pdf
percent_by_class <- percentages %>%
  rowwise() %>%
  mutate(
    # aligns with WisDOT weight classifications
    passenger = sum(class1, class2, class3),
    medium_duty = sum(class4, class5, class6, class7),
    heavy_duty = sum(
      class8, class9,
      class10, class11,
      class12, class13
    ),
    total = sum(passenger, medium_duty, heavy_duty)
  ) %>% # check to see if the total is 1
  select(station_id, year, passenger, medium_duty, heavy_duty, total)


saveRDS(percent_by_class, paste0("_transportation/data-raw/mndot/yearly_volume_percentage_by_class.RDS"))

# filter to only CPRG counties and recent time periods -----
# download list of current stations
# download.file("https://www.dot.state.mn.us/traffic/data/reports/Current_CC_StationList.xlsx",
#               destfile = "_transportation/data-raw/mndot/Current_CC_StationList.xlsx")

station_list <- readxl::read_excel("_transportation/data-raw/mndot/Current_CC_StationList.xlsx") %>%
  clean_names() %>%
  filter(
    collection_type %in% c(
      "ATR Volume, Speed, Class",
      "WIM"
    ),
    county_name %in% c(
      "Anoka",
      "Carver",
      "Dakota",
      "Hennepin",
      "Scott",
      "Ramsey",
      "Washington",
      "Chisago",
      "Sherburne"
    )
  )


percent_by_class_metro <- filter(percent_by_class, station_id %in% station_list$continuous_number)


# calculate standard deviation for all
dev <- percent_by_class %>%
  filter(
    year >= 2017,
    station_id %in% station_list$continuous_number
  ) %>%
  group_by(station_id) %>%
  summarize(
    pass_sd = sd(passenger, na.rm = TRUE),
    med_sd = sd(medium_duty, na.rm = TRUE),
    heavy_sd = sd(heavy_duty, na.rm = TRUE), .groups = "keep"
  ) %>%
  gather(pass_sd, med_sd, heavy_sd,
    key = "weight",
    value = "sd"
  ) %>%
  group_by(station_id) %>%
  summarize(mean_sd = mean(sd, na.rm = TRUE), .groups = "keep")

dev

## individual station review
### * 335, an urban interstate in Anoka (2016)
filter(percent_by_class_metro, station_id == "335")
filter(station_list, continuous_number == "335")

### * 400 , a rural minor arterial in Hennepin (2017)
filter(percent_by_class_metro, station_id == "400")
filter(station_list, continuous_number == "400")

### * 402, urban major collector in Hennepin (2016)
filter(percent_by_class_metro, station_id == "402")
filter(station_list, continuous_number == "402")

## 381, rural principal arterial in Carver (2015)
filter(percent_by_class_metro, station_id == "381")
filter(station_list, continuous_number == "381")

## 353, rural principal arterial in Scott (2015)
filter(percent_by_class_metro, station_id == "353")
filter(station_list, continuous_number == "353")

## 335 is a different road class than the other stations in Anoka (2016)
## 402 is the only major collector (2016)
## 400 has 2017 data

# Export select data ----
# We want to save the most recent data for each station, back to 2017
# So if station XX has 2020 data, we don't want to save data for station XX
# from earlier than 2020.

# select stations with 2021 data
region_2021 <- percent_by_class_metro %>%
  filter(year == "2021")

# select stations with 2020 data that don't have 2021 data
region_2020 <- percent_by_class_metro %>%
  filter(
    year == "2020",
    !station_id %in% region_2021$station_id
  )

# select stations with 2019 data that don't have 2020 or 2021 data
region_2019 <- percent_by_class_metro %>%
  filter(
    year == "2019",
    !station_id %in% c(
      region_2020$station_id,
      region_2021$station_id
    )
  )


# select stations with 2018 data that do NOT have 2019, 2020, or 2021
region_2018 <- percent_by_class_metro %>%
  filter(
    year == "2018",
    !station_id %in% c(
      region_2019$station_id,
      region_2020$station_id,
      region_2021$station_id
    )
  )

# select stations with 2017 data that do NOT have 2018, 2019, 2020, 2021 data
region_2017 <- percent_by_class_metro %>%
  filter(
    year == "2017",
    !station_id %in% c(
      region_2018$station_id,
      region_2019$station_id,
      region_2020$station_id,
      region_2021$station_id
    )
  )

region_2021_plus <- rbind(
  region_2017,
  region_2018,
  region_2019,
  region_2020,
  region_2021
)

saveRDS(region_2021_plus, paste0("_transportation/data-raw/mndot/most_recent_yearly_volume_percentage_by_class.RDS"))
