#### bring in NOAA historical weather and compile

noaa <- read_csv("_meta/data-raw/climate/noaa_hennepin_2015-2021.csv")

noaa_daily <- noaa %>% filter(!is.na(DailyHeatingDegreeDays)) %>% 
  mutate(inventory_year = year(DATE),
         month = month(DATE))

noaa_month <- noaa_daily %>% 
  group_by(inventory_year,month) %>% 
  summarize(heating_degree_days = sum(DailyHeatingDegreeDays),
            cooling_degree_days = sum(DailyCoolingDegreeDays),
            dry_bulb_temp = mean(DailyAverageDryBulbTemperature))

noaa_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "inventory_year", class(noaa_month$inventory_year), "Year of temperature recording",
    "month", class(noaa_month$month), "Month of temperature recording",
    "heating_degree_days ", class(noaa_month$heating_degree_days), "Accumulated number of degrees below 65F per day (amount of heating needed)",
    "cooling_degree_days ", class(noaa_month$cooling_degree_days), "Accumulated number of degrees above 65F per day (amount of cooling needed)",
    "dry_bulb_temp", class(noaa_month$dry_bulb_temp), "Average monthly dry bulb temperature"
  )


saveRDS(noaa_month, "./_meta/data/noaa_weather_2015-2021.rds")
saveRDS(noaa_meta, "./_meta/data/noaa_weather_2015-2021_meta.rds")
