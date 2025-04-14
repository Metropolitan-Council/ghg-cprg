#### bring in NOAA historical weather and compile
# https://www.ncei.noaa.gov/cdo-web/datatools/lcd

msp_2015 <- read_csv("_meta/data-raw/climate/noaa_msp_2015-2024.csv")
msp_2005 <- read_csv("_meta/data-raw/climate/noaa_msp_2005-2014.csv")

msp_month <- bind_rows(
  msp_2005 %>%
    filter(!is.na(DailyHeatingDegreeDays)) %>%
    mutate(
      inventory_year = year(DATE),
      month = month(DATE)
    ) %>%
    group_by(inventory_year, month) %>%
    summarize(
      heating_degree_days = sum(DailyHeatingDegreeDays),
      cooling_degree_days = sum(DailyCoolingDegreeDays),
      dry_bulb_temp = mean(DailyAverageDryBulbTemperature)
    ),
  msp_2015 %>%
    filter(!is.na(DailyHeatingDegreeDays)) %>%
    mutate(
      inventory_year = year(DATE),
      month = month(DATE)
    ) %>%
    group_by(inventory_year, month) %>%
    summarize(
      heating_degree_days = sum(DailyHeatingDegreeDays),
      cooling_degree_days = sum(DailyCoolingDegreeDays),
      dry_bulb_temp = mean(DailyAverageDryBulbTemperature)
    )
) %>% ungroup()

noaa_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "inventory_year", class(msp_month$inventory_year), "Year of temperature recording",
    "month", class(msp_month$month), "Month of temperature recording",
    "heating_degree_days ", class(msp_month$heating_degree_days), "Accumulated number of degrees below 65F per day (amount of heating needed)",
    "cooling_degree_days ", class(msp_month$cooling_degree_days), "Accumulated number of degrees above 65F per day (amount of cooling needed)",
    "dry_bulb_temp", class(msp_month$dry_bulb_temp), "Average monthly dry bulb temperature"
  )


saveRDS(msp_month, "./_meta/data/noaa_weather_monthly.rds")
saveRDS(noaa_meta, "./_meta/data/noaa_weather_monthy_meta.rds")
