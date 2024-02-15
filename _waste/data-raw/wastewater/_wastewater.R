source("R/_load_pkgs.R")

county_wastewater <- readRDS("_waste/data-raw/wastewater/county_wastewater.RDS")

wastewater_out <- county_wastewater %>%
  mutate(
    co2e = if_else(substr(NAME, nchar(NAME) - 8, nchar(NAME)) == "Wisconsin", epa_co2e, state_co2e),
    source = if_else(substr(NAME, nchar(NAME) - 8, nchar(NAME)) == "Wisconsin",
      "Environmental Protection Agency",
      "Minnesota Pollution Control Agency"
    )
  ) %>%
  mutate(COUNTY_NAME = gsub(" County, Wisconsin", "", gsub(" County, Minnesota", "", NAME))) %>%
  mutate(year = 2020) %>%
  select(COUNTY_NAME, year, co2e, source)

wastewater_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "NAME", class(wastewater_out$COUNTY_NAME), "County name",
    "year", class(wastewater_out$year), "Year",
    "co2e", class(wastewater_out$co2e), "Metric tons of CO2 equivalency generated from municipal wastewater",
    "source", class(wastewater_out$source), "Data source",
  )

saveRDS(wastewater_out, "_waste/data/wastewater.RDS")
saveRDS(wastewater_meta, "_waste/data/wastewater_meta.RDS")
