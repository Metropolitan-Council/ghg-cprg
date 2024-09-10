# the EPA publishes data for years in between NEI releases
# these are modeled estimates based on a variety of sources
source("R/_load_pkgs.R")
source("_meta/data-raw/county_geography.R")

if (file.exists("_transportation/data-raw/epa/air_emissions_modeling/2022v1/2022hc_cb6_22m/inputs/onroad/VMT_2022v1_full_annual_20240226_monthly_18jun2024_nf_v4.csv")) {
  # 2022
  cli::cli_alert_warning("These files are hefty")
  # emissions
  download.file(
    "https://gaftp.epa.gov/Air/emismod/2022/v1/2022emissions/onroad_emissions_SMOKE-MOVES_FF10_2022hc_17jul2024.zip",
    "_transportation/data-raw/epa/air_emissions_modeling/2022v1/onroad_emissions_SMOKE-MOVES_FF10_2022hc_17jul2024.zip"
  )

  unzip("_transportation/data-raw/epa/air_emissions_modeling/2022v1/onroad_emissions_SMOKE-MOVES_FF10_2022hc_17jul2024.zip", exdir = "_transportation/data-raw/epa/air_emissions_modeling/2022v1/")

  # activity
  download.file(
    "https://gaftp.epa.gov/Air/emismod/2022/v1/2022emissions/onroad_activity_SMOKE-MOVES_FF10_2022hc_17jul2024.zip",
    "_transportation/data-raw/epa/air_emissions_modeling/2022v1/onroad_activity_SMOKE-MOVES_FF10_2022hc_17jul2024.zip"
  )
  unzip("_transportation/data-raw/epa/air_emissions_modeling/2022v1/onroad_activity_SMOKE-MOVES_FF10_2022hc_17jul2024.zip",
    exdir = "_transportation/data-raw/epa/air_emissions_modeling/2022v1/"
  )

  # onroad comparisons
  download.file("https://gaftp.epa.gov/Air/emismod/2022/v1/reports/mobile/onroad/2022v1%20onroad%20comparisons%2022-26-32-38%2010aug2024.xlsx",
    destfile = "_transportation/data-raw/epa/air_emissions_modeling/2022v1/2022v1 onroad comparisons 22-26-32-38 10aug2024.xlsx"
  )


  # 2021
  ## activity
  download.file(
    "https://gaftp.epa.gov/Air/emismod/2021/2021emissions/onroad_emissions_SMOKE-MOVES_FF10_2021hb_26mar2024.zip",
    "_transportation/data-raw/epa/air_emissions_modeling/2021/onroad_activity_2021hb_24may2024.zip"
  )

  unzip("_transportation/data-raw/epa/air_emissions_modeling/2021/onroad_activity_2021hb_24may2024.zip",
    exdir = "_transportation/data-raw/epa/air_emissions_modeling/2021/"
  )

  ## emissions
  download.file("https://gaftp.epa.gov/Air/emismod/2021/2021emissions/onroad_emissions_SMOKE-MOVES_FF10_2021hb_26mar2024.zip",
    destfile = "_transportation/data-raw/epa/air_emissions_modeling/2021/onroad_emissions_SMOKE-MOVES_FF10_2021hb_26mar2024.zip"
  )

  unzip("_transportation/data-raw/epa/air_emissions_modeling/2021/onroad_emissions_SMOKE-MOVES_FF10_2021hb_26mar2024.zip",
    exdir = "_transportation/data-raw/epa/air_emissions_modeling/2021/"
  )


  download.file(
    "https://gaftp.epa.gov/Air/emismod/2020/2020emissions/2020ha2_onroad_SMOKE-MOVES_emissions_FF10_22sep2023.zip",
    "_transportation/data-raw/epa/air_emissions_modeling/2020/2020ha2_onroad_SMOKE-MOVES_emissions_FF10_22sep2023.zip"
  )
}


vmt22 <- read_nei_vmt("_transportation/data-raw/epa/air_emissions_modeling/2022v1/2022hc_cb6_22m/inputs/onroad/VMT_2022v1_full_annual_20240226_monthly_18jun2024_nf_v4.csv")

mod22 <- fread("_transportation/data-raw/epa/air_emissions_modeling/2022v1/2022hc_cb6_22m/inputs/onroad/2022hc_onroad_SMOKE_MOVES_MOVES4_forAQ_27jun2024_v0.csv",
  keepLeadingZeros = TRUE
) %>%
  .[region_cd %in% county_geography$geoid, ] %>%
  select(
    -starts_with(tolower(month.abb)),
    -tribal_code, -census_tract_cd, -shape_id
  ) %>%
  mutate(
    scc = as.character(scc),
    scc6 = str_sub(scc, 1, 6),
    calc_year = as.character(calc_year)
  ) %>%
  filter(poll %in% c("CH4", "CO2", "N2O", "CO")) %>%
  left_join(counties_light,
    by = c("region_cd" = "geoid")
  ) %>%
  left_join(scc6_desc)


vmt22 %>%
  group_by(scc6, region_cd, cprg_area, county_name) %>%
  summarize(ann_parm_value = sum(ann_parm_value)) %>%
  filter(cprg_area == TRUE) %>%
  left_join(mod22 %>%
    filter(emis_type == "RPD")) %>%
  mutate(emis_total = ann_parm_value * ann_value) %>%
  filter(
    poll == "CO",
    county_name == "Hennepin"
  )


mod22
names(mod22)
