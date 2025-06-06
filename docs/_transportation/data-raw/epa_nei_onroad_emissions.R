# fetch and process data from the onroad regional summary tables
source("R/_load_pkgs.R")
source("_meta/data-raw/county_geography.R")

if (any(purrr::map(
  c(
    "_transportation/data-raw/epa/nei/2008NEI/2008neiv3_onroad_byregions/2008NEIv3_onroad5.csv",
    "_transportation/data-raw/epa/nei/2011NEI/2011neiv2_onroad_byregions/onroad_5.csv",
    "_transportation/data-raw/epa/nei/2014NEI/2014neiv2_onroad_byregions/onroad_5.csv",
    "_transportation/data-raw/epa/nei/2017NEI/2017neiApr_onroad_byregions/onroad_5.csv",
    "_transportation/data-raw/epa/nei/2020NEI/2020nei_onroad_byregion/onroad_5.csv"
  ),
  file_exists
) == FALSE)) {
  cli::cli_abort(c(
    "Required datasets unavailable",
    "*" = "Consult documentation for more information",
    "*" = "{.file _transportation/data-raw/epa/README_epa_downloads.html}"
  ))
}

pollutants_keep <- c(
  "CH4", "N2O",
  "CO2", "NO", "NOX", "SO2", "NH3",
  "HFC", "VOC", "O3", "CO", "PFC", "SF6",
  "PM10-PRI", "PM25-PRI", "PM-CON"
)

v_or_2008 <- read.csv("_transportation/data-raw/epa/nei/2008NEI/2008neiv3_onroad_byregions/2008NEIv3_onroad5.csv",
  colClasses = "character"
) %>%
  clean_names() %>%
  mutate(
    geoid = state_and_county_fips_code,
    nei_inventory_year = "2008",
    pollutant_code = pollutant_cd,
    data_category = data_category_cd,
    emissions_uom = uom,
    pollutant_desc = description,
    data_set = data_set_short_name,
    emissions_operating_type = emissions_op_type_code,
    file_name = "2008neiv3_onroad_byregions/2008NEIv3_onroad5.csv"
  ) %>%
  filter(
    geoid %in% county_geography$geoid,
    pollutant_code %in% pollutants_keep
  )

v_or_2011 <- read.csv("_transportation/data-raw/epa/nei/2011NEI/2011neiv2_onroad_byregions/onroad_5.csv",
  colClasses = "character"
) %>%
  clean_names() %>%
  mutate(
    geoid = state_and_county_fips_code,
    nei_inventory_year = "2011",
    pollutant_code = pollutant_cd,
    emissions_uom = uom,
    data_category = data_category_cd,
    data_set = data_set_short_name,
    pollutant_desc = description,
    emissions_operating_type = emissions_op_type_code,
    file_name = "2011neiv2_onroad_byregions/onroad_5.csv"
  ) %>%
  filter(
    geoid %in% county_geography$geoid,
    pollutant_code %in% pollutants_keep
  )

v_or_2014 <- read.csv("_transportation/data-raw/epa/nei/2014NEI/2014neiv2_onroad_byregions/onroad_5.csv",
  colClasses = "character"
) %>%
  clean_names() %>%
  mutate(
    geoid = state_and_county_fips_code,
    nei_inventory_year = "2014",
    pollutant_code = pollutant_cd,
    emissions_uom = uom,
    data_category = data_category,
    file_name = "2014neiv2_onroad_byregions/onroad_5.csv"
  ) %>%
  filter(
    geoid %in% county_geography$geoid,
    pollutant_code %in% pollutants_keep
  )

v_or_2017 <- read.csv("_transportation/data-raw/epa/nei/2017NEI/2017neiApr_onroad_byregions/onroad_5.csv",
  colClasses = "character"
) %>%
  clean_names() %>%
  mutate(
    geoid = fips_code,
    nei_inventory_year = "2017",
    data_category = data_category,
    file_name = "2017neiApr_onroad_byregions/onroad_5.csv"
  ) %>%
  filter(
    geoid %in% county_geography$geoid,
    pollutant_code %in% pollutants_keep
  )

v_or_2020 <- read.csv("_transportation/data-raw/epa/nei/2020NEI/2020nei_onroad_byregion/onroad_5.csv",
  colClasses = "character"
) %>%
  clean_names() %>%
  mutate(
    geoid = fips_code,
    nei_inventory_year = "2020",
    file_name = "2020NEI/2020nei_onroad_byregion/onroad_5.csv"
  ) %>%
  filter(
    geoid %in% county_geography$geoid,
    pollutant_code %in% pollutants_keep
  )

# combine all
epa_nei_onroad_emissions <- bind_rows(
  v_or_2008, v_or_2011, v_or_2014, v_or_2017, v_or_2020
) %>%
  select(
    -aircraft_engine_type_cd, -tribal_name, -pollutant_cd, -county,
    -uom, -st_usps_cd, -state, -data_category_cd, -fips_state_code,
    -state_and_county_fips_code, -county_name, -description, -emissions_op_type_code,
    -data_set_short_name, -epa_region_code, -fips_code, -emission_operating_type, -sector,
    -emissions_type_code, -pollutant_type_s, -aetc, -reporting_period
  ) %>%
  select(geoid, scc, nei_inventory_year, everything()) %>%
  mutate(
    total_emissions = as.numeric(total_emissions),
    data_category = "Onroad",
    scc6 = stringr::str_sub(scc, 1, 6)
  ) %>%
  left_join(counties_light,
    by = join_by(geoid)
  )


saveRDS(epa_nei_onroad_emissions, "_transportation/data-raw/epa/epa_nei_onroad_emissions.RDS")


# other editions of the same dataset

# from https://gaftp.epa.gov/Air/nei/2017/doc/flat_files/2017NEI_onroad_20200427.zip
nei2017_conus <- read.csv("_transportation/data-raw/epa/nei/2017NEI/2017NEI_onroad_20200427/2017NEI_onroad_CONUS_noCalifornia.csv") %>%
  filter(
    region_cd %in% counties_light$geoid,
    poll %in% pollutants_keep
  ) %>%
  mutate(
    nei_inventory_year = "2017",
    data_category = "Onroad",
    geoid = as.character(region_cd),
    scc6 = stringr::str_sub(scc_agg, 1, 6)
  )

nei2017_conus %>%
  filter(
    geoid == "27053",
    poll == "CO2"
  ) %>%
  left_join(
    epa_nei_onroad_emissions %>%
      filter(
        geoid == "27053",
        pollutant_code == "CO2"
      )
  ) %>%
  mutate(diff = total_emissions - ann_value) %>%
  filter(abs(diff) > 1) %>%
  nrow()
