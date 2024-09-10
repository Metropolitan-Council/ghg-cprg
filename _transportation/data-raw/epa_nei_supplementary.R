# deep cuts from NEI
source("R/download_read_table.R")
source("_meta/data-raw/county_geography.R")
source("R/_load_pkgs.R")
source("_transportation/data-raw/epa_source_classification_codes.R")
dot_vmt <- readRDS("_transportation/data/dot_vmt.RDS")
options(timeout = 130)

# 2020 NEI -----
# ## find which counties were used as representatives for all others  -----
if (!file.exists("_transportation/data-raw/epa/nei/2020NEI/2020_Representative_Counties_Analysis_20220720.xlsx")) {
  rep_counties_raw <- download_read_table("https://gaftp.epa.gov/Air/nei/2020/doc/supporting_data/onroad/2020_Representative_Counties_Analysis_20220720.xlsx",
    exdir = "_transportation/data-raw/epa/nei/",
    sheet = 2,
    col_types = "text"
  ) %>%
    clean_names() %>%
    filter(county_id %in% county_geography$geoid)
} else {
  rep_counties_raw <- readxl::read_excel("_transportation/data-raw/epa/nei/2020NEI/2020_Representative_Counties_Analysis_20220720.xlsx",
    sheet = 2,
    col_types = "text"
  ) %>%
    clean_names() %>%
    filter(county_id %in% county_geography$geoid)
}

counties_light <- county_geography %>%
  select(geoid, state_name, county_name, cprg_area)


rep_counties <- counties_light %>%
  left_join(rep_counties_raw,
    by = c("geoid" = "county_id")
  ) %>%
  left_join(counties_light,
    by = c("x2020_final_rep_county" = "geoid")
  ) %>%
  mutate(self_represented = county_name.x == county_name.y) %>%
  select(geoid,
    county_name = county_name.x,
    represented_by = county_name.y,
    cprg_area = cprg_area.x,
    self_represented
  )

## onroad activity data -----

scc_codes <- read_xlsx("_transportation/data-raw/epa/nei/2020NEI/onroad_activity_data_SCC_descriptions.xlsx",
  sheet = 1,
  col_types = "text"
) %>%
  clean_names()

# download and unzip if it doesn't already exist
# All three data types are in FF10 format for SMOKE and are a combination of
#   EPA estimates, agency submittals, and corrections:
# 1.Vehicle population by county and SCC covering every county in the U.S.,
# 2.VMT annual and monthly by county and SCC covering every county in the U.S., and
# 3.Hoteling hours annual and monthly by county covering every county in the U.S.
#   including hours of extended idle and hours of auxiliary power units for
#   combination long-haul trucks only.
# 4.Off-network idle hours by county and SCC.
# 5.Starts by county and SCC.
if (!file.exists("_transportation/data-raw/epa/nei/2020NEI/2020NEI_onroad/inputs/onroad/VMT_2020NEI_full_monthly_run3_09jan2023_v0.csv")) {
  download.file("https://gaftp.epa.gov/Air/nei/2020/doc/supporting_data/onroad/2020NEI_onroad_activity_final_20230112.zip",
    destfile = "_transportation/data-raw/epa/nei/2020NEI/2020NEI_onroad_activity_final_20230112.zip"
  )
  unzip(
    zipfile = "_transportation/data-raw/epa/nei/2020NEI/2020NEI_onroad_activity_final_20230112.zip",
    exdir = "_transportation/data-raw/epa/nei/2020NEI/",
    overwrite = TRUE
  )
}


onroad_input_colnames <- c(
  "country_cd", "region_cd", "tribal_code", "census_tract_cd",
  "shape_id", "scc", "CD", "MSR", "activity_type", "ann_parm_value",
  "calc_year", "date_updated", "data_set_id", "jan_value",
  "feb_value", "mar_value", "apr_value", "may_value",
  "jun_value", "jul_value", "aug_value", "sep_value",
  "oct_value", "nov_value", "dec_value", "comment"
)

vmt <- data.table::fread("_transportation/data-raw/epa/nei/2020NEI/2020NEI_onroad/inputs/onroad/VMT_2020NEI_full_monthly_run3_09jan2023_v0.csv",
  skip = 16,
  header = FALSE,
  colClasses = "character",
  col.names = onroad_input_colnames
) %>%
  filter(region_cd %in% county_geography$geoid) %>%
  left_join(counties_light,
    by = c("region_cd" = "geoid")
  ) %>%
  left_join(scc_codes, by = c("scc" = "scc")) %>%
  mutate(across(ends_with("value"), as.numeric)) %>%
  rowwise() %>%
  select(-tribal_code, -census_tract_cd)

hoteling <- data.table::fread("_transportation/data-raw/epa/nei/2020NEI/2020NEI_onroad/inputs/onroad/HOTELING_2020NEI_monthly_23nov2022_v4.csv",
  skip = 15,
  header = FALSE,
  colClasses = "character",
  col.names = onroad_input_colnames
) %>%
  filter(region_cd %in% county_geography$geoid) %>%
  left_join(counties_light,
    by = c("region_cd" = "geoid")
  ) %>%
  left_join(scc_codes, by = c("scc" = "scc")) %>%
  mutate(across(ends_with("value"), as.numeric)) %>%
  rowwise()


starts <- data.table::fread("_transportation/data-raw/epa/nei/2020NEI/2020NEI_onroad/inputs/onroad/STARTS_2020NEI_full_monthly_run3_09jan2023_v0.csv",
  skip = 15,
  colClasses = "character",
  col.names = onroad_input_colnames
) %>%
  filter(region_cd %in% county_geography$geoid) %>%
  left_join(counties_light,
    by = c("region_cd" = "geoid")
  ) %>%
  left_join(scc_codes, by = c("scc" = "scc")) %>%
  mutate(across(ends_with("value"), as.numeric)) %>%
  rowwise()


oni <- data.table::fread("_transportation/data-raw/epa/nei/2020NEI/2020NEI_onroad/inputs/onroad/ONI_2020NEI_full_monthly_run3_09jan2023_v0.csv",
  skip = 16,
  colClasses = "character",
  col.names = onroad_input_colnames
) %>%
  filter(region_cd %in% county_geography$geoid) %>%
  left_join(counties_light,
    by = c("region_cd" = "geoid")
  ) %>%
  left_join(scc_codes, by = c("scc" = "scc")) %>%
  mutate(across(ends_with("value"), as.numeric)) %>%
  rowwise()

# vehicle population
vpop <- data.table::fread("_transportation/data-raw/epa/nei/2020NEI/2020NEI_onroad/inputs/onroad/VPOP_2020NEI_full_20220729_07oct2022_v1.csv",
  skip = 27,
  colClasses = "character",
  col.names = onroad_input_colnames
) %>%
  filter(region_cd %in% county_geography$geoid) %>%
  left_join(counties_light,
    by = c("region_cd" = "geoid")
  ) %>%
  left_join(scc_codes, by = c("scc" = "scc")) %>%
  mutate(across(ends_with("value"), as.numeric)) %>%
  rowwise()

## temperatures -----
# The temperature and relative humidity bins for running MOVES to create
# the full range of emissions factors necessary to run SMOKEMOVES and the
# ZMH files used to run MOVES. Generated by running the SMOKE Met4moves program.
download.file("https://gaftp.epa.gov/Air/nei/2020/doc/supporting_data/onroad/2020NEI_RepCounty_Temperatures.zip",
  destfile = "_transportation/data-raw/epa/nei/2020NEI/2020NEI_RepCounty_Temperatures.zip"
)


## MOVES run specifications -----
# download.file("https://gaftp.epa.gov/Air/nei/2020/doc/supporting_data/onroad/CDBs_for_rep_counties/2020_RepCounty_Runspecs.zip",
#               "_transportation/data-raw/epa/nei/2020_RepCounty_Runspecs.zip"
# )
# unzip("_transportation/data-raw/epa/nei/2020_RepCounty_Runspecs.zip",
#       exdir = "_transportation/data-raw/epa/nei/2020_RepCounty_Runspecs/",
#     overwrite = TRUE)

## speed and distance -----

# download.file("https://gaftp.epa.gov/Air/nei/2020/doc/supporting_data/onroad/2020NEI_spdist.zip",
#               "_transportation/data-raw/epa/nei/2020NEI_spdist.zip"
# )

## compare VMT data ------

vmt_summary <- vmt %>%
  mutate(
    geoid = region_cd,
    year = calc_year
  ) %>%
  filter(cprg_area == TRUE) %>%
  group_by(NAME, cprg_area, geoid, year) %>%
  summarize(ann_parm_value = sum(ann_parm_value, na.rm = T))

# close-ish? Ramsey County has the biggest disparity, for some reason
dot_vmt %>%
  right_join(vmt_summary,
    by = c(
      "geoid", "cprg_area",
      "year"
    )
  ) %>%
  mutate(
    vmt_diff = annual_vmt - ann_parm_value,
    vmt_pct_diff = (vmt_diff / annual_vmt)
  ) %>%
  arrange(vmt_pct_diff)

# compare with streetlight VMT results
readRDS("_transportation/data/county_vmt_emissions.RDS") %>%
  group_by(zone, year) %>%
  summarize(vmt_total = sum(vmt_total)) %>%
  arrange(-vmt_total)

# 2017 NEI -----

# download_read_table("https://gaftp.epa.gov/air/nei/2017/doc/supporting_data/onroad/2017_Representative_Counties_Analysis_20191220.xlsx",
#                     exdir = "_transportation/data-raw/epa/nei/2017NEI/",
#                     sheet = 2)

download.file("https://gaftp.epa.gov/air/nei/2017/doc/supporting_data/onroad/MOVES_Input_DBs.zip",
  destfile = "_transportation/data-raw/epa/nei/2017NEI/MOVES_Input_DBs.zip"
)



vmt17 <- data.table::fread("_transportation/data-raw/epa/nei/2017NEI/2017NEI_onroad_activity_final/VMT_2017NEI_final_from_CDBs_month_redist_27mar2020_v3.csv",
  skip = 16,
  header = FALSE,
  colClasses = "character",
  col.names = onroad_input_colnames
) %>%
  filter(region_cd %in% county_geography$geoid) %>%
  left_join(counties_light,
    by = c("region_cd" = "geoid")
  ) %>%
  left_join(scc_codes, by = c("scc" = "scc")) %>%
  mutate(across(ends_with("value"), as.numeric)) %>%
  rowwise() %>%
  select(-tribal_code, -census_tract_cd)


vmt17_summary <- vmt17 %>%
  mutate(
    geoid = region_cd,
    year = calc_year
  ) %>%
  filter(cprg_area == TRUE) %>%
  group_by(NAME, cprg_area, geoid, year) %>%
  summarize(ann_parm_value = sum(ann_parm_value, na.rm = T))


# 2017 VMT differences are MUCH smaller (practically negligible)
dot_vmt %>%
  right_join(vmt17_summary,
    by = c(
      "geoid", "cprg_area",
      "year"
    )
  ) %>%
  mutate(
    vmt_diff = annual_vmt - ann_parm_value,
    vmt_pct_diff = (vmt_diff / annual_vmt)
  ) %>%
  arrange(-annual_vmt)
