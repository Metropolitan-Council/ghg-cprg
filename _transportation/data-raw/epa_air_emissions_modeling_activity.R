# the EPA publishes data for years in between NEI releases
# these are modeled estimates based on a variety of sources
source("R/_load_pkgs.R")
source("_meta/data-raw/county_geography.R")

if (any(purrr::map(
  c(("_transportation/data-raw/epa/air_emissions_modeling/2022v1/2022hc_cb6_22m/inputs/onroad/VMT_2022v1_full_annual_20240226_monthly_18jun2024_nf_v4.csv")),
  file_exists
) == FALSE)) {
  cli::cli_abort(c(
    "Required datasets unavailable",
    "*" = "Consult documentation for more information",
    "*" = "{.file _transportation/data-raw/epa/README_epa_downloads.html}"
  ))
}

vmt22 <- read_nei_vmt("_transportation/data-raw/epa/air_emissions_modeling/2022v1/2022hc_cb6_22m/inputs/onroad/VMT_2022v1_full_annual_20240226_monthly_18jun2024_nf_v4.csv")

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
