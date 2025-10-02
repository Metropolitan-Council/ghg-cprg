# the EPA publishes data for years in between NEI releases
# these are modeled estimates based on a variety of sources
# Copies of the intermediary datasets are available in our MS Team/OneDrive.
tictoc::tic(msg = "Air emissions modeling point processing")

source("R/_load_pkgs.R")
source("_meta/data-raw/county_geography.R")
source("_transportation/data-raw/_read_smoke_ff10.R")
library(furrr)
rm(county_geography)
# check that we have all the necessary files -----
if (any(purrr::map(
  c(
    "_transportation/data-raw/epa/air_emissions_modeling/2022v2/2022hd_cb6_22m/inputs/airports/2022hd_airports_2022_point_top51_adjusted_ATL_data_20250428_27may2025_v0.csv"
  ),
  file_exists
) == FALSE)) {
  cli::cli_abort(c(
    "Required datasets unavailable",
    "*" = "Consult documentation for more information",
    "*" = "{.file _transportation/data-raw/epa/README_epa_downloads.html}"
  ))
}

# read and save -----
point_emissions_platform <- c(
  "_transportation/data-raw/epa/air_emissions_modeling/2022v2/2022hd_cb6_22m/inputs/airports/2022hd_airports_2022_point_top51_adjusted_ATL_data_20250428_27may2025_v0.csv",
  "_transportation/data-raw/epa/air_emissions_modeling/2022v2/2022hd_cb6_22m/inputs/pt_oilgas/oilgas_2022_POINT_20250812_14aug2025_v0.csv",
  "_transportation/data-raw/epa/air_emissions_modeling/2022v2/2022hd_cb6_22m/inputs/ptegu/egu_2022_POINT_20250812_multifix_2022cems_14aug2025_v0.csv",
  "_transportation/data-raw/epa/air_emissions_modeling/2022v2/2022hd_cb6_22m/inputs/ptnonipm/nonegu_norail_2022_POINT_20250812_14aug2025_nf_v1.csv",
  "_transportation/data-raw/epa/air_emissions_modeling/2022v2/2022hd_cb6_22m/inputs/ptnonipm/railyards_2022_POINT_20250812_14aug2025_v0.csv"
)


purrr::map(
  split(point_emissions_platform, sort(1:length(point_emissions_platform) %% 2)),
  safely(
    function(x) {
      # future::plan(strategy = future::multisession, workers = length(x))

      # furrr::future_map(
      purrr::map(
        x,
        read_smoke_ff10,
        out_directory = "_transportation/data-raw/epa/air_emissions_modeling/air_emissions_modeling_mn_wi/point/"
      )

      Sys.sleep(5)
    }
  )
)

# read back in, combine, and save -----
purrr::map(
  list.files("_transportation/data-raw/epa/air_emissions_modeling/air_emissions_modeling_mn_wi/point/",
    full.names = TRUE
  ),
  readRDS
) %>%
  bind_rows() %>%
  unique() %>%
  saveRDS("_transportation/data-raw/epa/air_emissions_modeling/point_mn_wi.RDS", compress = "xz")

tictoc::toc()
