# NEI SMOKE flat files
# Copies of the intermediary datasets are available in our MS Team/OneDrive.
source("R/_load_pkgs.R")
source("_meta/data-raw/county_geography.R")
source("_transportation/data-raw/_read_smoke_ff10.R")
library(furrr)
tictoc::tic("NEI SMOKE flat file proccessing")

# check that we have all the necessary files -----
if (any(purrr::map(
  c(
    "_transportation/data-raw/epa/nei/2020NEI/SmokeFlatFile_ONROAD_20230330.csv",
    "_transportation/data-raw/epa/nei/2017NEI/home/callen05/tmp/2017NEI_onroad_SMOKE_MOVES_NEIstyle_09mar2020_FIPS_27.csv",
    "_transportation/data-raw/epa/nei/2017NEI/home/callen05/tmp/2017NEI_onroad_SMOKE_MOVES_NEIstyle_09mar2020_FIPS_55.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2017/2017gb_17j/inputs/onroad/2017gb_nata_onroad_SMOKE_MOVES_NATAstyle_14may2020_v0.csv",
    "_transportation/data-raw/epa/nei/2014NEI/SmokeFlatFile_ONROAD_20160910.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2014/2014fd_cb6_14j/inputs/onroad/2014fd_nata_onroad_SMOKE_MOVES_MOVES2014a_AQstyle_06feb2018_v0.csv",
    "_transportation/data-raw/epa/nei/2014NEI/2014fd_cb6_14j/inputs/onroad/2014fd_onroad_FF10_SMOKE_MOVES2014a_FIPS_27.csv",
    "_transportation/data-raw/epa/nei/2014NEI/2014fd_cb6_14j/inputs/onroad/2014fd_onroad_FF10_SMOKE_MOVES2014a_FIPS_27.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2011/2011ek_onroad_SMOKE_MOVES_MOVES2014a_forOTAQ_21jan2016_v2_part1.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2011/2011ek_onroad_SMOKE_MOVES_MOVES2014a_forOTAQ_21jan2016_v2_part2.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2011/2011el_cb6v2_v6_11g/inputs/onroad/2011el_onroad_SMOKE_MOVES_MOVES2014a_forOTAQ_31aug2016_v1_part1.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2011/2011el_cb6v2_v6_11g/inputs/onroad/2011el_onroad_SMOKE_MOVES_MOVES2014a_forOTAQ_31aug2016_v1_part2.csv"
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

# number of workers should match number of items in the vector
plan(strategy = future::multisession, workers = 10)

# On the main webpage for NEI years 2017, 2014, and 2011
# the site says to get modeling/flat files from the air emissions modeling
# platforms. However, digging around the FTP site, there are multiple sets of
# flat files for each year found in different places.
# for easier access, we will compile all options
furrr::future_map(
  c(
    # 2020 NEI has a flat file directly on the main webpage
    "_transportation/data-raw/epa/nei/2020NEI/SmokeFlatFile_ONROAD_20230330.csv",

    # 2017, 2014, and 2011 NEI references the air emissions modeling platforms for Smoke flat files
    # 2017 has two options
    # 2017NEI is broken up by state
    # 2017gb is from the air emissions modeling plaforms
    "_transportation/data-raw/epa/nei/2017NEI/home/callen05/tmp/2017NEI_onroad_SMOKE_MOVES_NEIstyle_09mar2020_FIPS_27.csv",
    "_transportation/data-raw/epa/nei/2017NEI/home/callen05/tmp/2017NEI_onroad_SMOKE_MOVES_NEIstyle_09mar2020_FIPS_55.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2017/2017gb_17j/inputs/onroad/2017gb_nata_onroad_SMOKE_MOVES_NATAstyle_14may2020_v0.csv",

    # 2014 has two options
    # the first option (dated 20160910) represents 2014 v1 (https://gaftp.epa.gov/air/nei/2014/flat_files/README_2014NEIv1_flat_files.txt)
    # 2014fd represents 2014 v2
    # 2014fd by state is the exact same as 2014fd
    # 2014fd also has a CO and CO2 additional file that was run in 2018
    "_transportation/data-raw/epa/nei/2014NEI/SmokeFlatFile_ONROAD_20160910.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2014/2014fd_cb6_14j/inputs/onroad/2014fd_nata_onroad_SMOKE_MOVES_MOVES2014a_AQstyle_06feb2018_v0.csv",
    "_transportation/data-raw/epa/nei/2014NEI/2014fd_cb6_14j/inputs/onroad/2014fd_onroad_FF10_SMOKE_MOVES2014a_FIPS_27.csv",
    "_transportation/data-raw/epa/nei/2014NEI/2014fd_cb6_14j/inputs/onroad/2014fd_onroad_FF10_SMOKE_MOVES2014a_FIPS_27.csv",
    "_transportation/data-raw/epa/nei/2014NEI/CO_CO2_2014fd_nata_onroad_SMOKE_MOVES_MOVES2014a_forNATA_12jan2018_v1.csv",
    # 2011 has two options
    # 2011el is from January 2016
    # 2011ek is from August 2016
    "_transportation/data-raw/epa/air_emissions_modeling/2011/2011ek_onroad_SMOKE_MOVES_MOVES2014a_forOTAQ_21jan2016_v2_part1.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2011/2011ek_onroad_SMOKE_MOVES_MOVES2014a_forOTAQ_21jan2016_v2_part2.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2011/2011el_cb6v2_v6_11g/inputs/onroad/2011el_onroad_SMOKE_MOVES_MOVES2014a_forOTAQ_31aug2016_v1_part1.csv",
    "_transportation/data-raw/epa/air_emissions_modeling/2011/2011el_cb6v2_v6_11g/inputs/onroad/2011el_onroad_SMOKE_MOVES_MOVES2014a_forOTAQ_31aug2016_v1_part2.csv"
  ),
  read_smoke_ff10,
  out_directory = "_transportation/data-raw/epa/nei/SmokeFlatFile_MN_WI//"
)

# read back in, combine, and save -----
purrr::map(
  c(
    # 2020
    "_transportation/data-raw/epa/nei/SmokeFlatFile_MN_WI/SmokeFlatFile_ONROAD_20230330.RDS",

    # 2017
    # go with gb, because it is more recent
    "_transportation/data-raw/epa/nei/SmokeFlatFile_MN_WI/2017gb_nata_onroad_SMOKE_MOVES_NATAstyle_14may2020_v0.RDS",
    # "_transportation/data-raw/epa/nei/SmokeFlatFile_MN_WI/2017NEI_onroad_SMOKE_MOVES_NEIstyle_09mar2020_FIPS_27.RDS",
    # "_transportation/data-raw/epa/nei/SmokeFlatFile_MN_WI/2017NEI_onroad_SMOKE_MOVES_NEIstyle_09mar2020_FIPS_55.RDS",

    # 2014
    # go with fd because this represents the most recent version of 2014 NEI (v2)
    # "_transportation/data-raw/epa/nei/SmokeFlatFile_MN_WI/SmokeFlatFile_ONROAD_20160910.RDS",
    "_transportation/data-raw/epa/nei/SmokeFlatFile_MN_WI/2014fd_nata_onroad_SMOKE_MOVES_MOVES2014a_AQstyle_06feb2018_v0.RDS",
    # "_transportation/data-raw/epa/nei/2014NEI/2014fd_cb6_14j/inputs/onroad/2014fd_onroad_FF10_SMOKE_MOVES2014a_FIPS_27.csv",
    # "_transportation/data-raw/epa/nei/2014NEI/2014fd_cb6_14j/inputs/onroad/2014fd_onroad_FF10_SMOKE_MOVES2014a_FIPS_27.csv",
    "_transportation/data-raw/epa/nei/SmokeFlatFile_MN_WI/CO_CO2_2014fd_nata_onroad_SMOKE_MOVES_MOVES2014a_forNATA_12jan2018_v1.RDS",

    # 2011
    # we will go with el, because it is more recent
    "_transportation/data-raw/epa/nei/SmokeFlatFile_MN_WI/2011el_onroad_SMOKE_MOVES_MOVES2014a_forOTAQ_31aug2016_v1_part1.RDS",
    "_transportation/data-raw/epa/nei/SmokeFlatFile_MN_WI/2011el_onroad_SMOKE_MOVES_MOVES2014a_forOTAQ_31aug2016_v1_part2.RDS"
    # "_transportation/data-raw/epa/nei/SmokeFlatFile_MN_WI/2011ek_onroad_SMOKE_MOVES_MOVES2014a_forOTAQ_21jan2016_v2_part1.RDS",
    # "_transportation/data-raw/epa/nei/SmokeFlatFile_MN_WI/2011ek_onroad_SMOKE_MOVES_MOVES2014a_forOTAQ_21jan2016_v2_part2.RDS"),
  ),
  readRDS
) %>%
  bind_rows() %>%
  unique() %>%
  # CO was included in both the CO&CO2 version of 2014fd and the original 2014fd
  # remove CO from the original 2014fd
  mutate(remove_row = (poll == "CO" & calc_year == "2014" & file_location == "_transportation/data-raw/epa/air_emissions_modeling/2014/2014fd_cb6_14j/inputs/onroad/2014fd_nata_onroad_SMOKE_MOVES_MOVES2014a_AQstyle_06feb2018_v0.csv")) %>%
  filter(
    remove_row != TRUE
  ) %>%
  select(-remove_row) %>%
  saveRDS("_transportation/data-raw/epa/nei/epa_nei_smoke_ff.RDS", compress = "xz")

tictoc::toc()


# # compare data within individual years -----
#
# # 2017
# smoke_2017gb <- readRDS("_transportation/data-raw/epa/nei/SmokeFlatFile_MN_WI/2017gb_nata_onroad_SMOKE_MOVES_NATAstyle_14may2020_v0.RDS")
#
# smoke_2017nei <- bind_rows(
#   readRDS("_transportation/data-raw/epa/nei/SmokeFlatFile_MN_WI/2017NEI_onroad_SMOKE_MOVES_NEIstyle_09mar2020_FIPS_27.RDS"),
#   read_rds("_transportation/data-raw/epa/nei/SmokeFlatFile_MN_WI/2017NEI_onroad_SMOKE_MOVES_NEIstyle_09mar2020_FIPS_55.RDS"))
#
# waldo::compare(
#   smoke_2017gb %>% select(metadata_info) %>% unique() %>% extract2("metadata_info") %>% strsplit("#"),
#   smoke_2017nei %>% select(metadata_info) %>% unique() %>% extract2("metadata_info") %>% strsplit("#"), max_diffs = Inf)
#
#
# smoke_2017gb %>%
#   full_join(smoke_2017nei,
#             by = join_by(region_cd, scc, emis_type, poll, calc_year, scc6),
#             suffix = c(".gb", ".nei")) %>%
#   mutate(diff = emissions_short_tons.gb - emissions_short_tons.nei) %>% View
#
#
# # 2014
# smoke_2014fd <- readRDS("_transportation/data-raw/epa/nei/SmokeFlatFile_MN_WI/2014fd_nata_onroad_SMOKE_MOVES_MOVES2014a_AQstyle_06feb2018_v0.RDS")
#
# smoke2014fd2 <- bind_rows(
#   read_rds("_transportation/data-raw/epa/nei/SmokeFlatFile_MN_WI/2014fd_onroad_FF10_SMOKE_MOVES2014a_FIPS_55.RDS"),
#   read_rds("_transportation/data-raw/epa/nei/SmokeFlatFile_MN_WI/2014fd_onroad_FF10_SMOKE_MOVES2014a_FIPS_27.RDS")
# )
#
# smoke2014fd2_co <- read_rds("_transportation/data-raw/epa/nei/SmokeFlatFile_MN_WI/CO_CO2_2014fd_nata_onroad_SMOKE_MOVES_MOVES2014a_forNATA_12jan2018_v1.RDS")
#
# smoke2014nei <- read_rds( "_transportation/data-raw/epa/nei/SmokeFlatFile_MN_WI/SmokeFlatFile_ONROAD_20160910.RDS")
#
# smoke2014fd2_co %>% select(metadata_info) %>% unique() %>% extract2("metadata_info") %>% strsplit("#")
# smoke2014nei %>% select(metadata_info) %>% unique() %>% extract2("metadata_info") %>% strsplit("#")
#
# smoke_2014fd %>%
#   full_join(smoke2014nei,
#             by = join_by(region_cd, scc, poll, calc_year, scc6),
#             suffix = c(".fd", ".nei")) %>%
#   mutate(diff = emissions_short_tons.fd - emissions_short_tons.nei) %>% View
#
#
# smoke_2014fd %>%
#   full_join(smoke2014fd2,
#             by = join_by(region_cd, scc, poll, calc_year, scc6, emis_type),
#             suffix = c(".fd", ".fd2")) %>%
#   mutate(diff = emissions_short_tons.fd - emissions_short_tons.fd2) %>% View
#
# smoke_2014fd %>%
#   full_join(smoke2014fd2_co,
#             by = join_by(region_cd, scc, poll, calc_year, scc6, emis_type),
#             suffix = c(".fd", ".fd2co")) %>%
#   mutate(diff = emissions_short_tons.fd - emissions_short_tons.fd2co) %>% View


# # 2011
# smoke2011ek <- bind_rows(read_rds("_transportation/data-raw/epa/nei/SmokeFlatFile_MN_WI/2011ek_onroad_SMOKE_MOVES_MOVES2014a_forOTAQ_21jan2016_v2_part1.RDS"),
#                          read_rds("_transportation/data-raw/epa/nei/SmokeFlatFile_MN_WI/2011ek_onroad_SMOKE_MOVES_MOVES2014a_forOTAQ_21jan2016_v2_part2.RDS"))
#
# smoke2011el <- bind_rows(read_rds("_transportation/data-raw/epa/nei/SmokeFlatFile_MN_WI/2011el_onroad_SMOKE_MOVES_MOVES2014a_forOTAQ_31aug2016_v1_part1.RDS"),
#                          read_rds("_transportation/data-raw/epa/nei/SmokeFlatFile_MN_WI/2011el_onroad_SMOKE_MOVES_MOVES2014a_forOTAQ_31aug2016_v1_part2.RDS"))
#
# waldo::compare(
#   smoke2011ek %>% select(metadata_info) %>% unique() %>% extract2("metadata_info") %>% strsplit("#"),
#   smoke2011el %>% select(metadata_info) %>% unique() %>% extract2("metadata_info") %>% strsplit("#"))
#
#
# smoke2011ek %>%
#   full_join(smoke2011el,
#             by = join_by(region_cd, scc, poll, calc_year, scc6, emis_type),
#             suffix = c(".ek", ".el")) %>%
#   mutate(diff = emissions_short_tons.ek - emissions_short_tons.el) %>% View
