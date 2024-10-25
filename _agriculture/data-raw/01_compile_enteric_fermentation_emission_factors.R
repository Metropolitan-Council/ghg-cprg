source("R/_load_pkgs.R")
# note that the CSV imports will be replace with more direct connection
# from the SIT tool

# check that needed data are available locally
if (!file.exists("_agriculture/data-raw/ag-module.xlsx")) {
  cli::cli_abort("Download agriculture data from MS Team")
}

# enteric fermentation rates -
# not all of these are needed, just loading in livestock in usda census
# rates is kg CH4 / head
# The data sheet says per day, but this is never scaled up by 365 anywhere,
# and would result in huge numbers, so probably an error
enteric_dairy_cows <- readxl::read_xlsx("_agriculture/data-raw/ag-module.xlsx",
  sheet = "enteric EFsNEW",
  range = "A3:AK53"
) %>%
  rename(state = `Dairy Cows`) %>%
  filter(state %in% c("Minnesota", "Wisconsin")) %>%
  pivot_longer(cols = -1, names_to = "year", values_to = "kgch4_head_yr") %>%
  mutate(
    mt_ch4_head_yr = kgch4_head_yr / 1000,
    livestock_type = "Dairy Cows",
    year = as.numeric(year)
  )

enteric_dairy_calves <- readxl::read_xlsx("_agriculture/data-raw/ag-module.xlsx",
  sheet = "enteric EFsNEW",
  range = "A159:AK209"
) %>%
  rename(state = `Dairy Replacements Total`) %>%
  filter(state %in% c("Minnesota", "Wisconsin")) %>%
  pivot_longer(cols = -1, names_to = "year", values_to = "kgch4_head_yr") %>%
  mutate(
    mt_ch4_head_yr = kgch4_head_yr / 1000,
    livestock_type = "Calves",
    year = as.numeric(year)
  )

enteric_beef_cows <- readxl::read_xlsx("_agriculture/data-raw/ag-module.xlsx",
  sheet = "enteric EFsNEW",
  range = "A211:AK261"
) %>%
  rename(state = `Beef Cows`) %>%
  filter(state %in% c("Minnesota", "Wisconsin")) %>%
  pivot_longer(cols = -1, names_to = "year", values_to = "kgch4_head_yr") %>%
  mutate(
    mt_ch4_head_yr = kgch4_head_yr / 1000,
    livestock_type = "Beef Cows",
    year = as.numeric(year)
  )

enteric_beef_calves <- readxl::read_xlsx("_agriculture/data-raw/ag-module.xlsx",
  sheet = "enteric EFsNEW",
  range = "A367:AK417"
) %>%
  rename(state = `Beef Replacements Total`) %>%
  filter(state %in% c("Minnesota", "Wisconsin")) %>%
  pivot_longer(cols = -1, names_to = "year", values_to = "kgch4_head_yr") %>%
  mutate(
    mt_ch4_head_yr = kgch4_head_yr / 1000,
    livestock_type = "Calves",
    year = as.numeric(year)
  )

enteric_feedlot_steer <- readxl::read_xlsx("_agriculture/data-raw/ag-module.xlsx",
  sheet = "enteric EFsNEW",
  range = "A523:AK573"
) %>%
  rename(state = `Steer Feedlot`) %>%
  filter(state %in% c("Minnesota", "Wisconsin")) %>%
  pivot_longer(cols = -1, names_to = "year", values_to = "kgch4_head_yr") %>%
  mutate(
    mt_ch4_head_yr = kgch4_head_yr / 1000,
    livestock_type = "Feedlot Cattle",
    year = as.numeric(year)
  )

enteric_feedlot_heifer <- readxl::read_xlsx("_agriculture/data-raw/ag-module.xlsx",
  sheet = "enteric EFsNEW",
  range = "A575:AK625"
) %>%
  rename(state = `Heifer Feedlot`) %>%
  filter(state %in% c("Minnesota", "Wisconsin")) %>%
  pivot_longer(cols = -1, names_to = "year", values_to = "kgch4_head_yr") %>%
  mutate(
    mt_ch4_head_yr = kgch4_head_yr / 1000,
    livestock_type = "Feedlot Cattle",
    year = as.numeric(year)
  )

# constants for sheep, goats, swine, horses in control data sheet
enteric_other <- readxl::read_xlsx("_agriculture/data-raw/ag-module.xlsx",
  sheet = "Control",
  range = "A26:B30"
) %>%
  rename(
    livestock_type = `Other`,
    kgch4_head_yr = `...2`
  ) %>%
  mutate(mt_ch4_head_yr = kgch4_head_yr / 1000) %>%
  crossing(
    year = 2005:2021,
    state = c("Minnesota", "Wisconsin")
  )

enteric_out <- bind_rows(
  enteric_dairy_cows,
  enteric_dairy_calves,
  enteric_beef_cows,
  enteric_beef_calves,
  enteric_feedlot_steer,
  enteric_feedlot_heifer,
  enteric_other
) %>%
  group_by(state, year, livestock_type) %>%
  summarize(mt_ch4_head_yr = mean(mt_ch4_head_yr))


enteric_out_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "state", class(enteric_out$state), "State",
    "year", class(enteric_out$year), "Year",
    "livestock_type", class(enteric_out$livestock_type), "Formatted livestock classification - matches USDA census labels",
    "mt_ch4_head_yr", class(enteric_out$mt_ch4_head_yr), "Metric tons of CH4 emitted per animal per year"
  )

saveRDS(enteric_out, "./_agriculture/data/enteric_fermentation_emission_factors.rds")
saveRDS(enteric_out_meta, "./_agriculture/data/enteric_fermentation_emission_factors_meta.rds")
