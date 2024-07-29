source("R/_load_pkgs.R")
# note that the CSV imports will be replace with more direct connection
# from the SIT tool

# check that needed data are available locally
if(!file.exists("_agriculture/data-raw/ag-module.xlsx")){
  cli::cli_abort("Download agriculture data from MS Team")
}

if(!file.exists("_agriculture/data/typical_animal_mass.rds")){
  cli::cli_abort("Run _agriculture/data-raw/tam_and_nex_formatting.R script first")
}

tam <- read_rds("_agriculture/data/typical_animal_mass.rds")

# enteric fermentation rates - 
# not all of these are needed, just loading in livestock in usda census
# rates is kg CH4 / head 
# The data sheet says per day, but this is never scaled up by 365 anywhere, 
# and would result in huge numbers, so probably an error
vs_dairy_cows <- readxl::read_xlsx("_agriculture/data-raw/ag-module.xlsx",
                               sheet = "VS-CattleNEW",
                               range = "A3:AK53") %>% 
  rename(state = `Dairy Cows`) %>% 
  filter(state %in% c("Minnesota", "Wisconsin")) %>% 
  pivot_longer(cols = -1, names_to = "year", values_to = "kg_vs_head_yr") %>% 
  mutate(mt_vs_head_yr = kg_vs_head_yr  / 1000,
         livestock_type = "Dairy Cows",
         year = as.numeric(year))

vs_dairy_calves <- readxl::read_xlsx("_agriculture/data-raw/ag-module.xlsx",
                                   sheet = "VS-CattleNEW",
                                   range = "A55:AK105") %>% 
  rename(state = `Dairy Replacements`) %>% 
  filter(state %in% c("Minnesota", "Wisconsin")) %>% 
  pivot_longer(cols = -1, names_to = "year", values_to = "kg_vs_head_yr") %>% 
  mutate(mt_vs_head_yr = kg_vs_head_yr  / 1000,
         livestock_type = "Calves",
         year = as.numeric(year))

vs_beef_cows <- readxl::read_xlsx("_agriculture/data-raw/ag-module.xlsx",
                                         sheet = "VS-CattleNEW",
                                         range = "A107:AK157") %>% 
  rename(state = `Beef Cows`) %>% 
  filter(state %in% c("Minnesota", "Wisconsin")) %>% 
  pivot_longer(cols = -1, names_to = "year", values_to = "kg_vs_head_yr") %>% 
  mutate(mt_vs_head_yr = kg_vs_head_yr  / 1000,
         livestock_type = "Beef Cows",
         year = as.numeric(year))

vs_beef_calves <- readxl::read_xlsx("_agriculture/data-raw/ag-module.xlsx",
                                          sheet = "VS-CattleNEW",
                                          range = "A159:AK209") %>% 
  rename(state = `Beef Replacements`) %>% 
  filter(state %in% c("Minnesota", "Wisconsin")) %>% 
  pivot_longer(cols = -1, names_to = "year", values_to = "kg_vs_head_yr") %>% 
  mutate(mt_vs_head_yr = kg_vs_head_yr  / 1000,
         livestock_type = "Calves",
         year = as.numeric(year))

vs_feedlot_steer <- readxl::read_xlsx("_agriculture/data-raw/ag-module.xlsx",
                                         sheet = "VS-CattleNEW",
                                         range = "A315:AK365") %>% 
  rename(state = `Steer Feedlot`) %>% 
  filter(state %in% c("Minnesota", "Wisconsin")) %>% 
  pivot_longer(cols = -1, names_to = "year", values_to = "kg_vs_head_yr") %>% 
  mutate(mt_vs_head_yr = kg_vs_head_yr / 1000,
         livestock_type = "Feedlot Cattle",
         year = as.numeric(year))

vs_feedlot_heifer <- readxl::read_xlsx("_agriculture/data-raw/ag-module.xlsx",
                                           sheet = "VS-CattleNEW",
                                           range = "A367:AK417") %>% 
  rename(state = `Heifer Feedlot`) %>% 
  filter(state %in% c("Minnesota", "Wisconsin")) %>% 
  pivot_longer(cols = -1, names_to = "year", values_to = "kg_vs_head_yr") %>% 
  mutate(mt_vs_head_yr = kg_vs_head_yr / 1000,
         livestock_type = "Feedlot Cattle",
         year = as.numeric(year))

# constants for sheep, goats, swine, horses in control data sheet
vs_other <- readxl::read_xlsx("_agriculture/data-raw/ag-module.xlsx",
                              "VS-CattleNEW",
                              range = "AO3:BE39") %>% 
  pivot_longer(cols = -1, names_to = "livestock_type_temp", values_to = "kg_vs_1000kg_animal") %>% 
  mutate(mt_vs_1000kg_animal = kg_vs_1000kg_animal / 1000,
         year = as.numeric(Year),
         livestock_type = case_when(
           grepl("calf_nof", livestock_type_temp) ~ "Calves",
           grepl("goats", livestock_type_temp) ~ "Goats",
           grepl("horses", livestock_type_temp) ~ "Horses",
           grepl("poultry_broilers", livestock_type_temp) ~ "Broilers",
           grepl("poultry_layers", livestock_type_temp) ~ "Layers",
           grepl("poultry_pullets", livestock_type_temp) ~ "Pullets",
           grepl("poultry_turkeys", livestock_type_temp) ~ "Turkeys",
           grepl("swine", livestock_type_temp) ~ "Swine",
           grepl("sheep", livestock_type_temp) ~ "Sheep",
           TRUE ~ NA
         )) %>% 
  left_join(., tam, by = c("year" = "year", "livestock_type" = "livestock_type")) %>%
  mutate(mt_vs_head_yr = (mass_kg / 1000) * mt_vs_1000kg_animal * 365) %>% 
  filter(!is.na(mass_kg)) %>%
  crossing(state = c("Minnesota", "Wisconsin")) %>% 
  select(-c(kg_vs_1000kg_animal , livestock_type_temp, Year, mt_vs_1000kg_animal))

vs_livestock <-  bind_rows(vs_dairy_cows,
                          vs_dairy_calves,
                          vs_beef_cows,
                          vs_beef_calves,
                          vs_feedlot_steer,
                          vs_feedlot_heifer,
                          vs_other) %>% 
  group_by(state, year, livestock_type) %>% 
  summarize(mt_vs_head_yr = mean(mt_vs_head_yr))


vs_livestock_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "state", class(vs_livestock$state), "State",
    "year", class(vs_livestock$year), "Year",
    "livestock_type", class(vs_livestock$livestock_type), "Formatted livestock classification - matches USDA census labels",
    "mt_vs_head_yr", class(vs_livestock$mt_vs_head_yr), "Metric tons of volatile solids produced per animal per year"
  )

saveRDS(vs_livestock, "./_agriculture/data/volatile_solids.rds")
saveRDS(vs_livestock_meta, "./_agriculture/data/volatile_solids_meta.rds")
