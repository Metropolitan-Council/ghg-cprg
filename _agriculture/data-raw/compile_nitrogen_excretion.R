source("R/_load_pkgs.R")
# note that the CSV imports will be replace with more direct connection
# from the SIT tool

# check that needed data are available locally
if(!file.exists("_agriculture/data-raw/ag-module.xlsx")){
  cli::cli_abort("Download agriculture data from MS Team")
}

tam <- read_rds("_agriculture/data/typical_animal_mass.rds")


# pull out cattle nitrogen excreted by head per year - mn and wi
nex_cattle <- readxl::read_xlsx("_agriculture/data-raw/ag-module.xlsx",
                                sheet = "TAM and NEx Rates",
                                range = "N2:AY460") %>% 
  pivot_longer(cols = 3:38, names_to = "year", 
               values_to = "kg_nex_head_yr") %>%
  filter(state %in% c("MN", "WI")) %>%
  mutate(livestock_type = case_when(
    grepl("_OF_", Animal) ~ "Feedlot Cattle",
    grepl("Dairy_Cow", Animal) ~ "Dairy Cows",
    grepl("Beef_NOF", Animal) ~ "Beef Cows",
    TRUE ~ Animal
  )) %>%
  filter(livestock_type %in% c("Feedlot Cattle", "Beef Cows", "Dairy Cows")) %>%
  group_by(state, year, livestock_type) %>%
  summarize(kg_nex_head_yr = mean(kg_nex_head_yr)) %>%
  mutate(year = as.numeric(year))

# pull out other livestock nitrogen excreted by head per year - mn and wi. note these are in per day and per kg animal, so bring in TAM at end to calc
nex_other <-  readxl::read_xlsx("_agriculture/data-raw/ag-module.xlsx",
                                sheet = "TAM and NEx Rates",
                                range = "BA2:BQ38") %>% 
  pivot_longer(cols = 2:17, names_to = "livestock_type", 
               values_to = "kg_nex_day_kg_animal") %>%
  mutate(
    livestock_type = case_when(
      grepl("calf", livestock_type) ~ "Calves",
      grepl("goats", livestock_type) ~ "Goats",
      grepl("swine", livestock_type) ~ "Swine",
      grepl("sheep", livestock_type) ~ "Sheep",
      grepl("broilers", livestock_type) ~ "Broilers",
      grepl("layers", livestock_type) ~ "Layers",
      grepl("pullets", livestock_type) ~ "Pullets",
      grepl("turkeys", livestock_type) ~ "Turkeys",
      TRUE ~ livestock_type
    ),
    year = as.numeric(Year),
    kg_nex_day_kg_animal = as.numeric(kg_nex_day_kg_animal)
  ) %>%
  filter(Year >= 2005) %>%
  left_join(., tam, by = c("year" = "year", "livestock_type" = "livestock_type")) %>%
  mutate(kg_nex_head_yr = mass_kg / 1000 * kg_nex_day_kg_animal * 365) %>%
  filter(!is.na(mass_kg)) %>%
  crossing(state = c("MN", "WI")) %>% # repeat across states (static)
  dplyr::select(year, livestock_type, state, kg_nex_head_yr)

nex_formatted <- rows_append(nex_cattle, nex_other)

nex_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "state", class(nex_formatted), "State",
    "year", class(nex_formatted$year), "Year",
    "livestock_type", class(nex_formatted$livestock_type), "Formatted livestock classification - matches USDA census labels",
    "kg_nex_head_yr", class(nex_formatted$kg_nex_head_yr), "Kilograms of nitrogen excreted per animal per year"
  )

saveRDS(nex_formatted, "./_agriculture/data/nitrogen_excretion.rds")
saveRDS(nex_meta, "./_agriculture/data/nitrogen_excretion_meta.rds")
