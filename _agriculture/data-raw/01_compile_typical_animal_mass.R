source("R/_load_pkgs.R")
# note that the CSV imports will be replace with more direct connection
# from the SIT tool

# check that needed data are available locally
if(!file.exists("_agriculture/data-raw/ag-module.xlsx")){
  cli::cli_abort("Download agriculture data from MS Team")
}

ag_constants <- read_rds("_agriculture/data/ag_constants.rds")

### pull out typical animal mass by year
tam_cattle <- readxl::read_xlsx("_agriculture/data-raw/ag-module.xlsx",
                                sheet = "TAM and NEx Rates",
                                range = "A1:K39") %>% 
  slice(-1) %>% 
  pivot_longer(cols = 2:11, 
               names_to = "livestock_type", 
               values_to = "mass_kg") %>%
  rename(year = `Typical Animal Mass (Kg)`) %>%
  mutate(mass_kg = as.numeric(mass_kg)) %>% 
  filter(year >= 1990)

### pull our non-cattle weight from formatted ag_constants data

tam_other <- ag_constants %>%
  filter(grepl("Typical Animal Mass", description)) %>%
  rename(mass_kg = value) %>%
  mutate(
    livestock_type = str_replace_all(description, " Typical Animal Mass", ""),
    livestock_type = case_when(
      grepl("Swine", livestock_type) ~ "Swine",
      grepl("Market", livestock_type) ~ "Swine",
      grepl("Sheep", livestock_type) ~ "Sheep",
      grepl("Chickens", livestock_type) ~ "Layers",
      TRUE ~ livestock_type
    )
  ) %>%
  filter(!is.na(mass_kg)) %>%
  group_by(livestock_type) %>%
  summarize(mass_kg = mean(as.numeric(mass_kg)))

tam <- rows_append(
  # only need calves for K-N calc but keeping all census categories for posterity
  tam_cattle %>% filter(livestock_type %in% c("Calves",
                                              "Dairy Cows",
                                              "Beef Cows",
                                              "Steer Feedlot",
                                              "Heifer Feedlot")), 
  tam_other %>%
    crossing(year = 2005:2021)
)  ### repeat years for non-cattle (static)

  
tam_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "year", class(tam$year), "Year",
    "livestock_type", class(tam$livestock_type), "Formatted livestock classification - matches USDA census labels",
    "mass_kg", class(tam$mass_kg), "Typical animal mass in kilograms"
  )

saveRDS(tam, "./_agriculture/data/typical_animal_mass.rds")
saveRDS(tam_meta, "./_agriculture/data/typical_animal_mass_meta.rds")
