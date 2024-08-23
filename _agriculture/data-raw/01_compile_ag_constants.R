source("R/_load_pkgs.R")
# note that the CSV imports will be replace with more direct connection
# from the SIT tool

# check that needed data are available locally
if(!file.exists("_agriculture/data-raw/ag-module.xlsx")){
  cli::cli_abort("Download agriculture data from MS Team")
}

### going to slice and dice this below due to difficult formatting
ag_control <- readxl::read_xlsx("_agriculture/data-raw/ag-module.xlsx",
                                sheet = "Control")

### breaking constants down to categories, adding syntax friendly descriptor 
### - matched to EPA SIT short_text where possible
general_constants <-  readxl::read_xlsx("_agriculture/data-raw/ag-module.xlsx",
                                        sheet = "constants",
                                        range = "B2:C17") %>% 
  rename(value = General, description = `...2`) %>% 
  mutate(
    short_text = c(
    "MT_ton",
    "lbs_ton",
    "kg_lb",
    "kg_MT",
    "ft3_m3",
    "kg_m3",
    "kg_ton",
    "days_yr",
    "N2O_N2",
    "CH4GWP",
    "N2OGWP",
    "C_CO2",
    "lbs_hundredweight",
    "VolPercent_Indirect",
    "VolPercent"
  ),
  value = as.numeric(value)
  )


soil_plant_constants <- readxl::read_xlsx("_agriculture/data-raw/ag-module.xlsx",
                                          sheet = "constants",
                                          range = "B18:C28") %>% 
  rename(value = `Ag Soils-Plant`, description = `...2`) %>% 
  mutate(
  short_text = c(
    "NMan",
    "NOrg",
    "VolOrg",
    "VolSyn",
    "EF_Dir",
    "Vol_EF",
    "ac_ha",
    "HistEF",
    "TropHistEF",
    "N_content_legume"
  ),
  value = as.numeric(value)
  )



soil_animal_constants <- readxl::read_xlsx("_agriculture/data-raw/ag-module.xlsx",
                                           sheet = "constants",
                                           range = "G18:H28") %>% 
  rename(value = `Ag Soils-Animal`, description = `...2`) %>% 
  mutate(
  short_text = c(
    "NonVolEF",
    "prpEF",
    "LeachEF",
    "LeachEF2",
    "NH3_NOxEF",
    NA,
    "LiquidEF",
    "SolidEF",
    "nobedEF",
    "PoultryNotManaged"
  ),
  value = as.numeric(value)
  ) %>%
  filter(!is.na(value))


crop_mt_bushel <-  readxl::read_xlsx("_agriculture/data-raw/ag-module.xlsx",
                                     sheet = "constants",
                                     range = "B30:C39") %>% 
  rename(value = `...2`, description = Alfalfa) %>% 
  mutate(
  short_text = c(
    "corn_mtb",
    "wheat_mtb",
    "barley_mtb",
    "sorghum_mtb",
    "oats_mtb",
    "rye_mtb",
    "millet_mtb",
    NA,
    "soybeans_mtb"
  )
) %>%
  filter(!is.na(short_text)) %>%
  mutate(
    value = as.numeric(value),
    description = paste(description, "MT to bushels")
  )



crop_residue_mass_ratio <- read_xlsx("_agriculture/data-raw/ag-module.xlsx",
                                     sheet = "constants",
                                     range = "B29:D45") %>% 
  select(-`metric tons/bushel:`) %>% 
  rename(value = `Residue: Crop Mass Ratio`, description = Crop) %>% 
  mutate(
  short_text = c(
    "alfalfa_rcmr",
    "corn_rcmr",
    "wheat_rcmr",
    "barley_rcmr",
    "sorghum_rcmr",
    "oats_rcmr",
    "rye_rcmr",
    "millet_rcmr",
    "rice_mcmr",
    "soybeans_rcmr",
    "peanuts_rcmr",
    "beans_rcmr",
    "dry_peas_rcmr",
    "winter_peas_rcmr",
    "lentils_rcmr",
    "wrinkled_peas_rcmr"
  )
) %>%
  mutate(
    description = paste(description, "residue to crop mass ratio"),
    value = as.numeric(value)
  )

crop_residue <- ag_control[c(71, 74:89),
                           c(1, 2, 6, 10)] %>%
  row_to_names(row_number = 1) %>%
  pivot_longer(cols = 2:4, values_to = "value") %>%
  mutate(
    Crop = case_when(
      Crop == "Corn for Grain" ~ "corn",
      Crop == "All Wheat" ~ "wheat",
      Crop == "Dry Edible Beans" ~ "beans",
      Crop == "Dry Edible Peas" ~ "dry_peas",
      Crop == "Austrian Winter Peas" ~ "winter_peas",
      Crop == "Wrinkled Seed Peas" ~ "wrinkled_peas",
      TRUE ~ Crop
    ),
    short_text = case_when(
      name == "Residue Dry Matter Fraction" ~ tolower(paste0(Crop, "_rdmf")),
      name == "Fraction Residue Applied" ~ tolower(paste0(Crop, "_fra")),
      name == "Nitrogen Content of Residue" ~ tolower(paste0(Crop, "_ncr"))
    ),
    description = paste(Crop, name),
    value = as.numeric(value)
  ) %>%
  filter(!is.na(value)) %>%
  select(value, description, short_text)

animal_mass <- data.frame(
  value = ag_control[50:65, 2],
  description = ag_control[50:65, 1],
  short_text = c(
    "breeding_swine",
    "swine_under_60lbs",
    "swine_60_119_lbs",
    "swine_120_179_lbs", # KS: fixed minor typo here
    "swine_over_180lbs",
    NA, NA,
    "hens",
    "pullets",
    "chickens",
    "broilers",
    "turkeys",
    NA,
    "sheep_on_feed",
    "sheep_not_on_feed",
    "goats"
  )
) %>%
  rename(
    description = State.Inventory.Tool...Carbon.Dioxide..Methane..and.Nitrous.Oxide.Emissions.from.Agriculture.ModuleVersion.2024.1,
    value = ...2
  ) %>%
  mutate(
    description = paste(description, "Typical Animal Mass"),
    value = as.numeric(value)
  ) %>%
  filter(!is.na(value))

animal_Bo <- data.frame(  #max potential emissions (m3 CH4/kg VS)
  value = ag_control[37:65, 10],
  description = ag_control[37:65, 1],
  short_text = c(
    "Dairy Cattle",
    "Dairy Cows",
    'Dairy Replacement Heifers',
    'Beef Cattle',
    'Feedlot Heifers',
    'Feedlot Steer',
    'Bulls',
    'Calves',
    'Beef Cows',
    'Beef Replacement Heifers',
    'Steer Stockers',
    'Heifer Stockers',
    "Swine",
    "breeding_swine",
    "swine_under_60lbs",
    "swine_60_119_lbs",
    "swine_120_179_lbs", # KS: fixed minor typo here
    "swine_over_180lbs",
    NA, NA,
    "hens",
    "pullets",
    "chickens",
    "broilers",
    "turkeys",
    NA,
    "sheep_on_feed",
    "sheep_not_on_feed",
    "goats"
  )
) %>%
  rename(
    description = State.Inventory.Tool...Carbon.Dioxide..Methane..and.Nitrous.Oxide.Emissions.from.Agriculture.ModuleVersion.2024.1,
    value = ...10
  ) %>%
  mutate(
    description = paste(description, "Bo"),
    value = as.numeric(value)
  ) %>%
  filter(!is.na(value))

ag_constants_formatted <- bind_rows(
  general_constants,
  soil_plant_constants,
  soil_animal_constants,
  crop_mt_bushel,
  crop_residue_mass_ratio,
  crop_residue,
  animal_mass,
  animal_Bo
)

ag_constants_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "value", class(ag_constants_formatted$value), "Multiplier constant for agricultural emissions calculations",
    "description", class(ag_constants_formatted$description), "Description of use case for multiplier constant",
    "short_text", class(ag_constants_formatted$short_text), "Syntax friendly classifier for coding"
  )

saveRDS(ag_constants_formatted, "./_agriculture/data/ag_constants.rds")
saveRDS(ag_constants_meta, "./_agriculture/data/ag_constants_meta.rds")
