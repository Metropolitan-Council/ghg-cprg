source("R/_load_pkgs.R")
# note that the CSV imports will be replace with more direct connection
# from the SIT tool

# check that needed data are available locally
if (!file.exists("_agriculture/data-raw/ag_constants.csv")) {
  cli::cli_abort("Download agriculture data from MS Team")
}

# emission factors and constants are located in two places
ag_constants <- read_csv("_agriculture/data-raw/ag_constants.csv")
ag_control <- read_csv("_agriculture/data-raw/ag_control.csv")

### breaking constants down to categories, adding syntax friendly descriptor
### - matched to EPA SIT short_text where possible
general_constants <- data.frame(
  value = ag_constants[2:16, 1],
  description = ag_constants[2:16, 2],
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
  )
) %>%
  select(
    value = Constants,
    description = `...2`,
    short_text
  ) %>%
  mutate(value = as.numeric(value))


soil_plant_constants <- data.frame(
  value = ag_constants[18:27, 1],
  description = ag_constants[18:27, 2],
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
  )
) %>%
  select(
    value = Constants,
    description = `...2`,
    short_text
  ) %>%
  mutate(value = as.numeric(value))


soil_animal_constants <- data.frame(
  value = ag_constants[18:27, 6],
  description = ag_constants[18:27, 7],
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
  )
) %>%
  rename(value = ...6, description = ...7) %>%
  filter(!is.na(value)) %>%
  mutate(value = if_else(short_text == "PoultryNotManaged",
    as.numeric(str_remove(value, "%")) / 100,
    as.numeric(value)
  ))


crop_mt_bushel <- data.frame(
  value = ag_constants[30:38, 2],
  description = ag_constants[30:38, 1],
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
  rename(value = ...2, description = Constants) %>%
  mutate(
    value = as.numeric(value),
    description = paste(description, "MT to bushels")
  )



crop_residue_mass_ratio <- data.frame(
  value = ag_constants[29:44, c(3)],
  descriptor = ag_constants[29:44, c(1)],
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
  rename(value = ...3, description = Constants) %>%
  mutate(
    description = paste(description, "residue to crop mass ratio"),
    value = as.numeric(value)
  )

crop_residue <- ag_control[
  c(71, 74:89),
  c(1, 2, 6, 10)
] %>%
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
    description = State.Inventory.Tool...Carbon.Dioxide..Methane..and.Nitrous.Oxide.Emissions.from.Agriculture.Module.Version.2024.1,
    value = ...2
  ) %>%
  mutate(
    description = paste(description, "Typical Animal Mass"),
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
  animal_mass
)

ag_constants_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "value", class(ag_constants_formatted$value), "Multiplier constant for agricultural emissions calculations",
    "description", class(ag_constants_formatted$description), "Description of use case for multiplier constant",
    "short_text", class(ag_constants_formatted$short_text), "Syntax friendly classifier for coding"
  )

saveRDS(ag_constants_formatted, "./_agriculture/data/ag_constants_formatted.rds")
saveRDS(ag_constants_meta, "./_agriculture/data/ag_constants_formatted.rds_meta.rds")
