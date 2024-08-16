source("R/_load_pkgs.R")
source("R/global_warming_potential.R")
source("_agriculture/data-raw/_fetch_usda_key.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")

# formatted files
ag_constants <- readRDS("_agriculture/data/ag_constants.rds")

## convert to named vector for easier indexing
ag_constants_vec <- ag_constants %>%
  select(short_text, value) %>%
  tibble::deframe()

### create county names matched to USDA format
counties <- toupper(cprg_county$county_name)
counties <- if_else(counties == "ST. CROIX", "ST CROIX", counties)


usda_survey <- tidyUSDA::getQuickstat(
  sector = "CROPS",
  group = "FIELD CROPS",
  commodity = NULL,
  category = "PRODUCTION",
  domain = NULL,
  county = counties,
  key = usda_key,
  program = "SURVEY",
  data_item = NULL,
  geographic_level = "COUNTY",
  year = as.character(2005:2021),
  state = c("MINNESOTA", "WISCONSIN"),
  geometry = TRUE,
  lower48 = TRUE,
  weighted_by_area = TRUE
) %>%
  as.data.frame() %>% 
  st_drop_geometry()


usda_survey_formatted <- usda_survey %>%
  filter(short_desc %in% c(
    "BARLEY - PRODUCTION, MEASURED IN BU",
    "BEANS, DRY EDIBLE, INCL CHICKPEAS - PRODUCTION, MEASURED IN CWT",
    "CORN, GRAIN - PRODUCTION, MEASURED IN BU",
    "HAY, ALFALFA - PRODUCTION, MEASURED IN TONS",
    "OATS - PRODUCTION, MEASURED IN BU",
    "SOYBEANS - PRODUCTION, MEASURED IN BU",
    "WHEAT - PRODUCTION, MEASURED IN BU"
  )) %>%
  select(year, county_name, short_desc, Value) %>%
  ### convert names to match ag conversions
  mutate(crop_type = case_when(
    grepl("BARLEY", short_desc) ~ "Barley",
    grepl("BEANS", short_desc) ~ "Dry Edible Beans",
    grepl("CORN", short_desc) ~ "Corn for Grain",
    grepl("ALFALFA", short_desc) ~ "Alfalfa",
    grepl("OATS", short_desc) ~ "Oats",
    grepl("SOYBEANS", short_desc) ~ "Soybeans",
    grepl("WHEAT", short_desc) ~ "All Wheat",
    TRUE ~ short_desc
  )) %>% # convert all units to metric tons
  left_join(., crop_conversions, by = c("crop_type" = "crop")) %>%
  mutate(metric_tons = case_when(
    grepl("Beans", crop_type) ~ Value * .0454, # hundredweights to metric tons
    grepl("Alfalfa", crop_type) ~ Value * 0.9072, # tons to metric tons
    TRUE ~ Value * metric_tons_bushel # bushels to metric tons
  )) %>% 
  mutate(
    # determine N delivered to soils
    MT_N_to_soil = if_else(
      crop_type == "Alfalfa",
      0, # no N to soil via residue for alfalfa
      metric_tons * residue_crop_mass_ratio * residue_dry_matter_fraction * fraction_residue_applied * nitrogen_content_of_residue
    ),
    MT_N_fixation = if_else(
      crop_type %in% c("Alfalfa", "Soybeans", "Dry Edible Beans"),
      metric_tons * (1 + residue_crop_mass_ratio) * residue_dry_matter_fraction * 0.03,
      # last value is constant of N content of N-fixer biomass
      0
    )
  )

