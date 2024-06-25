source("R/_load_pkgs.R")
source("R/global_warming_potential.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")

## you'll need an API key to use USDA data (https://quickstats.nass.usda.gov/api)
key <- keyring::key_get('usda_key')

ag_soils_residuals <- read_csv('_agriculture/data-raw/ag_soils_residuals.csv')
ag_soils_fertilizer <- read_csv('_agriculture/data-raw/ag_soils_fertilizer.csv')
ag_soils_manure <- read_csv('_agriculture/data-raw/ag_soils_manure.csv')
ag_residual_burning_ch4 <- read_csv('_agriculture/data-raw/ag_residual_burning_ch4.csv')
ag_residual_burning_n2o <- read_csv('_agriculture/data-raw/ag_residual_burning_n2o.csv')
ag_soils_liming <- read_csv('_agriculture/data-raw/ag_soils_liming.csv')
ag_constants <- read_csv('_agriculture/data-raw/ag_constants.csv')
ag_control <- read_csv('_agriculture/data-raw/ag_control.csv')



### create county names matched to USDA format
counties <- toupper(cprg_county$NAME)
counties <- if_else(counties == 'ST. CROIX', "ST CROIX", counties)


#starting with biggest source of emissions - residuals

usda_survey <- tidyUSDA::getQuickstat(
  sector="CROPS",
  group="FIELD CROPS",
  commodity=NULL,
  category= "PRODUCTION",
  domain= NULL,
  county= counties,
  key = key,
  program = "SURVEY",
  data_item = NULL,
  geographic_level = 'COUNTY',
  year = as.character(2005:2021),
  state = c("MINNESOTA","WISCONSIN"),
  geometry = TRUE,
  lower48 = TRUE, 
  weighted_by_area = T) %>% 
  as.data.frame() %>% select(-geometry)


unique(usda_survey$short_desc)

usda_survey %>% filter(grepl("HAYLAGE ",short_desc))
usda_survey %>% filter(grepl("HAY",short_desc), county_name == "PIERCE") %>% select(year, short_desc, Value) %>% arrange(year)

### extracting crops with conversions from EPA tool (corn and soy will likely account for >90% of emissions)

crop_conversions <- left_join(ag_constants[28:44,1:3] %>% 
  row_to_names(row_number = 1),
  ag_control[c(71,74:89), c(1,2,6,10)] %>% 
    row_to_names(row_number = 1)) %>% 
  mutate_at(c(2:6), as.numeric) %>% 
  clean_names()


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
  select(year,county_name, short_desc, Value) %>% 
### convert names to match ag conversions
  mutate(crop_type =  case_when(
    grepl("BARLEY", short_desc) ~ "Barley",
    grepl("BEANS", short_desc) ~ "Dry Edible Beans",
    grepl("CORN", short_desc) ~ "Corn for Grain",
    grepl("ALFALFA",short_desc) ~ "Alfalfa",
    grepl("OATS", short_desc) ~ "Oats",
    grepl("SOYBEANS",short_desc) ~ "Soybeans",
    grepl("WHEAT",short_desc) ~ "All Wheat",
    TRUE ~ short_desc
  )) %>% #convert all units to metric tons
  left_join(.,crop_conversions, by = c("crop_type" = "crop")) %>% 
  mutate(metric_tons = case_when(
    grepl("Beans", crop_type) ~ Value * 45.4, # 1000s of hundredweights to metric tons
    grepl("Alfalfa", crop_type) ~ Value * 1000 * 0.9072, # 1000s of tons to metric tons
    TRUE ~ Value * 1000 * metric_tons_bushel # 1000s of bushels to metric tons
  )
         ) %>% # determine N delivered to soils
  mutate(MT_N_to_soil = if_else(
    crop_type == "Alfalfa", 
    0,  # no N to soil via residue for alfalfa
    metric_tons * residue_crop_mass_ratio  * residue_dry_matter_fraction * fraction_residue_applied * nitrogen_content_of_residue 
  ),
  MT_N_fixation = if_else(
    crop_type %in% c("Alfalfa", "Soybeans", "Dry Edible Beans"),
    metric_tons * (1 + residue_crop_mass_ratio) * residue_dry_matter_fraction * 0.03, # last value is constant of N content of N-fixer biomass
    0
    ))

soil_residue_emissions <- usda_survey_formatted %>% 
  filter(!is.na(MT_N_fixation)) %>% 
  group_by(county_name, year) %>% 
  summarize(mt_n_soils = sum(MT_N_to_soil + MT_N_fixation)) %>% 
  mutate(mt_n2o = mt_n_soils * 0.1571428,
         mt_c2oe = mt_n2o * gwp$n2o)

soil_residue_emissions %>% filter(year == 2021) %>% pull(mt_c2oe) %>% sum()
# 4500561915 - so that's not right...
