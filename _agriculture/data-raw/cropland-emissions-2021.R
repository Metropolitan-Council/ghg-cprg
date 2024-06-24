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


### reformat constants

ag_constants

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
  weighted_by_area = T)


unique(usda_survey$short_desc)
