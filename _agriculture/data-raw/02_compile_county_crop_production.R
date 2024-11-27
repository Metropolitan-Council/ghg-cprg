source("R/_load_pkgs.R")
source("_agriculture/data-raw/_fetch_usda_key.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")

# formatted files
ag_constants <- readRDS("_agriculture/data/ag_constants.rds")

## convert to named vector for easier indexing
ag_mtb <- ag_constants %>%
  filter(grepl("mtb", short_text)) %>%
  mutate(crop_type = case_when(
    grepl("barley", short_text) ~ "barley",
    grepl("corn", short_text) ~ "corn",
    grepl("oats", short_text) ~ "oats",
    grepl("soybeans", short_text) ~ "soybeans",
    grepl("wheat", short_text) ~ "wheat",
    TRUE ~ short_text
  ))

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
  filter(!(NAME == "Washington" & state_name == "WISCONSIN")) %>%
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
    grepl("BARLEY", short_desc) ~ "barley",
    grepl("BEANS, DRY", short_desc) ~ "dry beans",
    grepl("CORN", short_desc) ~ "corn",
    grepl("ALFALFA", short_desc) ~ "alfalfa",
    grepl("OATS", short_desc) ~ "oats",
    grepl("SOYBEANS", short_desc) ~ "soybeans",
    grepl("WHEAT", short_desc) ~ "wheat",
    TRUE ~ short_desc
  )) %>% # convert all units to metric tons
  left_join(., ag_mtb, by = c("crop_type")) %>%
  mutate(metric_tons = case_when(
    grepl("dry beans", crop_type) ~ Value *
      ag_constants_vec["lbs_hundredweight"] *
      ag_constants_vec["kg_lb"] /
      ag_constants_vec["kg_MT"], # hundredweights to metric tons
    grepl("alfalfa", crop_type) ~ Value * ag_constants_vec["MT_ton"], # tons to metric tons
    TRUE ~ Value * value # bushels to metric tons
  )) %>%
  mutate(
    # there are three missing alfalfa values in 2013 in Dakota, Carver, and Scott, interpolating
    metric_tons = zoo::na.approx(metric_tons, na.rm = FALSE),
  ) %>%
  mutate(county_name = if_else(county_name == "ST CROIX",
    "St. Croix",
    str_to_sentence(county_name)
  )) %>%
  left_join(., cprg_county %>% select(county_name, geoid)) %>%
  select(inventory_year = year, geoid, county_name, crop_type, metric_tons)
# bring back geoid from cprg_county

# create metadata
crop_production_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "inventory_year", class(usda_survey_formatted$inventory_year), "Year of survey",
    "geoid", class(usda_survey_formatted$geoid), "County GEOID",
    "county_name", class(usda_survey_formatted$county_name), "County name",
    "crop_type ", class(usda_survey_formatted$crop_type), "Crop type",
    "metric_tons", class(usda_survey_formatted$metric_tons), "Metric tons of crop produced"
  )

saveRDS(usda_survey_formatted, "./_agriculture/data/county_crop_production.rds")
saveRDS(crop_production_meta, "./_agriculture/data/county_crop_production_meta.rds")
