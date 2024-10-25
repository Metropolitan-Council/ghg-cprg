source("R/_load_pkgs.R")
source("_agriculture/data-raw/_fetch_usda_key.R")

cprg_county <- readRDS("_meta/data/cprg_county.RDS")

### create county names matched to USDA format
counties <- toupper(cprg_county$county_name)
counties <- if_else(counties == "ST. CROIX", "ST CROIX", counties)


#### Survey data ####
### this is an API to get livestock data (mammals) from the USDA.
### USDA has yearly survey data that provides heads of animals -
###  only cattle appear to be available after 2013.
### USDA also has 5 year census data with more detailed information
### (2007, 2012, 2017, 2022).
### Census data will be needed for hogs, sheep, and feedlots
### Cattle survey and census data should be compared in 2017 and 2022
usda_survey <- tidyUSDA::getQuickstat(
  sector = "ANIMALS & PRODUCTS",
  group = "LIVESTOCK",
  commodity = NULL,
  category = "INVENTORY",
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
  weighted_by_area = T
)


### pull out usable data
usda_cattle <- usda_survey %>%
  filter(
    # exclude missing values, this omits Ramsey county
    # (the UofM has some cattle, but this is low importance)
    !is.na(Value),
    # only CATTLE extends to 2021 for survey data,
    # will use census data and interpolation for other livestock
    commodity_desc == "CATTLE",
    ## feedlot data also stops in 2013, so going to use census
    !grepl("FEED", short_desc)
  ) %>%
  as.data.frame() %>%
  dplyr::select(-geometry) %>% # Don't need this to be a spatial object
  # create consistent labels for use with emission calculations
  mutate(
    livestock_type =
      case_when(
        grepl("MILK", short_desc) ~ "Dairy Cows",
        grepl("BEEF", short_desc) ~ "Beef Cows",
        grepl("FEED", short_desc) ~ "Feedlot Cattle",
        grepl("CALVES", short_desc) ~ "Calves",
        grepl("HOGS", short_desc) ~ "Swine",
        grepl("SHEEP", short_desc) ~ "Sheep",
        TRUE ~ short_desc
      )
  ) %>%
  group_by(year, county_name, livestock_type) %>%
  summarise(head_count = sum(Value))

### Calf category appears to be all cattle plus calves.
### The documentation is scarce, but subtracting away adult cattle appears to be correct.
### Update this matched county generated reports, validating this approach
usda_cattle_corrected <- left_join(
  usda_cattle,
  usda_cattle %>%
    ## grab any non-calf categories (contains cows- no bull or steer data)
    filter(grepl("Cows$", livestock_type)) %>%
    group_by(year, county_name) %>%
    summarise(cows_sum = sum(head_count)),
  by = c("year", "county_name")
) %>%
  ### subtract the adults from the combined category to leave calves
  mutate(head_count = ifelse(livestock_type == "Calves",
    head_count - cows_sum, head_count
  )) %>%
  dplyr::select(-cows_sum)

# create metadata
usda_survey_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "year", class(usda_cattle_corrected$year), "Year of survey",
    "county_name", class(usda_cattle_corrected$county_name), "County name",
    "livestock_type", class(usda_cattle_corrected$livestock_type), "Livestock classification",
    "head_count", class(usda_cattle_corrected$head_count), "Number of individual (heads) of livestock type"
  )

saveRDS(usda_cattle_corrected, "./_agriculture/data/usda_cattle_survey.rds")
saveRDS(usda_survey_meta, "./_agriculture/data/usda_cattle_survey_meta.rds")
