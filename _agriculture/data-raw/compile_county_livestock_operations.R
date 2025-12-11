source("R/_load_pkgs.R")
source("_agriculture/data-raw/_fetch_usda_key.R")

cprg_county <- readRDS("_meta/data/cprg_county.RDS")

### create county names matched to USDA format
counties <- toupper(cprg_county$county_name)
counties <- if_else(counties == "ST. CROIX", "ST CROIX", counties)

### load in ctu_ag_proportions to split out livestock to ctu at end
ctu_ag_proportion <- read_rds("./_agriculture/data/ctu_ag_proportion.rds")

#### Census data ####
#### census data is collected every five years (years ending in 2 and 7) and is the only inventory source data for non-cattle past 2012

usda_census <- tidyUSDA::getQuickstat(
  sector = "ANIMALS & PRODUCTS",
  group = "LIVESTOCK",
  commodity = NULL,
  category = "INVENTORY",
  domain = NULL,
  county = counties,
  key = usda_key,
  program = "CENSUS",
  data_item = NULL,
  geographic_level = "COUNTY",
  year = as.character(2002:2022),
  state = c("MINNESOTA"),
  geometry = TRUE,
  lower48 = TRUE,
  weighted_by_area = T
) %>%
  as.data.frame() %>%
  dplyr::select(-geometry)

usda_cattle_ops <- tidyUSDA::getQuickstat(
  key = usda_key,
  program = "CENSUS",
  sector = "ANIMALS & PRODUCTS",
  group = "LIVESTOCK",
  commodity = "CATTLE",
  data_item = "CATTLE, INCL CALVES - OPERATIONS WITH INVENTORY",
  geographic_level = "COUNTY",
  county = "DAKOTA",
  state = "MINNESOTA",
  year = as.character(2022),
  geometry = TRUE,
  lower48 = TRUE
) %>% as.data.frame() %>% dplyr::select(-geometry)

usda_census_ops <- usda_census %>%
  filter(
    # first avoid including non-targeted labels
    # as there is a lot of nesting and redundancy
    domain_desc == "TOTAL", !is.na(Value),
    short_desc %in% c(
      "CATTLE, (EXCL COWS) - INVENTORY",
      "CATTLE, ON FEED - INVENTORY",
      "CATTLE, COWS, BEEF - INVENTORY",
      "CATTLE, COWS, MILK - INVENTORY",
      "GOATS - INVENTORY",
      "HOGS - INVENTORY",
      "SHEEP, INCL LAMBS - INVENTORY"
    )
  ) %>%
  ## change labels to match EPA
  mutate(
    livestock_type =
      case_when(
        grepl("MILK", short_desc) ~ "Dairy Cows",
        grepl("BEEF", short_desc) ~ "Beef Cows",
        grepl("FEED", short_desc) ~ "Feedlot Cattle",
        grepl("(EXCL COWS)", short_desc) ~ "Calves",
        grepl("HOGS", short_desc) ~ "Swine",
        grepl("SHEEP", short_desc) ~ "Sheep",
        grepl("GOATS", short_desc) ~ "Goats",
        TRUE ~ short_desc
      )
  ) %>%
  group_by(year, county_name, livestock_type) %>%
  summarise(head_count = sum(Value))
