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
  state = c("MINNESOTA", "WISCONSIN"),
  geometry = TRUE,
  lower48 = TRUE,
  weighted_by_area = T
) %>%
  as.data.frame() %>%
  filter(!(NAME == "Washington" & state_name == "WISCONSIN")) %>%
  dplyr::select(-geometry)


### rename and aggregate variables from short_dec to matchable
### labels with EPA emissions data
usda_census_agg <- usda_census %>%
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

### subtract feedlot calves from total calves
usda_census_corrected <- left_join(usda_census_agg,
  usda_census_agg %>%
    filter(grepl("Feedlot", livestock_type)) %>%
    group_by(year, county_name) %>%
    summarise(feedlot_sum = sum(head_count)),
  by = c("year", "county_name")
) %>%
  mutate(head_count = ifelse(livestock_type == "Calves", head_count - feedlot_sum, head_count)) %>%
  dplyr::select(-feedlot_sum)

### interpolate between census years for all animal types
census_interpolated <- left_join(
  # this creates an empty grid of all desired year,livestock combinations
  expand.grid(
    # merged with our populated census data, it creates NAs wherever data is missing
    year = seq(2002, 2022, by = 1),
    county_name = unique(usda_census_corrected$county_name),
    livestock_type = unique(usda_census_corrected$livestock_type)
  ),
  usda_census_corrected
) %>%
  # override census years to be 0 if livestock-county combo is missing
  mutate(head_count = if_else(year %in% c(2002, 2007, 2012, 2017, 2022) &
    is.na(head_count), 0, head_count)) %>%
  group_by(county_name, livestock_type) %>%
  arrange(year) %>%
  mutate(
    # this function linearly interpolates NAs between known values,
    # following the group_by
    head_count = zoo::na.approx(head_count, na.rm = FALSE),
    # marking whether values are from the census or interpolation
    data_type = ifelse(year %in% c(2002, 2007, 2012, 2017, 2022), "census", "interpolated")
  )


#### Poultry ####
#### repeat process for poultry

usda_poultry <- tidyUSDA::getQuickstat(
  sector = "ANIMALS & PRODUCTS",
  group = "POULTRY",
  commodity = NULL,
  category = "INVENTORY",
  domain = NULL,
  county = counties,
  key = usda_key,
  program = "CENSUS",
  data_item = NULL,
  geographic_level = "COUNTY",
  year = as.character(2002:2022),
  state = c("MINNESOTA", "WISCONSIN"),
  geometry = TRUE,
  lower48 = TRUE,
  weighted_by_area = T
) %>%
  as.data.frame() %>%
  filter(!(NAME == "Washington" & state_name == "WISCONSIN")) %>%
  dplyr::select(-geometry)

### rename and aggregate
usda_poultry_agg <- usda_poultry %>%
  filter(
    domain_desc == "TOTAL", !is.na(Value),
    short_desc %in% c(
      "CHICKENS, BROILERS - INVENTORY",
      "CHICKENS, LAYERS - INVENTORY",
      "CHICKENS, PULLETS, REPLACEMENT - INVENTORY",
      "CHICKENS, ROOSTERS - INVENTORY",
      "TURKEYS - INVENTORY"
    )
  ) %>%
  mutate(
    livestock_type =
      case_when(
        grepl("BROILERS", short_desc) ~ "Broilers",
        grepl("LAYERS", short_desc) ~ "Layers",
        grepl("PULLETS", short_desc) ~ "Pullets",
        grepl("ROOSTERS", short_desc) ~ "Broilers",
        grepl("TURKEYS", short_desc) ~ "Turkeys",
        TRUE ~ short_desc
      )
  ) %>%
  group_by(year, county_name, livestock_type) %>%
  summarise(head_count = sum(Value))


### interpolate between census years for all animal types
### of note is that poultry farms in the region are often sparse enough that USDA doesn't report data to protect individual farmer data
### there are two spikes of poultry counts (Turkeys in Dakota in 2007 and Broilers in Sherburne in 2017)
### whether these are real spikes, data errors, or surrounding years are masked due to the data privacy issue is unclear, but these spikes have
### no discernible impact on the emission totals for these counties as cattle dominate animal emissions

poultry_interpolated <- left_join( # this creates an empty grid of all desired year,livestock combinations
  expand.grid(
    year = seq(2002, 2022, by = 1),
    county_name = unique(usda_poultry_agg$county_name),
    livestock_type = unique(usda_poultry_agg$livestock_type)
  ),
  usda_poultry_agg
) %>% # merged with our populated census data, it creates NAs wherever data is missing
  mutate(head_count = if_else(year %in% c(2002, 2007, 2012, 2017, 2022) & is.na(head_count), 0, head_count)) %>% # override census years to be 0 if livestock-county combo is missing
  group_by(county_name, livestock_type) %>%
  arrange(year) %>%
  mutate(
    head_count = zoo::na.approx(head_count, na.rm = FALSE), # this function linearly interpolates NAs between known values, following the group_by
    data_type = ifelse(year %in% c(2002, 2007, 2012, 2017, 2022), "census", "interpolated") # marking whether values are from the census or interpolation
  )


## stack mammals and poultry

livestock_interpolated <- bind_rows(census_interpolated, poultry_interpolated) %>%
  mutate(county_name = if_else(county_name == "ST CROIX", "St. Croix", str_to_sentence(county_name))) # homogenize county case to other data


# create metadata
livestock_census_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "year", class(livestock_interpolated$year), "Year of survey",
    "county_name", class(livestock_interpolated$county_name), "County name",
    "livestock_type", class(livestock_interpolated$livestock_type), "Livestock classification",
    "head_count", class(livestock_interpolated$head_count), "Number of individual (heads) of livestock type",
    "data_type", class(livestock_interpolated$data_type), "Data source: interpolated is linear interpolation between census years",
  )

saveRDS(livestock_interpolated, "./_agriculture/data/usda_census_data.rds")
saveRDS(livestock_census_meta, "./_agriculture/data/usda_census_data_meta.rds")

### assigning livestock to TOWNSHIPs only. Assuming livestock within city limits is rare
township_ag_proportion <- ctu_ag_proportion %>%
  filter(grepl("TOWN",ctu_class) |
           (county_name == "Hennepin" & ctu_ag_area > 10)) %>% #no townships in Hennepin, making minimum ag area for now 
  group_by(county_name, inventory_year) %>%
  mutate(
    total_rural_ag_area = sum(ctu_ag_area, na.rm = TRUE),
    proportion_ag_land = ctu_ag_area / total_rural_ag_area
  ) %>%
  select(-total_rural_ag_area) %>%
  ungroup()

livestock_township <- left_join(township_ag_proportion,
                                livestock_interpolated,
                                by = c("county_name",
                                       "inventory_year" = "year")) %>% 
  mutate(township_head_count = proportion_ag_land * head_count) %>% 
  filter(!is.na(township_head_count)) %>% 
  select(ctu_id, ctu_name, ctu_class, county_name, state_name,
         inventory_year, livestock_type, township_head_count, data_type)


saveRDS(livestock_township, "./_agriculture/data/township_usda_census_data.rds")
