source("R/_load_pkgs.R")
source("_agriculture/data-raw/_fetch_usda_key.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")

### create county names matched to USDA format
counties <- toupper(cprg_county$county_name)
counties <- if_else(counties == "ST. CROIX", "ST CROIX", counties)


### pull in county values of fertilizer purchased as proxy for us
usda_fertilizer_mn_wi <- tidyUSDA::getQuickstat(
  sector = "ECONOMICS",
  group = "EXPENSES",
  commodity = "FERTILIZER TOTALS",
  category = "EXPENSE",
  domain = NULL,
  county = NULL,
  key = usda_key,
  program = "CENSUS",
  data_item = "FERTILIZER TOTALS, INCL LIME & SOIL CONDITIONERS - EXPENSE, MEASURED IN $",
  geographic_level = "COUNTY",
  year = as.character(2002:2022),
  state = c("MINNESOTA", "WISCONSIN"),
  geometry = TRUE,
  lower48 = TRUE,
  weighted_by_area = T
) %>%
  as.data.frame() %>%
  select(-geometry)

# usda_fertilizer_mn_wi %>%
#   filter(is.na(Value)) %>%
#   arrange(year) %>%
#   select(state_name, county_name, year)
### there are 9 missing values by county. 
### 2 MN, 2 WI in 2002. 2 MN in 2007, 3 WI in 2017.
###  None are counties of focus except Ramsey in 2002, which has limited ag.

# get proportion of county fertilizer purchase to state
usda_fert_prop <- usda_fertilizer_mn_wi %>%
  group_by(state_name, year) %>%
  mutate(state_total = sum(Value, na.rm = TRUE), fert_prop = Value / state_total) %>%
  filter(!(NAME == "Washington" & state_name == "WISCONSIN")) %>% 
  filter(county_name %in% counties) %>%
  select(year, county_name, fert_prop)

### expand grid and interpolate

fert_prop_interpolated <- left_join( # this creates an empty grid of all year county_name possibilities from 2002-2022
  expand.grid(
    year = seq(2002, 2022, by = 1),
    county_name = unique(usda_fert_prop$county_name)
  ),
  usda_fert_prop
) %>% # merged with our populated fertilizer proportion data, it creates NAs wherever data is missing
  mutate(fert_prop = if_else(year %in% c(2002, 2007, 2012, 2017, 2022) & is.na(fert_prop), 
                             0,
                             fert_prop)) %>% # add 0 as Ramsey value in 2002
  group_by(county_name) %>%
  arrange(year) %>%
  mutate(
    # this function linearly interpolates NAs between known values, 
    # following the group_by
    fert_prop = zoo::na.approx(fert_prop, na.rm = FALSE),
    # marking whether values are from the census or interpolation
    data_type = ifelse(year %in% c(2002, 2007, 2012, 2017, 2022),
                       "census", "interpolated") 
  ) %>% 
  mutate(county_name = if_else(county_name == "ST CROIX",
                               "St. Croix",
                               str_to_sentence(county_name))) %>% 
  left_join(., cprg_county %>% select(county_name, geoid)) %>% 
  select(inventory_year = year, geoid, county_name,fertilizer_proportion = fert_prop, data_type)


# create metadata
fertilizer_proportion_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "inventory_year", class(fert_prop_interpolated$inventory_year), "Year of survey",
    "geoid", class(fert_prop_interpolated$geoid), "County GEOID",
    "county_name", class(fert_prop_interpolated$county_name), "County name",
    "fertilizer_proportion", class(fert_prop_interpolated$fertilizer_proportion), "County proportion of state fertilizer sales",
    "data_type", class(fert_prop_interpolated$data_type), "Data source: interpolated is linear interpolation between census years"
  )

saveRDS(fert_prop_interpolated, "./_agriculture/data/county_fertilizer_proportion.rds")
saveRDS(fertilizer_proportion_meta, "./_agriculture/data/county_fertilizer_proportion_meta.rds")

