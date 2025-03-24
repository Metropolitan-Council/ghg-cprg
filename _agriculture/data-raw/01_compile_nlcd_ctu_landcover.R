## determine relative proportion of CTU agricultural land per county to allocate 
## livestock and crop activity data

source("R/_load_pkgs.R")
#source("R/cprg_colors.R")

### load in nlcd data from natural systems analysis

ag_county <- read_rds("_nature/data/nlcd_county_landcover_allyrs.rds") %>% 
  filter(land_cover_type == "Cropland")
ag_ctu <- read_rds("_nature/data/nlcd_ctu_landcover_allyrs.rds")  %>% 
  filter(land_cover_type == "Cropland")


ctu_ag_proportion <- left_join(ag_ctu %>% 
  select(ctu_id, ctu_name, ctu_class, county_name, state_name, inventory_year, ctu_ag_area = area), 
                               ag_county %>%
  select(county_name, county_ag_area = area, inventory_year),
by = c("county_name","inventory_year")
) %>%
  mutate(proportion_ag_land = ctu_ag_area / county_ag_area) %>% 
  ungroup()

# make sure proportions add to 100
ctu_ag_proportion %>% 
  group_by(inventory_year, county_name) %>% 
  summarize(total_prop = sum(proportion_ag_land)) %>% 
  ungroup() %>% 
  distinct(total_prop) # all 1, great


# create metadata
ctu_ag_proportion_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "inventory_year", class(ctu_ag_proportion$inventory_year), "Year",
    "ctu_id", class(ctu_ag_proportion$ctu_id), "Unique ID",
    "ctu_name", class(ctu_ag_proportion$ctu_name), "City, township, or unorganized territory name",
    "ctu_class", class(ctu_ag_proportion$ctu_class), "City, township, or unorganized territory",
    "county_name", class(ctu_ag_proportion$county_name), "County name",
    "state_name", class(ctu_ag_proportion$state_name), "State name",
    "ctu_ag_area", class(ctu_ag_proportion$ctu_ag_area), "Area of CTU cropland cover in square kilometers",
    "county_ag_area", class(ctu_ag_proportion$county_ag_area), "Area of county cropland cover in square kilometers",
    "proportion_ag_land", class(ctu_ag_proportion$proportion_ag_land), "Proportion of county land in city or township boundary",
  )

saveRDS(ctu_ag_proportion, "./_agriculture/data/ctu_ag_proportion.rds")
saveRDS(ctu_ag_proportion_meta, "./_agriculture/data/ctu_ag_proportion_meta.rds")
