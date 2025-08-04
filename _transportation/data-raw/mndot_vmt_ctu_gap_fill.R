# using population, centerline miles, or another method
# attribute the remainder of county level VMT to 
# CTUs that don't have CTU level VMT data

source("R/_load_pkgs.R")
# source("_transportation/data-raw/mndot_vmt_ctu.R")
mndot_vmt_spatial <- readRDS("_transportation/data-raw/mndot/mndot_vmt_ctu_spatial.RDS")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
source("_transportation/data-raw/mndot_vmt_ctu.R")

ctu_unreliable <- mndot_vmt_spatial %>% 
  filter(reliable == FALSE)

ctu_population <- readRDS("_meta/data/ctu_population.RDS") %>%
  left_join(cprg_county %>% sf::st_drop_geometry()) %>%
  mutate(ctu_name_full = paste0(ctu_name, ", ", ctu_class),
         ctu_name_full_county = paste0(ctu_name_full, ", ", county_name ))


cprg_ctu <- readRDS("_meta/data/cprg_ctu.RDS") %>%
  left_join(cprg_county %>% sf::st_drop_geometry()) %>% 
  mutate(ctu_name_full = paste0(ctu_name, ", ", ctu_class)) %>% 
  left_join(ctu_population %>% 
              select(geoid, gnis, coctu_id_fips, coctu_id_gnis) %>% 
              unique(),
            by = c("gnis", "geoid"))

mndot_vmt_ctu <- readRDS("_transportation/data/mndot_vmt_ctu.RDS") %>% 
  left_join(cprg_county %>% sf::st_drop_geometry()) %>% 
  unique()

mndot_vmt_county <- readRDS("_transportation/data/dot_vmt.RDS") %>% 
  filter(data_source == "MnDOT")



multi_co_ctus <- cprg_ctu %>% 
  sf::st_drop_geometry() %>% 
  group_by(ctu_name_full, ctu_name) %>% 
  count() %>% 
  filter(n >= 2)


# 
# mndot_vmt_ctu %>% 
#   left_join(ctu_segment_summary)



mndot_vmt_ctu %>% 
  filter(ctu_name %in% multi_co_ctus$ctu_name) 


ctu_vmt_proportions <- mndot_vmt_ctu %>% 
  left_join(mndot_vmt_county,
            join_by(geoid, vmt_year),
            suffix = c(".ctu", ".county")) %>% 
  unique() %>% 
  mutate(ctu_proportion_of_county_daily_vmt = daily_vmt.ctu/daily_vmt.county,
         ctu_proportion_of_county_annual_vmt = annual_vmt.ctu/annual_vmt.county,
         ctu_proportion_of_county_centerline_miles = centerline_miles.ctu/centerline_miles.county
         )


# from the CTU level, sum up to get county level VMT
ctu_county_vmt <- mndot_vmt_ctu %>% 
  group_by(geoid, county_name, vmt_year) %>% 
  summarize(daily_vmt = sum(daily_vmt),
            annual_vmt = sum(annual_vmt),
            centerline_miles = sum(centerline_miles),
            n_ctus = n()) %>% 
  ungroup()

# find the difference in total reported County-level VMT 
# and county-level VMT aggregated UP from CTU's with reliable data
vmt_gaps <- ctu_county_vmt %>% 
  left_join(mndot_vmt_county,
            by = c("geoid", "vmt_year", "county_name"),
            suffix = c(".ctu_county", ".county")) %>% 
  mutate(gap_daily = daily_vmt.county - daily_vmt.ctu_county,
         gap_annual = annual_vmt.county - annual_vmt.ctu_county,
         gap_centerline_miles = centerline_miles.county - centerline_miles.ctu_county) 
  # select(geoid, vmt_year, county_name, gap_daily, gap_annual, gap_centerline_miles, n_ctus)


vmt_gap_true <- vmt_gaps %>% 
  left_join(net_county_corrections,
            by = c("county_name" = "correct_county_name",
                   "vmt_year" = "year")) %>% 
  ungroup() %>% 
  # true gap is the difference in the corrected and non-corrected CTU/county assignments
  mutate(gap_true_daily = gap_daily - daily_diff,
         gap_true_annual = gap_annual - annual_diff,
         gap_true_centerline_miles  =  gap_centerline_miles - centerline_miles_diff) %>%
  # fix NA values from 2015
  mutate(gap_true_annual = ifelse(is.na(gap_true_annual), gap_annual, gap_true_annual),
         gap_true_daily = ifelse(is.na(gap_true_daily), gap_daily, gap_true_daily),
         gap_true_centerline_miles = ifelse(is.na(gap_true_centerline_miles), gap_centerline_miles, gap_true_centerline_miles)) %>% 
  select(geoid, vmt_year, county_name, gap_true_daily, gap_true_annual, gap_true_centerline_miles, n_ctus)


  
