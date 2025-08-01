# using population, centerline miles, or another method
# attribute the remainder of county level VMT to 
# CTUs that don't have CTU level VMT data

source("R/_load_pkgs.R")

cprg_ctu <- readRDS("_meta/data/cprg_ctu.RDS") %>%
  mutate(ctu_name_full = paste0(ctu_name, ", ", ctu_class))
cprg_county <- readRDS("_meta/data/cprg_county.RDS") %>% sf::st_drop_geometry()


ctu_population <- readRDS("_meta/data/ctu_population.RDS") %>%
  left_join(cprg_county) %>%
  mutate(ctu_name_full = paste0(ctu_name, ", ", ctu_class),
         ctu_name_full_county = paste0(ctu_name_full, ", ", county_name ))

readRDS("_meta/data/prop")

mndot_vmt_ctu <- readRDS("_transportation/data/mndot_vmt_ctu.RDS")
mndot_vmt_county <- readRDS("_transportation/data/dot_vmt.RDS") %>% 
  filter(data_source == "MnDOT")


ctu_vmt_proportions <- mndot_vmt_ctu %>% 
  left_join(mndot_vmt_county,
            join_by(geoid, vmt_year),
            suffix = c(".ctu", ".county")) %>% 
  mutate(ctu_proportion_of_county_daily_vmt = daily_vmt.ctu/daily_vmt.county,
         ctu_proportion_of_county_annual_vmt = annual_vmt.ctu/annual_vmt.county,
         ctu_proportion_of_county_centerline_miles = centerline_miles.ctu/centerline_miles.county
         )


ctu_county_vmt <- mndot_vmt_ctu %>% 
  group_by(geoid, vmt_year) %>% 
  summarize(daily_vmt = sum(daily_vmt),
            annual_vmt = sum(annual_vmt),
            centerline_miles = sum(centerline_miles))
