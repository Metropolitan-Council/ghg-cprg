# assembling cprg data for CD_Emissions import
load("_meta/data/cprg_county_emissions.RDS")
load("_meta/data/inventory_2018.RDS")
load("_meta/data/sector_category.RDS")

library(dplyr)

sectors <- select(cprg_county_emissions, sector, category, source) %>% unique() %>%
  mutate(sector_desc = sector,
         sector = case_when(sector_desc == 'Energy' ~ 'stationary_energy',
                            sector_desc == 'Waste' ~ 'waste_wastewater',
                            sector_desc == 'Transportation' ~ 'transportation',
                            .default = NA)
         )

sectors_tbl <- sectors %>% select(sector, sector_desc) %>% unique()

categories <- sectors %>% mutate(category_desc = category,
                                   category = case_when(category_desc == 'Passenger vehicles' ~ 'passenger',
                                                        category_desc == 'Commercial vehicles' ~ 'commercial_vehicles',
                                                        category_desc == 'Residential energy' ~ 'residential',
                                                        category_desc == 'Commercial energy' ~ 'commercial_energy',
                                                        category_desc == 'Industrial energy' ~ 'industrial',
                                                        category_desc == 'Liquid stationary fuels' ~ 'liquid_fuels',
                                                        category_desc == 'Wastewater' ~ 'wastewater',
                                                        category_desc == 'Solid waste' ~ 'solid_waste',
                                                        .default = NA
                                                          )) 
category_tbl <- categories %>% select(category, category_desc, sector) %>% unique()

source_tbl <- categories %>% 
  mutate(source_desc = source,
         source = case_when(source_desc == 'Light-duty vehicles' ~ 'light_duty',
                            source_desc == 'Medium-duty vehicles' ~ 'medium_duty',
                            source_desc == 'Heavy-duty vehicles' ~ 'heavy_duty',
                            source_desc == 'Propane' ~ 'propane',
                            source_desc == 'Kerosene' ~ 'kerosene',
                            source_desc == 'Electricity' ~ 'electricity',
                            source_desc == 'Natural gas' ~ 'natural_gas',
                            source_desc == 'Wastewater' ~ 'wastewater',
                            source_desc == 'Landfill' ~ 'landfill',
                            source_desc == 'Organics' ~ 'organics',
                            source_desc == 'Recycling' ~ 'recycling',
                            source_desc == 'Waste to energy' ~ 'waste_to_energy',
                            .default = NA
                            )) %>%
  select(source, source_desc, sector) %>% unique() %>% 
  add_row(source = 'transit', source_desc = 'Transit', sector = 'transportation') # maybe???

write.csv(sectors_tbl, '_meta/data-raw/sectors_tbl.csv', row.names = FALSE)
write.csv(source_tbl, '_meta/data-raw/source_tbl.csv', row.names = FALSE)
write.csv(category_tbl, '_meta/data-raw/category_tbl.csv', row.names = FALSE)


inv_2021_reformat <- cprg_county_emissions %>% select(geog_id, year, sector, category, source, emissions_metric_tons_co2e, data_source, factor_source) %>%
  mutate(geog_level_id = 'CO', .after = geog_id) %>%
  mutate(value_Emissions = emissions_metric_tons_co2e,
         units_Emissions = 'Tonnes CO2e') %>% rename(geog_unit_id = geog_id)
pop_2021 <- cprg_county_emissions %>% select(geog_id, geog_level, year, population = county_total_population, data_source = population_data_source) %>%
  mutate(geog_level_id = 'CO') %>% rename(geog_unit_id = geog_id)

# same process to merge with inventory_2018
sectors_2018 <- inventory_2018 %>% select(sector, sub_sector, source) %>% unique() %>%
  mutate(sector = case_when(source == 'trucks' ~ 'transportation',
                            .default = sector),
         sub_sector = case_when(source == 'trucks' ~ 'on-road',
                                .default = sub_sector)) %>%
  mutate(source_2018 = source,
         source = case_when(sector == 'transportation' ~ stringr::str_to_sentence(source_2018),
                            .default = stringr::str_to_sentence(sub_sector)),
         category = case_when(sector == 'waste_wastewater' ~ stringr::str_to_sentence(source_2018),
                              sub_sector == 'kerosene and other fuels' ~ 'Liquid stationary fuels',
                              sub_sector == 'propane' ~ 'Liquid stationary fuels',
                              source_2018 == 'buildings' ~ 'Residential energy', # I think this is what we figured out?
                              source_2018 == 'light-duty vehicle' ~ 'Passenger vehicles',
                              source_2018 == 'transit' ~ 'Passenger vehicles',
                              sector == 'transportation' ~ 'Commercial vehicles',
                              .default = NA))

inv_2018_reformat <- inventory_2018 %>% 
  mutate(total_Emissions = case_when(stringr::str_starts(variable_Emissions, stringr::fixed('$')) ~ value_Emissions * Population,
                                     .default = value_Emissions)) %>%
  mutate(source_2018 = source,
         source = case_when(sector == 'transportation' ~ stringr::str_to_sentence(source_2018),
                            sub_sector == 'compost' ~ 'Landfill',
                            .default = stringr::str_to_sentence(sub_sector)),
         category = case_when(sector == 'waste_wastewater' ~ stringr::str_to_sentence(source_2018),
                              sub_sector %in% c('kerosene', 'propane') ~ 'Liquid stationary fuels',
                              source_2018 == 'buildings' ~ 'Residential energy', # I think this is what we figured out?
                              source_2018 == 'light-duty vehicles' ~ 'Passenger vehicles',
                              source_2018 == 'transit' ~ 'Passenger vehicles',
                              sector == 'transportation' ~ 'Commercial vehicles',
                              .default = NA)) %>%
  select(geog_id = GEOG_ID, year = Year, sector, category, source, value_Emissions = total_Emissions, value_Activity, units_Emissions, units_Activity) %>%
  mutate(geog_id = as.character(geog_id),
         geog_level_id = 'CTU', .after = geog_id) %>% rename(geog_unit_id = geog_id)

## check trucks
trucks <- inv_2018_reformat %>% filter(source %in% c('heavy-duty vehicle', 'medium-duty vehicle')) %>%
  group_by(geog_id, year) %>% summarise(heavy_medium = sum(value_Emissions)) %>% 
  left_join(filter(inv_2018_reformat, source == 'trucks'), by = c('geog_id', 'year'))
write.csv(trucks, '_meta/data-raw/trucks_troubleshooting_05212024.csv')
# trucks ARE just the sum of heavy and medium duty vehicles!!! GREAT.

pop_2018 <- inventory_2018 %>% select(geog_id = GEOG_ID, year = Year, population = Population) %>% unique() %>%
  mutate(geog_id = as.character(geog_id), geog_level_id = 'CTU', .after = geog_id) %>% rename(geog_unit_id = geog_id) %>%
  mutate(data_source = 'Met Council Population Estimates')

population_tbl <- bind_rows(pop_2021, pop_2018) 
write.csv(population_tbl, '_meta/data-raw/population_tbl.csv', row.names = FALSE)

inventory_db <- inv_2021_reformat %>% 
  left_join(sectors_tbl, by = join_by(sector == sector_desc)) %>% mutate(sector = sector.y) %>%
  select(-emissions_metric_tons_co2e, -sector.y) %>% 
  bind_rows(inv_2018_reformat) %>%
  left_join(select(category_tbl, category, category_desc), by = join_by(category == category_desc)) %>%
  mutate(category = category.y, category.y = NULL) %>%
  left_join(unique(select(source_tbl, source, source_desc)), by = join_by(source == source_desc)) %>%
  mutate(source = source.y, source.y = NULL) 

write.csv(inventory_db, '_meta/data-raw/inventory_tbl.csv', row.names = FALSE, na = "")

nulls <- filter(inventory_db, is.na(source)) 

# data sources table
inv_data_sources <- inventory_db %>% select(data_source) %>% unique() %>%
  mutate(data_source_desc = data_source,
         data_source = case_when(data_source_desc == 'Streetlight' ~ 'streetlight',
                                 data_source_desc == 'Wisconsin DNR' ~ 'WI_DNR',
                                 data_source_desc == 'EIA RECS (2020)' ~ 'EIA_RECS',
                                 data_source_desc == 'EPA State GHG Inventory and Projection Tool' ~ 'EPA_STATE',
                                 data_source_desc == 'MPCA SCORE' ~ 'MPCA_SCORE'
                                 stringr::str_detect(data_source_desc, 'NREL SLOPE') ~ 'NREL_SLOPE', # string contains NREL SLOPE
                                 # individual electric utilities
                                 # individual natural gas utilities
                                 .default = NA
                                 ))

# emissions factors table 
factors_tbl <- inventory_db %>% select(factor_source) %>% unique() %>% filter(!is.na(factor_source)) %>%
  mutate(year = stringr::str_extract(factor_source, '[\\d]+'),
         factor_code = case_when(stringr::str_detect(factor_source, 'EPA MOVES') ~ 'MOVES',
                                 stringr::str_detect(factor_source, 'EPA GHG Emission Factors Hub') ~ 'EPA_HUB',
                                 factor_source == 'eGRID MROW' ~ 'EGRID_MROW',
                                 factor_source == 'EPA State GHG Inventory and Projection Tool' ~ 'EPA_STATE')
  )

# also need to add ST records to geogs table (done!)
# and ST records to population table
st_pop <- cprg_county_proportions %>% select(geog_unit_id = STATEFP, year, population = state_population, population_data_source) %>%
  unique() %>% mutate(geog_level_id = 'ST', .after = geog_unit_id) %>%
  bind_rows(
    select(cprg_county_proportions, geog_unit_id = COUNTYFP, year, population = county_population, population_data_source) %>% 
      mutate(geog_level_id = 'CO', .after = geog_unit_id)
    )


## to do as of EOD May 21: 
# add 2020 ST/CO pop records to population table

# make cprg_county_emissions_vw ## drafted!
# make cprg_county_proportions_vw
