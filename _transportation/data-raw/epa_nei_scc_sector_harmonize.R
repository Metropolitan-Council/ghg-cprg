# scc sector mappings

nei_onroad_emissions <- readRDS("_transportation/data-raw/epa/nei_onroad_emissions.RDS")
nei_nonroad_emissions <- readRDS("_transportation/data-raw/epa/nei_nonroad_emissions.RDS")



# we need to match the NEI sector groupings with individual SCCs
# First, lets get all the SCCs that are observed in the most detailed 
# emissions data for on-road and non-road
nei_scc <- nei_nonroad_emissions %>% 
  bind_rows(nei_onroad_emissions) %>% 
  select(scc, data_category) %>% 
  unique()

# now join our observed SCCs
# with our more complete on_non_road_scc
scc_sector <- nei_scc %>% 
  left_join(scc_complete_road) 

scc_sector %>% 
  filter(is.na(sector)) %>% 
  nrow()

scc_forward <- scc_sector %>% 
  filter(scc_new != scc)


scc_forward_combine <- scc_sector %>% 
  select(scc = scc_new, 
         scc6  = scc6_new,
         data_category) %>% 
  unique() %>% 
  left_join(scc_sector %>% 
              select(-map_to))


nonroad_correct <- nei_nonroad_emissions %>% 
  # filter(scc %in% scc_forward$scc) %>% 
  left_join(scc_sector) %>% 
  select(geoid, state_name, county_name, scc, scc_new, scc6, scc6_new, nei_year, 
         total_emissions, pollutant_code, emissions_uom,
         pollutant_desc, data_set) %>% 
  mutate(scc = ifelse(scc != scc_new, scc_new, scc),
         scc6 = ifelse(scc6 != scc6_new, scc6_new, scc6)) %>% 
  select(-scc_new, -scc6_new) %>% 
  left_join(scc_sector)

# test that all emissions are still accounted for

testthat::expect_identical(
  nei_nonroad_emissions %>% 
    left_join(scc_sector) %>% 
    group_by(geoid, nei_year, pollutant_code, pollutant_desc) %>% 
    summarise(total_emissions = sum(total_emissions)),
  nonroad_correct %>% 
    left_join(scc_sector) %>% 
    group_by(geoid, nei_year, pollutant_code, pollutant_desc) %>% 
    summarise(total_emissions = sum(total_emissions))
)


# combine scc level emissions

nei_scc_emissions <- bind_rows(nei_onroad_emissions %>% 
                                 left_join(scc_sector),
                               nonroad_correct) %>% 
  select(-map_to, -scc_new)


nei_emissions_summary <- nei_scc_emissions %>% 
  group_by(geoid, county_name, cprg_area, nei_year, data_category,
           pollutant_code, pollutant_desc, emissions_uom,
           # sector,
           # fuel_type_detect, scc6,
           # scc_level_one, scc_level_two,
           # scc_level_three,
           # alt_mode,
           alt_mode_truck
           # scc6_desc_broad
           ) %>% 
  summarize(total_emissions = sum(total_emissions),
            .groups = "keep")


nei_emissions_summary %>% 
  filter(county_name == "Hennepin",
         pollutant_code == "CO2",
         data_category == "Onroad") %>% 
  plot_ly(
    type = "scatter",
    x = ~nei_year,
    y = ~total_emissions,
    color = ~alt_mode_truck
  )
