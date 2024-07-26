# deep cuts from the 2020 NEI

# find which counties were used as representatives for all others  -----
rep_counties <- read_xlsx("_transportation/data-raw/epa/nei/2020_Representative_Counties_Analysis_20220720.xlsx",
          sheet = 2,
          col_types = "text"
) %>% 
  clean_names() %>% 
  filter(county_id %in% county_geography$GEOID)


counties_light <- county_geography %>% 
  select(GEOID, STATE, NAME, cprg_area)


  counties_light %>% 
    left_join(rep_counties, 
            by = c("GEOID" = "county_id")) %>% 
    left_join(counties_light,
              by =c("x2020_final_rep_county" = "GEOID")) %>% 
    mutate(represented = NAME.x == NAME.y) %>% 
    select(NAME.x, NAME.y, cprg_area.x,  represented) %>% 
    View
  
  # onroad activity data -----
  
datapasta::vector_paste()
  
  #FORMAT=FF10_ACTIVITY
  #COUNTRY US
  #YEAR 2020
  # Vehicle Miles Traveled (VMT) for 2020 NEI
  # CONUS and nonCONUS
  # Starting point: VMT_2020NEI_full_20220630_monthly_23nov2022_v5.csv
  # All monthly data (where it existed) was wiped out and replaced with new monthly data based on:
  # 1) state-submitted (cleaned) monthly profiles where they existed
  # 2) Where a state-submitted profile did not exist:
  #    LD vehicles: state- and road-type-specific profiles from Streelight dataset (NEI2020_monthVMTFraction_PERS_by_RoadTypeID_20230107.csv)
  #    HD vehicles except 62s: state-specific profiles from Streetlight dataset (NEI2020_monthVMTFraction_COMM_and_AcceptedAgencySubmittals_20230107.csv)
  #    Combo long haul trucks (62s): Flat profile; monthly total = annual total * (number of days in month) / 366
  
  
  scc_codes <- read_xlsx("_transportation/data-raw/epa/nei/onroad_activity_data_SCC_descriptions.xlsx",
                         sheet = 1,
                         col_types = "text"
  ) %>%
    clean_names()

  onroad <- data.table::fread("_transportation/data-raw/epa/nei/2020NEI_onroad/inputs/onroad/VMT_2020NEI_full_monthly_run3_09jan2023_v0.csv",
                              skip = 16,
                              header = FALSE,
                              colClasses = "character",
                              col.names =   c(
                                "country_cd", "region_cd", "tribal_code", "census_tract_cd", 
                                "shape_id", "scc", "CD", "MSR", "activity_type", "ann_parm_value",
                                "calc_year", "date_updated", "data_set_id", "jan_value",
                                "feb_value", "mar_value", "apr_value", "may_value", 
                                "jun_value", "jul_value", "aug_value", "sep_value", 
                                "oct_value", "nov_value", "dec_value", "comment")
  ) %>% 
    filter(region_cd %in% county_geography$GEOID) %>% 
    left_join(counties_light, 
              by = c("region_cd" = "GEOID")) %>% 
    left_join(scc_codes, by = c("scc" = "scc")) %>% 
    mutate(across(ends_with("value"), as.numeric)) %>% 
    rowwise() %>% 
    mutate(sum_values = sum(jan_value,
                            feb_value, mar_value, apr_value, may_value, 
                            jun_value, jul_value, aug_value, sep_value, 
                            oct_value, nov_value, dec_value)) %>% 
    select(STATE, NAME, cprg_area, vehicle_type, fuel_type, road_type, sum_values, everything())
    
  
