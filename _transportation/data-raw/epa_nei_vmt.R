source("R/_load_pkgs.R")
source("_meta/data-raw/county_geography.R")
counties_light <- county_geography %>% 
  select(GEOID, STATE, NAME, cprg_area)

# download files if they don't already exist ----- 
if(!file.exists("_transportation/data-raw/epa/nei/2020NEI/2020NEI_onroad/inputs/onroad/VMT_2020NEI_full_monthly_run3_09jan2023_v0.csv")){
  
  ## 2020 -----
  # https://gaftp.epa.gov/Air/nei/2020/doc/supporting_data/
  download.file("https://gaftp.epa.gov/Air/nei/2020/doc/supporting_data/onroad/2020NEI_onroad_activity_final_20230112.zip",
                destfile = "_transportation/data-raw/epa/nei/2020NEI/2020NEI_onroad_activity_final_20230112.zip")
  
  unzip(zipfile = "_transportation/data-raw/epa/nei/2020NEI/2020NEI_onroad_activity_final_20230112.zip",
        exdir = "_transportation/data-raw/epa/nei/2020NEI/",
        overwrite = TRUE)
  
  download.file("https://gaftp.epa.gov/Air/nei/2020/doc/supporting_data/onroad/EIC_SCC_XWALK_042721.xlsx",
                "_transportation/data-raw/epa/nei/2020NEI/EIC_SCC_XWALK_042721.xlsx")
  
  ## 2017 ----- 
  # https://gaftp.epa.gov/air/nei/2017/doc/supporting_data/
  download.file("https://gaftp.epa.gov/air/nei/2017/doc/supporting_data/2017NEI_onroad_activity_final.zip",
                destfile = "_transportation/data-raw/epa/nei/2017NEI//2017NEI_onroad_activity_final.zip")
  
  unzip(zipfile = "_transportation/data-raw/epa/nei/2017NEI/2017NEI_onroad_activity_final.zip",
        exdir = "_transportation/data-raw/epa/nei/2017NEI/",
        overwrite = TRUE)
  
  ## 2014 -----
  # https://gaftp.epa.gov/air/nei/2014/doc/2014v2_supportingdata/
  download.file("https://gaftp.epa.gov/air/nei/2014/doc/2014v2_supportingdata/onroad/2014v2_onroad_activity_final.zip",
                destfile = "_transportation/data-raw/epa/nei/2014NEI/2014v2_onroad_activity_final.zip")
  
  unzip(zipfile = "_transportation/data-raw/epa/nei/2014NEI/2014v2_onroad_activity_final.zip",
        exdir = "_transportation/data-raw/epa/nei/2014NEI/",
        overwrite = TRUE)
  
  download.file("https://gaftp.epa.gov/air/nei/2014/doc/2014v2_supportingdata/onroad/2014v1_EICtoEPA_SCCmapping.xlsx",
                destfile = "_transportation/data-raw/epa/nei/2014NEI/2014v1_EICtoEPA_SCCmapping.xlsx")
  
  ## 2011 ----- 
  # https://gaftp.epa.gov/air/nei/2011/doc/2011v2_supportingdata/onroad/
  download.file("https://gaftp.epa.gov/air/nei/2011/doc/2011v2_supportingdata/onroad/2011neiv2_supdata_or_VMT.zip",
                "_transportation/data-raw/epa/nei/2011NEI/2011neiv2_supdata_or_VMT.zip")
  
  unzip(zipfile = "_transportation/data-raw/epa/nei/2011NEI/2011neiv2_supdata_or_VMT.zip",
        exdir = "_transportation/data-raw/epa/nei/2011NEI/",
        overwrite = TRUE)
  
  download.file("https://gaftp.epa.gov/air/nei/2011/doc/2011v2_supportingdata/onroad/2011neiv2_supdata_or_SCC.zip",
                "_transportation/data-raw/epa/nei/2011NEI/2011neiv2_supdata_or_SCC.zip")
  
  unzip("_transportation/data-raw/epa/nei/2011NEI/2011neiv2_supdata_or_SCC.zip",
        exdir = "_transportation/data-raw/epa/nei/2011NEI/")
  
  ## 2008 -----
  # https://gaftp.epa.gov/air/nei/2008/doc/2008v3_supportingdata/
  
  download.file("https://gaftp.epa.gov/air/nei/2008/doc/2008v3_supportingdata/2008nei_supdata_4c.zip",
                destfile = "_transportation/data-raw/epa/nei/2008NEI/2008nei_supdata_4c.zip")
  unzip("_transportation/data-raw/epa/nei/2008NEI/2008nei_supdata_4c.zip",
        exdir = "_transportation/data-raw/epa/nei/2008NEI/")
  unzip("_transportation/data-raw/epa/nei/2008NEI/section4-mobile/onroad/VMT_NEI_2008_updated2_18jan2012_v3.zip",
        exdir = "_transportation/data-raw/epa/nei/2008NEI/section4-mobile/onroad/")
  
  download.file("https://gaftp.epa.gov/air/nei/2008/doc/2008v3_supportingdata/scc_eissector_xwalk_2008neiv3.xlsx",
                "_transportation/data-raw/epa/nei/2008NEI/scc_eissector_xwalk_2008neiv3.xlsx")
}


onroad_input_colnames <- c(
  "country_cd", "region_cd", "tribal_code", "census_tract_cd", 
  "shape_id", "scc", "CD", "MSR", "activity_type", "ann_parm_value",
  "calc_year", "date_updated", "data_set_id", "jan_value",
  "feb_value", "mar_value", "apr_value", "may_value", 
  "jun_value", "jul_value", "aug_value", "sep_value", 
  "oct_value", "nov_value", "dec_value", "comment")


read_nei_vmt <- function(vmt_path){
  
  data.table::fread(vmt_path,
                    skip = 16,
                    header = FALSE,
                    colClasses = "character",
                    col.names =  onroad_input_colnames
  ) %>% 
    filter(region_cd %in% county_geography$GEOID) %>% 
    left_join(counties_light, 
              by = c("region_cd" = "GEOID")) %>% 
    mutate(across(ends_with("value"), as.numeric)) %>% 
    rowwise() %>% 
    select(-tribal_code, -census_tract_cd,
           -shape_id, -CD, -MSR, -country_cd)
  
}


nei_vmt <- purrr::map_dfr(
  c("_transportation/data-raw/epa/nei/2008NEI/section4-mobile/onroad/activity_data_2008/VMT_NEI_2008_updated2_18jan2012_v3",
    "_transportation/data-raw/epa/nei/2011NEI/VMT_NEI_v2_2011_no_E85_30sep2014_v1.csv",
    "_transportation/data-raw/epa/nei/2014NEI/2014v2_onroad_activity_final/inputs/onroad/VMT_NEI_v2_2014_from_CDBs_06dec2017_nf_v4.csv",
    "_transportation/data-raw/epa/nei/2017NEI/2017NEI_onroad_activity_final/VMT_2017NEI_final_from_CDBs_month_redist_27mar2020_v3.csv",
    "_transportation/data-raw/epa/nei/2020NEI/2020NEI_onroad/inputs/onroad/VMT_2020NEI_full_monthly_run3_09jan2023_v0.csv"),
  read_nei_vmt
) %>% 
  arrange(desc(calc_year)) %>% 
  tidyr::separate_wider_position(
    scc, widths = c(
      "mobile_source" = 2,
      "fuel_type" = 2,
      "vehicle_type" = 2,
      "road_type" = 2,
      "process_type" = 2
    ), 
    cols_remove = FALSE
  ) %>% 
  select(-c("jan_value",
            "feb_value", "mar_value", "apr_value", "may_value", 
            "jun_value", "jul_value", "aug_value", "sep_value", 
            "oct_value", "nov_value", "dec_value"))



nei_vmt %>% 
  # filter(calc_year == "2008") %>% 
  # mutate(process_group_id = case_when(
  #   process_type %in% c("50", #  rural?
  #                       "70", # 
  #                       "90",
  #                       "10",
  #                       "30"
  #                       ) ~ "00",
  #   TRUE ~ NA
  # ),
  # scc_new = paste0(mobile_source, fuel_type, vehicle_type, road_type, 
  #                  process_group_id)) %>% 
  group_by(scc, calc_year,
           # mobile_source, fuel_type,
           # vehicle_type, road_type,process_type,
           region_cd, cprg_area, NAME) %>% 
  summarize(ann_parm_value = sum(ann_parm_value)) %>% 
  left_join(scc_mobile, join_by(scc)) %>% 
  left_join(scc_codes20)
# inner_join(scc_moves_smoke$process_types$process_types_agg,
#           by = c("process_type" = "scc")) %>% 
View


nei_vmt %>% 
  filter(calc_year %in% c("2020",
                          "2017",
                          "2014")) %>% 
  left_join(scc_codes20 %>% 
              select(-calc_year)) %>% 
  # left_join(scc_mobile) %>% 
  filter(is.na(scc_level_two),
         NAME == "Hennepin")


# read and harmonize scc codes------
# find unique scc codes per year
scc_year <- nei_vmt %>% 
  select(calc_year,
         scc, mobile_source, fuel_type, vehicle_type, road_type, 
         process_type) %>% 
  unique()

scc_all_process <- scc_year %>% 
  filter(process_type == "00") %>% 
  left_join(scc_mobile %>% 
              select(mobile_source, scc_level_one) %>% 
              unique(),
            by = c("mobile_source")) %>% 
  left_join(scc_mobile %>% 
              select(fuel_type, scc_level_two) %>% 
              unique(),
            by = c("fuel_type")) %>% 
  left_join(scc_mobile %>% 
              select(vehicle_type, scc_level_two, scc_level_three) %>% 
              unique(),
            by = c("vehicle_type",
                   "scc_level_two")) %>% 
  left_join(scc_mobile %>%
              select(process_type, scc_process, scc_road_type,
                     scc_level_one, scc_level_two) %>%
              unique()
            # by = join_by(process_type, scc_level_one, scc_level_two)
  ) %>% 
  left_join(scc_moves_smoke$process_types$process_types_agg,
            by = c("process_type" = "process_type_agg")) %>%
  left_join(scc_mobile %>%
              select(road_type, scc_road_type, scc_level_two) %>%
              unique()) %>% 
  left_join(scc_moves_smoke$road_types$road_types,
            by = c("road_type" = "moves_road_type")) %>%
  # mutate(
  #   scc_road_type = case_when(
  #     is.na(scc_road_type)  ~ paste0(road_type_description, " (MOVES/SMOKE)"),
  #     TRUE ~ scc_road_type
  #   ),
  #   scc_process = case_when(
  #     is.na(scc_process) ~ paste0(process_type_agg_desc, " (Aggregate MOVES/SMOKE)"),
  #     TRUE ~ scc_process)
  # ) %>% 
  select(-calc_year, -process_type_agg_desc,
         -road_type_description) %>% 
  unique()


scc_mobile_aggregate <- bind_rows(scc_mobile,
                                  scc_all_process)


left_join(nei_vmt,
          scc_mobile_aggregate) %>% View


nei_vmt %>% 
  filter(calc_year == "2020") %>% 
  left_join(scc_codes20) %>% View
left_join(scc_complete) %>% 
  select(scc, map_to, calc_year, last_inventory_year) %>% 
  unique() %>% View

scc_year %>% 
  left_join(scc_complete) %>% View


codes_year_unique <- 
  bind_rows(scc_codes08, 
            scc_codes11, 
            scc_codes14,
            scc_codes20) %>% 
  select(scc, data_category, calc_year,
         road_type,
         fuel_type, 
         category, 
         vehicle_type,
         scc_level_one,
         scclevel) %>%
  unique()

scc_year %>% 
  left_join(scc_codes14) 
# filter(is.na(road_type))
inner_join(scc_codes08)

