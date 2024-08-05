epa_nei_vmt


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
  
  # 2017 ----- 
  # https://gaftp.epa.gov/air/nei/2017/doc/supporting_data/
  download.file("https://gaftp.epa.gov/air/nei/2017/doc/supporting_data/2017NEI_onroad_activity_final.zip",
                destfile = "_transportation/data-raw/epa/nei/2017NEI//2017NEI_onroad_activity_final.zip")
  
  unzip(zipfile = "_transportation/data-raw/epa/nei/2017NEI/2017NEI_onroad_activity_final.zip",
        exdir = "_transportation/data-raw/epa/nei/2017NEI/",
        overwrite = TRUE)
  
  # 2014 -----
  # https://gaftp.epa.gov/air/nei/2014/doc/2014v2_supportingdata/
  download.file("https://gaftp.epa.gov/air/nei/2014/doc/2014v2_supportingdata/onroad/2014v2_onroad_activity_final.zip",
                destfile = "_transportation/data-raw/epa/nei/2014NEI/2014v2_onroad_activity_final.zip")
  
  unzip(zipfile = "_transportation/data-raw/epa/nei/2014NEI/2014v2_onroad_activity_final.zip",
        exdir = "_transportation/data-raw/epa/nei/2014NEI/",
        overwrite = TRUE)
  
  download.file("https://gaftp.epa.gov/air/nei/2014/doc/2014v2_supportingdata/onroad/2014v1_EICtoEPA_SCCmapping.xlsx",
                destfile = "_transportation/data-raw/epa/nei/2014NEI/2014v1_EICtoEPA_SCCmapping.xlsx")
  
  # 2011 ----- 
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
  
  # 2008 -----
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
    select(-tribal_code, -census_tract_cd)
  
}


nei_vmt <- purrr::map_dfr(
  c("_transportation/data-raw/epa/nei/2008NEI/section4-mobile/onroad/VMT_NEI_2008_updated2_18jan2012_v3",
    "_transportation/data-raw/epa/nei/2011NEI/VMT_NEI_v2_2011_no_E85_30sep2014_v1.csv",
    "_transportation/data-raw/epa/nei/2014NEI/2014v2_onroad_activity_final/inputs/onroad/VMT_NEI_v2_2014_from_CDBs_06dec2017_nf_v4.csv",
    "_transportation/data-raw/epa/nei/2017NEI/2017NEI_onroad_activity_final/VMT_2017NEI_final_from_CDBs_month_redist_27mar2020_v3.csv",
    "_transportation/data-raw/epa/nei/2020NEI/2020NEI_onroad/inputs/onroad/VMT_2020NEI_full_monthly_run3_09jan2023_v0.csv"),
  read_nei_vmt
) %>% 
  arrange(desc(calc_year))


# read and harmonize scc codes------
scc_year <- nei_vmt %>% 
  select(calc_year, scc) %>% 
  unique() %>% 
  mutate(scc_length = nchar(scc))

scc_complete <- read_csv("_transportation/data-raw/epa/SCCDownload-2024-0805-165321.csv",
                         col_types = "c") %>% 
  clean_names()


scc_complete_map <- scc_complete %>% 
  select(scc, last_inventory_year, map_to, data_category) %>% 
  unique()

nei_vmt %>% 
  filter(calc_year == "2020") %>% 
  left_join(scc_codes20) %>% View
  left_join(scc_complete) %>% 
  select(scc, map_to, calc_year, last_inventory_year) %>% 
  unique() %>% View

scc_year %>% 
  left_join(scc_complete) %>% View


scc_codes20 <- read_xlsx("_transportation/data-raw/epa/nei/2020NEI/onroad_activity_data_SCC_descriptions.xlsx",
                         sheet = 1,
                         col_types = "text"
) %>%
  clean_names() %>% 
  mutate(calc_year = "2020")


scc_codes14 <- read_xlsx("_transportation/data-raw/epa/nei/2014NEI/2014v1_EICtoEPA_SCCmapping.xlsx",
                         sheet = 3,
                         col_types = "text") %>% 
  clean_names() %>% 
  mutate(calc_year = "2014")

scc_codes11 <- read_xlsx("_transportation/data-raw/epa/nei/2011NEI/MOVES2014_SCC_List_v8.xlsx",
                         sheet = 2,
                         col_types = "text") %>% 
  clean_names() %>% 
  mutate(calc_year = "2011")

scc_codes08 <- read_xlsx("_transportation/data-raw/epa/nei/2008NEI/scc_eissector_xwalk_2008neiv3.xlsx",
                         sheet = 2,
                         col_types = "text") %>% 
  clean_names() %>% 
  mutate(calc_year = "2008",
         scc = code)



nei_vmt %>% 
  filter(calc_year == "2008") %>% 
  left_join(scc_codes08) %>% names

nei_vmt %>% 
  filter(calc_year == "2020") %>% 
  left_join(scc_codes20)

scc_year %>% scc_year %>% names()
  filter(scc %in% scc_codes08$scc)


scc_codes20 %>% 
  full_join(scc_codes14,
            by = c("scc",
                   "category" = "nei_data_category")) %>% View

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
         
