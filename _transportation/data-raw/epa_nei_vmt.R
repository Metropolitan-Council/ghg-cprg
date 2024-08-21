source("R/_load_pkgs.R")
source("_meta/data-raw/county_geography.R")

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
  download.file("https://gaftp.epa.gov/air/nei/2017/doc/supporting_data/onroad/2017NEI_onroad_activity_final.zip",
                destfile = "_transportation/data-raw/epa/nei/2017NEI//2017NEI_onroad_activity_final.zip")
  
  unzip(zipfile = "_transportation/data-raw/epa/nei/2017NEI/2017NEI_onroad_activity_final.zip",
        exdir = "_transportation/data-raw/epa/nei/2017NEI/2017NEI_onroad_activity_final/",
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
  unzip("_transportation/data-raw/epa/nei/2008NEI/section4-mobile/onroad/activity_data_2008.zip",
      exdir = "_transportation/data-raw/epa/nei/2008NEI/section4-mobile/onroad/activity_data_2008/")
  download.file("https://gaftp.epa.gov/air/nei/2008/doc/2008v3_supportingdata/scc_eissector_xwalk_2008neiv3.xlsx",
                "_transportation/data-raw/epa/nei/2008NEI/scc_eissector_xwalk_2008neiv3.xlsx")
}


read_nei_vmt <- function(vmt_path){
  
  data.table::fread(vmt_path,
                    skip = 16,
                    header = FALSE,
                    colClasses = "character",
                    col.names =  c(
                      "country_cd", "region_cd", "tribal_code", "census_tract_cd", 
                      "shape_id", "scc", "CD", "MSR", "activity_type", "ann_parm_value",
                      "calc_year", "date_updated", "data_set_id", "jan_value",
                      "feb_value", "mar_value", "apr_value", "may_value", 
                      "jun_value", "jul_value", "aug_value", "sep_value", 
                      "oct_value", "nov_value", "dec_value", "comment")
  ) %>% 
    filter(region_cd %in% county_geography$geoid) %>% 
    mutate(geoid = region_cd) %>% 
    left_join(counties_light, 
              by = c("geoid")) %>% 
    mutate(across(ends_with("value"), as.numeric),
           scc6 = str_sub(scc, 1, 6)) %>% 
    rowwise() %>% 
    select(-tribal_code, -census_tract_cd,
           -shape_id, -CD, -MSR, -country_cd,
           -date_updated, -data_set_id,
           -starts_with(tolower(month.abb))
    )
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
  left_join(scc_onroad,
            by= c("scc", "scc6"))


# how does the total by road type compare with 
# the values we have from MnDOT?
# 
# If we total ALL VMT, we get almost identical values
# 
# totaling (roughly) by (functional class -> MOVES road type)
# we get values in the same ballpark

nei_vmt %>% 
  group_by(geoid, county_name, calc_year, road_type) %>%
  summarize(total_vmt = sum(ann_parm_value)) %>% View
