# process epa equates
source("R/_load_pkgs.R")
source("R/download_read_table.R")
source("_transportation/data-raw/epa_nei_vmt.R")
source("_transportation/data-raw/epa_source_classification_codes.R")

equates <- read_rds("_transportation/data-raw/epa/air_emissions_modeling/EQUATES/equates_mn_wi.RDS")
equates_cprg <- read_rds("_transportation/data-raw/epa/air_emissions_modeling/EQUATES/equates_cprg.RDS")

# https://dep.nj.gov/wp-content/uploads/airplanning/app-4-4-2016-2023-nj-modeling-inventory-statewide-5-13-24.xlsx
# listen, this is the best I could find using google.com

if(!file.exists("_transportation/data-raw/epa/air_emissions_modeling/EQUATES/app-4-4-2016-2023-nj-modeling-inventory-statewide-5-13-24.xlsx")){
  download.file("https://dep.nj.gov/wp-content/uploads/airplanning/app-4-4-2016-2023-nj-modeling-inventory-statewide-5-13-24.xlsx",
                destfile = "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/app-4-4-2016-2023-nj-modeling-inventory-statewide-5-13-24.xlsx")
}

scc_equates <- readxl::read_xlsx(
  "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/app-4-4-2016-2023-nj-modeling-inventory-statewide-5-13-24.xlsx",
  sheet = 2,
  col_types = "text") %>% 
  clean_names() %>% 
  select(label, scc, scc_description) %>% 
  unique() %>% 
  filter(scc %in% equates_cprg$scc) %>% 
  tidyr::separate_wider_delim(
    cols = scc_description,
    delim = ";",
    names = c(
      "scc_level_one",
      "scc_level_two",
      "scc_level_three",
      "scc_level_four"
    ),
    too_many = "merge",
    cols_remove = FALSE
  ) %>% 
  tidyr::separate_wider_delim(
    cols = scc_level_four,
    delim = ":",
    names = c(
      "scc_level_five",
      "scc_level_six"
    ),
    too_few = "align_start",
    too_many = "merge",
    cols_remove = FALSE
  ) %>% 
  mutate(across(starts_with("scc"), str_trim)) %>% 
  mutate(fuel_type = stringr::str_split(
    scc_level_two, 
    pattern = "-",
    simplify = TRUE)[,2] %>% 
      stringr::str_trim(),
    fuel_type = ifelse(fuel_type == "Ethanol (E",
                       "Ethanol (E-85)",
                       fuel_type)) %>% 
  select(label, scc, scc_description,
         scc_level_one,
         scc_level_two, scc_level_three,
         scc_level_four,
         scc_level_five, scc_level_six,
         fuel_type) %>% 
  unique() %>% 
  mutate(scc6 = stringr::str_sub(scc, 1, 6)) %>% 
  left_join(scc6_desc,
            by = "scc6")
  


equates_cprg_summary <- equates_cprg %>% 
  select(-equates_path) %>% 
  left_join(scc_equates,
            join_by(scc, scc6)) %>% 
  group_by(geoid, county_name, cprg_area, 
           emis_type, poll, calc_year,
           scc6, scc6_desc,
           scc_level_three,
           scc_level_five, 
           scc_level_six,
           fuel_type
  ) %>% 
  summarize(emis_per_mile = sum(ann_value),
            .groups = "keep")

nei_vmt_summary <- nei_vmt %>% 
  group_by(geoid, county_name, cprg_area, 
           calc_year,
           scc6, 
           scc6_desc,
           vehicle_type,
           road_type,
           fuel_type_detail
  ) %>% 
  summarize(annual_vmt = sum(ann_parm_value) %>% round(),
            .groups = "keep")

nei_vmt_summary %>% 
  rename(fuel_type = fuel_type_detail,
         scc_level_five = road_type) %>% 
  inner_join(equates_cprg_summary) %>% 
  filter(fuel_type != "Compressed Natural Gas (CNG)",
         scc_level_six != "Brake and Tire Wear") %>% 
  mutate(total_emissions = emis_per_mile * annual_vmt) %>% 
  filter(geoid == 
           "27003")
  
