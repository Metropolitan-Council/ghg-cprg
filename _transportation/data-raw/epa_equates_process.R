# process epa equates
source("R/_load_pkgs.R")
source("R/download_read_table.R")
source("_transportation/data-raw/epa_nei_vmt.R")
source("_transportation/data-raw/epa_source_classification_codes.R")

equates <- read_rds("_transportation/data-raw/epa/air_emissions_modeling/EQUATES/equates_mn_wi.RDS")
equates_cprg <- read_rds("_transportation/data-raw/epa/air_emissions_modeling/EQUATES/equates_cprg.RDS") %>% 
  select(-equates_path)

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
  # filter(scc %in% equates_cprg$scc) %>% 
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
            by = "scc6") %>% 
  mutate(alt_vehicle_type =
           case_when(
             scc_level_three %in% c(
               "Single Unit Short-haul Trucks",
               "Single Unit Long-haul Trucks",
               "Refuse Trucks", 
               "Combination Short-haul Trucks", 
               "Combination Long-haul Trucks"
             ) ~ "Commercial trucks",
             scc_level_three %in% c("Transit Buses",
                                    "Intercity Buses",
                                    "School Buses") ~ "Buses",
         scc_level_three %in% c("Motorcycles", "Motor Homes")  ~ "Other",
         TRUE ~ scc_level_three))


equates_cprg_scc <- equates_cprg %>% 
  left_join(scc_equates,
            join_by(scc, scc6))



equates_cprg_summary <- equates_cprg_scc %>% 
  group_by(geoid, county_name, cprg_area, 
           emis_type, poll, calc_year,
  ) %>% 
  summarize(ann_value = sum(ann_value),
            .groups = "keep") %>% 
  mutate(ann_value_grams = ann_value %>% 
           units::as_units("short_ton") %>% 
           units::set_units("gram") %>% 
           as.numeric) %>% 
  select(-ann_value) %>% 
  pivot_wider(names_from = poll,
              values_from = ann_value_grams) %>% 
  clean_names() %>% 
  rowwise() %>% 
  mutate(
    co2_co2_equivalent =
      sum(co2, (ch4 * gwp$ch4), na.rm = T),
    emissions_metric_tons_co2e = co2_co2_equivalent / 1000000
  )


epa_nei %>% 
  filter(vehicle_group == "On-Road") %>% 
  rowwise() %>% 
  mutate(
    co2_co2_equivalent =
      sum(total_co2, (total_ch4 * gwp$ch4), na.rm = T),
    emissions_metric_tons_co2e = co2_co2_equivalent / 1000000
  ) %>% 
  group_by(geoid, county_name, nei_inventory_year) %>% 
  summarize(total_ch4 = sum(total_ch4),
            total_co2 = sum(total_co2),
            total_n2o = sum(total_n2o),
            total_co2_w_equiv = sum(total_co2_w_equiv),
    emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e)) %>% 
  filter(county_name == "Hennepin") %>% 
  View

# visualize over time -----

equates_cprg_scc %>% 
  filter(poll == "CO2") %>% 
  filter(fuel_type != "Compressed Natural Gas (CNG)",
         scc_level_six != "Brake and Tire Wear") %>% 
ggplot() +
  aes(x = calc_year,
      y = ann_value,
      color = alt_vehicle_type) +
  geom_jitter(
    alpha = 0.5
  )

equates_cprg_scc %>% 
  group_by(geoid, county_name,  calc_year, poll, fuel_type, 
           scc_level_three) %>% 
  summarize(ann_value = sum(ann_value)) %>% 
  filter(geoid == "27053") %>% View




