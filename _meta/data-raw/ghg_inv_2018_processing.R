source("R/_load_pkgs.R")
library(dplyr)
library(stringr)

# load in data from GHG Inventory 2018 tableau workbooks
ctus_summary_2018 <- readr::read_csv(file.path(here::here(), "_meta/data-raw/ctus_summary_2018.csv"))
# load in field names, metadata etc.
Fields_Formulas_GHGInv1 <- readr::read_csv(file.path(here::here(), "_meta/data-raw/Fields_Formulas_GHGInv1.csv"))
sector_breakdown <- readr::read_csv(file.path(here::here(), "_meta/data-raw/sector_breakdown.csv"))

# manipulate metadata
select_fields_v1 <- Fields_Formulas_GHGInv1 %>%
  filter(Keep_YN == "Y") %>%
  mutate(
    units = case_when(str_detect(ctus_summary_field, fixed("Co2")) ~ "Tonnes CO2e",
      str_detect(ctus_summary_field, fixed("CO2e")) ~ "Tonnes CO2e",
      str_detect(ctus_summary_field, fixed("Therms")) ~ "Therms",
      str_detect(ctus_summary_field, fixed("Mmbtu")) ~ "MMBtu",
      str_detect(ctus_summary_field, fixed("MWh")) ~ "MWh",
      .default = NA
    ),
    source = case_when(Activity_Emissions %in% c("Metadata", "Demographics") ~ NA,
      str_detect(ctus_summary_field, fixed("Heavy")) ~ "heavy-duty vehicles",
      str_detect(ctus_summary_field, fixed("Medium")) ~ "medium-duty vehicles",
      # str_detect(ctus_summary_field, fixed("Personal")) ~ "light-duty vehicle", # different from passenger numbers? but Passenger is the numbers showing up in GHG 2018 app
      str_detect(ctus_summary_field, fixed("Passenger")) ~ "light-duty vehicles",
      str_detect(ctus_summary_field, fixed("Transit")) ~ "transit",
      # str_detect(ctus_summary_field, fixed("Trucks")) ~ "trucks", # is this just heavy plus medium duty? yes
      .default = NA
    ),
    sub_sector = case_when(
      Activity_Emissions %in% c("Metadata", "Demographics") ~ NA,
      str_detect(ctus_summary_field, fixed("Compost")) ~ "compost",
      str_detect(ctus_summary_field, fixed("Landfill")) ~ "landfill",
      str_detect(ctus_summary_field, fixed("Prop")) ~ "propane",
      str_detect(ctus_summary_field, fixed("Kerodfo")) ~ "kerosene", 
      # other fuels entry is the one in GHG 2018 app, but it's just the sum of propane and kerosene
      # str_detect(ctus_summary_field, fixed("Other Fuels")) ~ "kerosene and other fuels",
      str_detect(ctus_summary_field, fixed("Electricity")) ~ "electricity",
      str_detect(ctus_summary_field, fixed("NG")) ~ "natural gas",
    )
  ) %>%
  left_join(select(sector_breakdown, -category), by = c("source")) %>%
  mutate(
    sub_sector = if_else(is.na(sub_sector.x), sub_sector.y, sub_sector.x),
    sub_sector.x = NULL, sub_sector.y = NULL
  ) %>%
  left_join(select(sector_breakdown, -category), by = "sub_sector") %>%
  mutate(
    source = if_else(is.na(source.x), source.y, source.x),
    source.x = NULL, source.y = NULL,
    sector = if_else(is.na(sector.x), sector.y, sector.x),
    sector.x = NULL, sector.y = NULL
  ) %>%
  unique()

# reshape data and add metadata
data_2018 <- select(ctus_summary_2018, all_of(select_fields_v1[[1]])) %>%
  tidyr::pivot_longer(
    cols = !(filter(select_fields_v1, Activity_Emissions %in% c("Metadata", "Demographics"))[[1]]),
    names_to = "variable", values_to = "value"
  ) %>%
  left_join(select_fields_v1, by = c("variable" = "ctus_summary_field")) %>%
  mutate(
    GEOG_ID = `Ctu Id`,
    GEOG_DESC = `Ctu Name`, .before = `Ctu Id`
  ) %>% mutate(variable_Emissions = variable) %>%
  select(-Keep_YN, -`Ctu Id`, -`Ctu Name`, -variable) %>% 
  tidyr::pivot_wider(
    names_from = Activity_Emissions, names_sep = "_",
    values_from = c(value, units)
  )


saveRDS(data_2018, "_meta/data/inventory_2018.RDS")
saveRDS(sector_breakdown, "_meta/data/sector_category.RDS")
write.csv(inventory_2018, "_meta/data/ghg_inventory_2018.csv", row.names = FALSE)
