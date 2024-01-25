source("R/_load_pkgs.R")

# load in data from GHG Inventory 2018 tableau workbooks
ctus_summary_2018 <- readr::read_csv("_meta/data-raw/ctus_summary_2018.csv")
# load in field names, metadata etc.
Fields_Formulas_GHGInv1 <- readr::read_csv("_meta/data-raw/Fields_Formulas_GHGInv1.csv")
sector_breakdown <- readr::read_csv("_meta/data-raw/sector_breakdown.csv")

# manipulate metadata 
select_fields_v1 <- Fields_Formulas_GHGInv1 %>% filter(Keep_YN == 'Y') %>%
  mutate(units = case_when(str_detect(ctus_summary_field, fixed('Co2')) ~ 'Tonnes CO2e',
                           str_detect(ctus_summary_field, fixed('CO2e')) ~ 'Tonnes CO2e',
                           str_detect(ctus_summary_field, fixed('Therms')) ~ 'Therms',
                           str_detect(ctus_summary_field, fixed('Mmbtu')) ~ 'MMBtu',
                           str_detect(ctus_summary_field, fixed('MWh')) ~ 'MWh',
                           .default = NA),
         sector_source = case_when(Activity_Emissions %in% c('Metadata', 'Demographics') ~ NA,
                                   str_detect(ctus_summary_field, fixed('Compost')) ~ 'compost',
                                   str_detect(ctus_summary_field, fixed('Prop')) ~ 'propane',
                                   str_detect(ctus_summary_field, fixed('Kerodfo')) ~ 'kerodfo',
                                   str_detect(ctus_summary_field, fixed('Electricity')) ~ 'electricity',
                                   str_detect(ctus_summary_field, fixed('Landfill')) ~ 'landfill',
                                   str_detect(ctus_summary_field, fixed('NG')) ~ 'natural gas',
                                   str_detect(ctus_summary_field, fixed('Other Fuels')) ~ 'other fuels',
                                   str_detect(ctus_summary_field, fixed('Heavy')) ~ 'heavy-duty vehicle',
                                   str_detect(ctus_summary_field, fixed('Medium')) ~ 'medium-duty vehicle',
                                   str_detect(ctus_summary_field, fixed('Personal')) ~ 'light-duty vehicle',
                                   str_detect(ctus_summary_field, fixed('Passenger')) ~ 'passenger',
                                   str_detect(ctus_summary_field, fixed('Transit')) ~ 'transit',
                                   str_detect(ctus_summary_field, fixed('Trucks')) ~ 'trucks',
                                   .default = NA
                                )
         )


data_2018 <- select(ctus_summary_2018, all_of(select_fields_v1[[1]])) %>%
  pivot_longer(cols = !(filter(select_fields_v1, Activity_Emissions %in% c('Metadata', 'Demographics'))[[1]]),
               names_to = 'variable', values_to = 'value') %>%
  left_join(select_fields_v1, by = c('variable' = 'ctus_summary_field')) %>%
  mutate(GEOG_ID = `Ctu Id`,
            GEOG_DESC = `Ctu Name`, .before = `Ctu Id`) %>%
  select(-Keep_YN, -`Ctu Id`, -`Ctu Name`) %>% 
  pivot_wider(names_from = Activity_Emissions, names_sep = '_', 
              values_from = c(value, units, variable))

saveRDS(data_2018, '_meta/data/inventory_2018.RDS')
