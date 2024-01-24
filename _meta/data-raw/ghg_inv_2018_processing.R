library(tidyr)
library(dplyr)
library(stringr)

ctus_summary_2018 <- read_csv("_meta/data-raw/ctus_summary_2018.csv")
Fields_Formulas_GHGInv1 <- read_excel("_meta/data-raw/Fields_Formulas_GHGInv1.xlsx")
select_fields_v1 <- Fields_Formulas_GHGInv1 %>% filter(Keep_YN == 'Y') %>%
  mutate(units = case_when(str_detect(ctus_summary_field, fixed('Co2')) ~ 'Tonnes Co2e',
                           str_detect(ctus_summary_field, fixed('CO2e')) ~ 'Tonnes Co2e',
                           str_detect(ctus_summary_field, fixed('Therms')) ~ 'Therms',
                           str_detect(ctus_summary_field, fixed('Mmbtu')) ~ 'MMBtu',
                           str_detect(ctus_summary_field, fixed('MWh')) ~ 'MWh',
                           .default = NA))

data_2018 <- select(ctus_summary_2018, all_of(select_fields_v1[[1]])) %>%
  pivot_longer(cols = !(filter(select_fields_v1, Activity_Emissions %in% c('Metadata', 'Demographics'))[[1]]),
               names_to = 'variable', values_to = 'value') %>%
  left_join(select_fields_v1, by = c('variable' = 'ctus_summary_field')) %>%
  select(-Keep_YN) 
