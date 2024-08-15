# scc sector mappings
# 
# we need to match the NEI sector groupings with individual SCCs

nei_scc <- nei_nonroad_emissions %>% 
  select(scc, nei_year) %>% 
  mutate(data_category = "Nonroad") %>% 
  unique() %>% 
  bind_rows(nei_onroad_emissions %>% 
              select(scc, nei_year) %>% 
              unique() %>% 
              mutate(data_category = "Onroad"))




nei_map_forward <- scc_complete %>% 
  filter(scc %in% nei_scc$scc,
         !is.na(map_to),
         map_to != "None") %>% 
  select(-last_edited_date, -activity_value_required, -date_created,
         -last_updated_date, -x28)


scc_nei_join <- nei_scc %>% 
  left_join(scc_complete %>% 
              select(-last_edited_date, -activity_value_required, -date_created,
                     -last_updated_date, -x28)) %>% 
  mutate(
    scc_new = case_when(is.na(map_to) | map_to == "None" ~ scc,
                        TRUE ~ map_to)
  )

  
  
  
