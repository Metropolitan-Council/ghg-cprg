source("R/_load_pkgs.R")

ag_manure_mgmt <- read_csv('_agriculture/data-raw/ag_manure_management.csv')

# format csv into something usable

dairy_mm <- ag_manure_mgmt[3:1603,1:7] %>% 
  row_to_names(1) %>% 
  mutate(year = as.numeric(substr(`Year & State`,1,4)),
         state = substr(`Year & State`,5,6)) %>% 
  filter(state %in% c("MN","WI"), year >=2005) %>% 
  select(-1) %>% 
  pivot_longer(cols = 1:6, names_to = "mgmt_system", values_to = "percentage") %>% 
  mutate(percentage = as.numeric(str_remove(percentage, "%"))/100,
         managed = if_else(mgmt_system %in% c("Daily Spread", "Pasture"), "No", "Yes"),
         livestock_type = "Dairy Cows")

heifers_mm <- ag_manure_mgmt[3:1603,9:12] %>% 
  row_to_names(1) %>% 
  mutate(year = as.numeric(substr(`Year & State`,1,4)),
         state = substr(`Year & State`,5,6)) %>% 
  filter(state %in% c("MN","WI"), year >=2005) %>% 
  select(-1) %>% 
  pivot_longer(cols = 1:3, names_to = "mgmt_system", values_to = "percentage") %>% 
  mutate(percentage = as.numeric(str_remove(percentage, "%"))/100,
         managed = if_else(mgmt_system %in% c("Daily Spread", "PRP"), "No", "Yes"),
         livestock_type = "Heifers") #not sure we have data on percent that are heifers

feedlot_cattle_mm <- ag_manure_mgmt[3:1603,14:15] %>% 
  row_to_names(1) %>% 
  mutate(state = STATE) %>% 
  filter(state %in% c("MN","WI")) %>% 
  select(-1) %>% 
  pivot_longer(cols = 1, names_to = "mgmt_system", values_to = "percentage") %>% 
  mutate(percentage = as.numeric(str_remove(percentage, "%"))/100,
         managed = "Yes",
         livestock_type = "Feedlot Cattle") %>% 
  crossing(year = 2005:2021)

beef_cattle_mm <- ag_manure_mgmt[3:1603,17:18] %>% 
  row_to_names(1) %>% 
  mutate(state = STATE) %>% 
  filter(state %in% c("MN","WI")) %>% 
  select(-1) %>% 
  pivot_longer(cols = 1, names_to = "mgmt_system", values_to = "percentage") %>% 
  mutate(percentage = as.numeric(str_remove(percentage, "%"))/100,
         managed = "No",
         livestock_type = "Beef Cows") %>% 
  crossing(year = 2005:2021)

swine_mm <- ag_manure_mgmt[3:1603,20:25] %>% 
  row_to_names(1) %>% 
  mutate(year = as.numeric(substr(`Year & State`,1,4)),
         state = substr(`Year & State`,5,6)) %>% 
  filter(state %in% c("MN","WI"), year >=2005) %>% 
  select(-1) %>% 
  pivot_longer(cols = 1:5, names_to = "mgmt_system", values_to = "percentage") %>% 
  mutate(percentage = as.numeric(str_remove(percentage, "%"))/100,
         managed = if_else(mgmt_system %in% c("Daily Spread", "Pasture"), "No", "Yes"),
         livestock_type = "Swine")

layers_mm <- ag_manure_mgmt[3:1603,27:31] %>% 
  row_to_names(1) %>% 
  mutate(year = as.numeric(substr(`Year & State`,1,4)),
         state = substr(`Year & State`,5,6)) %>% 
  filter(state %in% c("MN","WI"), year >=2005) %>% 
  select(-1) %>% 
  pivot_longer(cols = 1:4, names_to = "mgmt_system", values_to = "percentage") %>% 
  mutate(percentage = as.numeric(str_remove(percentage, "%"))/100,
         managed = "Yes",
         livestock_type = "Layers")

turkeys_mm <- ag_manure_mgmt[3:1603,36:38] %>% 
  row_to_names(1) %>% 
  mutate(state = State) %>% 
  filter(state %in% c("MN","WI")) %>% 
  select(-1) %>% 
  pivot_longer(cols = 1:2, names_to = "mgmt_system", values_to = "percentage") %>% 
  mutate(percentage = as.numeric(str_remove(percentage, "%"))/100,
         managed = if_else(mgmt_system %in% c("Range"), "No", "Yes"),
         livestock_type = "Turkeys")%>% 
  crossing(year = 2005:2021)

sheep_mm <- ag_manure_mgmt[3:1603,40:42] %>% 
  row_to_names(1) %>% 
  mutate(state = `Year & State`) %>% 
  filter(state %in% c("MN","WI")) %>% 
  select(-1) %>% 
  pivot_longer(cols = 1:2, names_to = "mgmt_system", values_to = "percentage") %>% 
  mutate(percentage = as.numeric(str_remove(percentage, "%"))/100,
         managed = "Unknown",
         livestock_type = "Sheep") %>% 
  crossing(year = 2005:2021)

goats_mm <- ag_manure_mgmt[3:1603,44:45] %>% 
  row_to_names(1) %>% 
  mutate(state = STATE) %>% 
  filter(state %in% c("MN","WI")) %>% 
  select(-1) %>% 
  pivot_longer(cols = 1, names_to = "mgmt_system", values_to = "percentage") %>% 
  mutate(percentage = as.numeric(str_remove(percentage, "%"))/100,
         managed = "No",
         livestock_type = "Goats")%>% 
  crossing(year = 2005:2021)

manure_mgmt_formatted <- bind_rows(
  dairy_mm,
  heifers_mm,
  feedlot_cattle_mm,
  beef_cattle_mm,
  swine_mm,
  layers_mm,
  turkeys_mm,
  sheep_mm,
  goats_mm
)

manure_mgmt_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "year", class(manure_mgmt_formatted$year), "Year",
    "state", class(manure_mgmt_formatted$state ), "State",
    "mgmt_system", class(manure_mgmt_formatted$mgmt_system), "Number of individual (heads) of livestock type",
    "percentage", class(manure_mgmt_formatted$percentage), "Percentage of animals on management system",
    "managed", class(manure_mgmt_formatted$managed), "Is this a managed manure system or are animals free range?",
    "livestock_type", class(manure_mgmt_formatted$livestock_type), "Livestock classification"
  )

saveRDS(manure_mgmt_formatted, "./_agriculture/data/manure_management_systems.rds")
saveRDS(manure_mgmt_meta, "./_agriculture/data/manure_management_systems_meta.rds")

