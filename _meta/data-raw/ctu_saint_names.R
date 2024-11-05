source("R/_load_pkgs.R")

# first, find all place names in the state that have 
# "St. " in their name
mn_sts <- tigris::places(state = "MN") %>% 
  sf::st_drop_geometry() %>% 
  select(NAME) %>% 
  unique() %>% 
  filter(stringr::str_detect(NAME, "St. ")) %>% 
  extract2("NAME") %>% 
  sort()

# index of all the Saints
# convert our Saint places from "St. " to "Saint"
saints <- mn_sts %>% 
  stringr::str_replace("St. ", "Saint ")

# make our alternate list of Saint cities, 
# 
saint_alternates <- saints %>% 
  stringr::str_replace("Saint ", "St ")
