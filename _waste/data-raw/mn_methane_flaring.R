# pull in and calculate f_rec ----

# Source: EPA State Inventory Solid Waste tool, methane flaring data 

# flaring_natl_data <- read_csv(file.path(here::here(), "_waste/data-raw/solid-waste-module-flaring.csv"))%>%  
#   row_to_names(row_number = 1) 
# flaring_natl_data <- data.frame(t(flaring_natl_data)) %>%  
#   row_to_names(row_number = 1) 


flaring_data <- readxl::read_xlsx("_waste/data-raw/solid_waste_flaring.xlsx",
                                  range = "A2:AG54") %>% 
  rename(State = 1) %>% 
  filter(State == "MN") 


flaring_data <- data.frame(t(flaring_data)) %>% 
  tibble::rownames_to_column("Year") %>% 
  rename(flared_mmt_ch4_mn = t.flaring_data.)

flaring_data <- flaring_data[-1,]

# need to:
# multiply by population percentages to scale down
# save both flaring and lfgte

lfgte_data <- readxl::read_xlsx("_waste/data-raw/solid_waste_lfgte.xlsx",
                                  range = "A2:AG54") %>% 
  rename(State = 1) %>% 
  filter(State == "MN") 

lfgte_data <- data.frame(t(lfgte_data)) %>% 
  tibble::rownames_to_column("Year") %>% 
  rename(lfgte_mmt_ch4_mn = t.lfgte_data.)

lfgte_data <- lfgte_data[-1,]
