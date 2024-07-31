source("R/_load_pkgs.R")
# note that the CSV imports will be replace with more direct connection
# from the SIT tool

# check that needed data are available locally
if(!file.exists("_agriculture/data-raw/ag-module.xlsx")){
  cli::cli_abort("Download agriculture data from MS Team")
}

mcf_system <- readxl::read_xlsx("_agriculture/data-raw/ag-module.xlsx",
                                sheet = "MCF",
                                range = "B3:D53") %>% 
  rename(state = State, Pasture =  `Pasture, Range & Paddocks`) %>% 
  filter(state %in% c("MN", "WI")) %>% 
  mutate(state = if_else(state == "MN", "Minnesota", "Wisconsin")) %>% 
  pivot_longer(cols = -1, names_to = "management_system", values_to = "mcf_percent")

mcf_system_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "state", class(mcf_system$state), "State",
    "management_system", class(mcf_system$management_system), "Manure management system",
    "mcf_percent", class(mcf_system$mcf_percent), "Methane conversion factor (%)"
  )

saveRDS(mcf_system, "./_agriculture/data/methane_conversion_factor_system.rds")
saveRDS(mcf_system_meta, "./_agriculture/data/methane_conversion_factor_system_meta.rds")

mcf_livestock <- readxl::read_xlsx("_agriculture/data-raw/ag-module.xlsx",
                                sheet = "MCF",
                                range = "F3:M1803") %>% 
  mutate(year = as.numeric(substr(`Year & State`,1,4)),
         state = substr(`Year & State`,5,6)) %>% 
  filter(state %in% c("MN","WI")) %>% 
  select(-1) %>% 
  pivot_longer(cols = -c("year","state"), names_to = "livestock_type", values_to = "mcf_percent") %>% 
  mutate(livestock_type = case_when(
    grepl("Swine", livestock_type) ~ "Swine",
    grepl("Feedlot", livestock_type) ~ "Feedlot Cattle",
    TRUE ~ livestock_type
  )) %>% 
  filter(year >= 2005 & year <=2021) %>% 
  group_by(year,state,livestock_type) %>% 
  summarize(mcf_percent = mean(mcf_percent))

#  sheep, goats, calves, and beef cows use the 'pasture' rate of mcf from mcf_system. 
#  Sheep on feed is slightly less but too convoluted and rare to justify excessive effort

mcf_livestock_pasture <-  data.frame(livestock_type = c("Calves", "Beef Cows",
                                                        "Sheep", "Goats"))  %>% 
  crossing(state = c("Minnesota", "Wisconsin")) %>% 
  left_join(., mcf_system %>% 
              filter(management_system == "Pasture") %>% 
              select(-management_system)) %>% 
  crossing(year = 2005:2021)

mcf_constant <- readxl::read_xlsx("_agriculture/data-raw/ag-module.xlsx",
                                  sheet = "MCF",
                                  range = "O5:P7") %>% 
  rename(livestock_type = `Constant MCFS`, mcf_percent = `...2`) %>% 
  { 
    broilers_mcf_percent <- filter(., livestock_type == "Broilers") %>% pull(mcf_percent) # pullets use the same value as broilers but aren't listed elsewhere
    add_row(., livestock_type = "Pullets", mcf_percent = broilers_mcf_percent)
  } %>% 
  crossing(year = 2005:2021,
           state = c("Minnesota","Wisconsin"))

mcf_livestock <- bind_rows(mcf_livestock,
                           mcf_livestock_pasture,
                           mcf_constant)

mcf_livestock_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "year", class(mcf_livestock$year), "Year",
    "state", class(mcf_livestock$state), "State",
    "management_system", class(mcf_livestock$livestock_type), "Formatted livestock classification - matches USDA census labels",
    "mcf_percent", class(mcf_livestock$mcf_percent), "Methane conversion factor (%)"
  )

saveRDS(mcf_livestock, "./_agriculture/data/methane_conversion_factor_livestock.rds")
saveRDS(mcf_livestock_meta, "./_agriculture/data/methane_conversion_factor_livestock_meta.rds")
