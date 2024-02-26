source(file.path(here::here(), "R/_load_pkgs.R"))

# read in csvs generated at https://cfpub.epa.gov/ghgdata/inventoryexplorer/#waste/entiresector/allgas/category/all

epa_wi <- read_csv(file.path(here::here(), "_waste/data-raw/epa_inventory_wi.csv")) %>%
  mutate(values = `2021`*10^6) %>% 
  select("Category" = "Wisconsin Emissions, Waste Management, MMT CO2 eq.",
         "values") %>% 
  pivot_wider(names_from = "Category", values_from = "values") %>% 
  mutate("STATE" = "Wisconsin",
         "STATEFP" = "55")
  
epa_mn <- read_csv(file.path(here::here(), "_waste/data-raw/epa_inventory_mn.csv")) %>% 
  mutate(values = `2021`*10^6) %>% 
  select("Category" = "Minnesota Emissions, Waste Management, MMT CO2 eq.",
         "values") %>% 
  pivot_wider(names_from = "Category", values_from = "values") %>% 
  mutate("STATE" = "Minnesota",
         "STATEFP" = "27")

epa_all <- rbind(epa_wi, epa_mn)

# allocate to counties by pop

cprg_county_proportions <- readRDS(file.path(here::here(),"_meta/data/cprg_county_proportions.RDS"))

federal_inventory_waste_allocated <- cprg_county_proportions %>%
  filter(year == 2021) %>% 
  left_join(epa_all) %>% 
  mutate(
    "Landfill" = Landfills*county_proportion_of_state_pop,
    "Compost" = Composting*county_proportion_of_state_pop,
    "Anaerobic Digestion" = `Anaerobic digestion`*county_proportion_of_state_pop,
    "Wastewater" = `Wastewater treatment`*county_proportion_of_state_pop,
    "Total" = Total*county_proportion_of_state_pop
  ) %>% 
  select(
    "STATE",
    "NAME",
    "year",
    "Landfill",
    "Compost",
    "Anaerobic Digestion",
    "Wastewater",
    "Total"
  )

