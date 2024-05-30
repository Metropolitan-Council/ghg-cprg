source("R/_load_pkgs.R")

### read in state and MSA inventories
mpca_inv <- read_rds("_meta/data/mpca_ghg_inv_2005_2020.RDS")
cprg_inv <- read_rds("_meta/data/cprg_county_emissions.RDS")
cprg_pop <- read_rds("_meta/data/cprg_population.RDS")

comp_pop <- sum(cprg_pop$population[!cprg_pop$NAME %in% c('St. Croix',"Sherburne","Chisago","Pierce")])

cprg_comp <- cprg_inv %>% 
  filter(!geog_name %in% c('St. Croix',"Sherburne","Chisago","Pierce")) %>% 
  group_by(sector, category, source) %>% 
  summarize(co2e = sum(emissions_metric_tons_co2e)) %>% 
  mutate(co2e_per_cap = co2e/comp_pop)

### Transportation

