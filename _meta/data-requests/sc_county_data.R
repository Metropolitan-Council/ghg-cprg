# output st croix county data 

source(file.path(here::here(), "R/_load_pkgs.R"))

scc <- read_rds("_meta/data/cprg_county_emissions.rds") %>% 
  filter(county_name == "St. Croix", emissions_year <= 2022)


scc_2022 <- scc %>% 
  filter(emissions_year == 2022)
