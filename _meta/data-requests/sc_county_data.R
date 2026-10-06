# output st croix county data 

source(file.path(here::here(), "R/_load_pkgs.R"))

scc <- read_rds("_meta/data/cprg_county_emissions.rds") %>% 
  filter(county_name == "St. Croix", emissions_year <= 2022)

# outputs are git-ignored (_meta/data-requests/outputs/)
out_dir <- here::here("_meta", "data-requests", "outputs")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

write_csv(scc, file.path(out_dir, "scc_data_request_2026.csv"))


scc_2022 <- scc %>% 
  filter(emissions_year == 2022)
