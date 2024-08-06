# This script will reorganize our 2021 final inventory into the Global Covenant 
# of Mayors Common Reporting Framework, including activity data and emissions 
# factors as required. The GCOM inventory will cover the year 2021 (only) and the 
# 7-county region.
source("R/_load_pkgs.R")

county_emissions <- readRDS(file.path(here::here(), "_meta/data/cprg_county_emissions.RDS"))

met_counties <- c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington")

county_emissions <- county_emissions %>% 
  filter(geog_name %in% met_counties,
         year == 2021)
# once naming conventions/what's included is finalized: 
# make sure activity data and factors are included
# modify categories eurgh
# sum to total region
