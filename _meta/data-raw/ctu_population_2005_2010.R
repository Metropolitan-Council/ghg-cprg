source("R/_load_pkgs.R")

# ctu_estimates <- readxl::read_xls("_meta/data-raw/2009PopulationEstimates.xls") %>% 
#   select(
#     1:3
#   )
# ctu_list <- vector(mode = "list", length = 8)
# 
# ctu_list[[1]] <- slice(ctu_estimates, 5:26) %>%
#   mutate(county = "Anoka")
# see clean_tabula_tables
# is this the best way to do it? maybe not but it's how i'm doing it
# this section on hold until we can hook it up to FRED

source("N:/CommDev/Research/Research/FRED/passwords.R")

ctu_estimates_2005 <- councilR::import_from_FRED("COMMUNITY_DESIGNATION")

# 2011-2019
ctu_estimates_2011 <- readxl::read_xlsx("_meta/data-raw/IntercensalEstimates.xlsx") %>% 
  mutate(
    geoid = paste0("27", COUNTY_CODE)
    ) %>% 
  select(
    countyfp = COUNTY_CODE,
    geoid,
    ctuid = CTU_CODE,
    year = EST_YEAR,
    ctu_population = POPTOTAL_EST
  ) %>%
  group_by(
    countyfp, geoid, year
  ) %>% 
  mutate(
    county_population = sum(ctu_population)
  ) %>% 
  ungroup() %>% 
  mutate(
    ctu_proportion_of_county_pop = ctu_population/county_population
  )

