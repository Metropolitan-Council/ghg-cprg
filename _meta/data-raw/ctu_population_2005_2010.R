source("R/_load_pkgs.R")

# Matt working on 2009-2010

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

