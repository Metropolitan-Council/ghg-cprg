source("R/_load_pkgs.R")
# source("R/download_read_table.R")
# cprg_county <- readRDS("_meta/data/cprg_county.RDS")
# cprg_county_meta <- readRDS("_meta/data/cprg_county_meta.RDS")
# source("_meta/data-raw/county_geography.R")

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

# 2000-2009 ------
# Using Intercensal year estimates from US Census

# directly download intercensal data years from census.gov
# if they don't already exist
if (!file.exists("_meta/data-raw/population/sub-est00int.csv")) {
  # download directly from census.gov
  download.file("https://www2.census.gov/programs-surveys/popest/datasets/2000-2010/intercensal/cities/sub-est00int.csv",
                destfile = "_meta/data-raw/population/sub-est00int.csv",
                mode = "wb"
  )
}

ctu_pop_intercensal <- read.csv("_meta/data-raw/population/sub-est00int.csv") %>% 
  filter(STATE == 27) %>% 
  mutate(
    ctuid = str_pad(PLACE, 5, pad = "0")
  ) %>% 
  filter(
    ctuid %in% ctu_estimates_2011$ctuid
  )
