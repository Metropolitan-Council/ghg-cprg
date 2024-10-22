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
    geoid,
    ctuid = CTU_CODE,
    year = EST_YEAR,
    ctu_population = POPTOTAL_EST
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
    ctuid = str_pad(PLACE, 5, pad = "0"),
    geoid = paste0(STATE, str_pad(COUNTY, 3, pad = "0"))
  ) %>% 
  filter(
    ctuid %in% ctu_estimates_2011$ctuid,
    geoid %in% ctu_estimates_2011$geoid,
    SUMLEV == 157
  ) %>% 
  select(
    geoid,
    ctuid,
    POPESTIMATE2000:CENSUS2010POP
  ) %>% 
  pivot_longer(
    cols = !c(ctuid, geoid),
    names_to = "year",
    names_prefix = "POPESTIMATE",
    values_to = "ctu_population"
  ) %>% 
  mutate(
    year = if_else(
      year == "CENSUS2010POP", 2010, as.integer(year)
    ) # note on error: no they're not
  )

# join

ctu_pop_estimates <- ctu_pop_intercensal %>% 
  rbind(ctu_estimates_2011) %>%
  group_by(
    geoid, year
  ) %>% 
  mutate(
    county_population = sum(ctu_population)
  ) %>% 
  ungroup() %>% 
  mutate(
    ctu_proportion_of_county_pop = ctu_population/county_population
  )

## test
# pop_summary <- ctu_pop_estimates %>% 
#   group_by(year, geoid) %>% 
#   summarize(pop_totals = sum(ctu_population))
# 
# ggplot(pop_summary, aes(x = year, y = pop_totals, color = geoid)) +
#   geom_line()

ctu_pop_meta <- tribble(
  ~Column, ~Class, ~Description,
  "geoid", class(ctu_pop_estimates$geoid), "GEOID tag for MN county",
  "ctuid", class(ctu_pop_estimates$ctuid), "CTU census tag",
  "year", class(ctu_pop_estimates$year), "Population year between 2000 and 2020",
  "ctu_population", class(ctu_pop_estimates$ctu_population),
  "Population of CTU in given year",
  "county_population", class(ctu_pop_estimates$county_population), 
  "Population of county in given year",
  "ctu_proportion_of_county_pop", class(ctu_pop_estimates$ctu_proportion_of_county_pop),
  "Percentage of county population atttributed to this CTU in the given year"
)
  
saveRDS(ctu_pop_estimates, file.path(here::here(), "_meta/data/ctu_population.RDS"))
saveRDS(ctu_pop_meta, file.path(here::here(), "_meta/data/ctu_population_meta.RDS"))
