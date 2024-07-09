# state population data ------
# create population timeseries for state level back to the year 2000

source("R/_load_pkgs.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")


county_geography <- bind_rows(tigris::counties(state = "MN"),
                              tigris::counties(state = "WI"))
library(tidycensus)

# 2001-2009 ----
# 2001-2009: intercensal year table
# directly download intercensal data years from census.gov
# if they don't already exist, make a folder 
if(!file.exists("_meta/data-raw/population/co-est00int-01-27.xls")){
  fs::dir_create("_meta/data-raw/population/")
  download.file("https://www2.census.gov/programs-surveys/popest/tables/2000-2010/intercensal/county/co-est00int-01-27.xls",
                destfile = "_meta/data-raw/population/co-est00int-01-27.xls")
  download.file("https://www2.census.gov/programs-surveys/popest/tables/2000-2010/intercensal/county/co-est00int-01-55.xls",
                destfile = "_meta/data-raw/population/co-est00int-01-55.xls")}



county_pop_intercensal1 <- 
  # start with Minnesota
  readxl::read_xls("_meta/data-raw/population/co-est00int-01-27.xls",
                   skip = 3) %>% 
  mutate(NAMELSAD = `...1`) %>% 
  # remove state total row
  filter(stringr::str_detect(NAMELSAD, "County")) %>% 
  # we will use the official 2000 and 2010 (April 1) estimates
  # and the July 1 estimates for all intercensal years
  select(NAMELSAD, everything(), 
         -`2000`,
         `2000` = `...2`,
         -`...1`,
         `2010` = `...14`,
         -`...13`) %>% 
  mutate(NAMELSAD = stringr::str_sub(NAMELSAD, start =2, end = -1),
         STATE = "Minnesota") %>%
  bind_rows(
    readxl::read_xls("_meta/data-raw/population/co-est00int-01-55.xls",
                     skip = 3) %>% 
      mutate(NAMELSAD = `...1`) %>% 
      filter(stringr::str_detect(NAMELSAD, "County")) %>% 
      select(NAMELSAD, everything(), 
             -`2000`,
             `2000` = `...2`,
             -`...1`,
             `2010` = `...14`,
             -`...13`) %>% 
      mutate(NAMELSAD = stringr::str_sub(NAMELSAD, start =2, end = -1),
             STATE = "Wisconsin")) %>% 
  group_by(NAMELSAD, STATE) %>% 
  pivot_longer(cols = 2:12,
               names_to = "population_year",
               values_to = "population") %>% 
  mutate(population_data_source = ifelse(population_year %in% c(2000, 2010), 
                                         "US Decennial Census",
                                         "US Census County Intercensal Tables (CO-EST00INT-01 -27 and -55)")) %>% 
  left_join(cprg_county %>% 
              sf::st_drop_geometry(),
            by = join_by(NAMELSAD, STATE))

# state_pop_intercensal <- county_pop_intercensal %>% 
#   group_by(STATE, population_year, population_data_source) %>% 
#   summarize(population = sum(population, na.rm = T))

# 2011-2019 -----
# 2011-2019: Intercensal table

if(!file.exists("_meta/data-raw/population/co-est2020.csv")){
  fs::dir_create("_meta/data-raw/population/")
  download.file("https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/counties/totals/co-est2020.csv",
                destfile = "_meta/data-raw/population/co-est2020.csv")
}

county_pop_intercensal2 <- read_csv("_meta/data-raw/population/co-est2020.csv",
                                    col_types = c(
                                      rep("c", 7),
                                      rep("d", 14)
                                    )) %>% 
  clean_names() %>% 
  filter(stname %in% state_pop_intercensal$STATE,
         ! ctyname %in% state_pop_intercensal$STATE) %>% 
  mutate(census2010pop = as.numeric(census2010pop)) %>% 
  pivot_longer(cols = 8:21,
               names_to = "population_source_year",
               values_to  = "population") %>% 
  mutate(GEOID = paste0(state, county),
         STATE = stname,
         NAMELSAD = ctyname,
         NAME  = str_remove(ctyname, " County"),
         population_year = str_extract(population_source_year, "[:digit:][:digit:][:digit:][:digit:]"),
         population_data_source = ifelse(population_year %in% c(2000, 2010, 2020), 
                                         "US Decennial Census",
                                         "US Census County Intercensal Tables (CO-EST2020)")
  ) %>% 
  select(-1:-3) %>% 
  filter(!population_source_year %in% c("estimatesbase2010",
                                        "popestimate2010"))
left_join(cprg_county %>% 
            sf::st_drop_geometry())


county_pop_intercensal <- bind_rows(county_pop_intercensal1, 
                                    county_pop_intercensal2) %>% 
  arrange(population_year) %>% 
  select(1:5, GEOID) %>% 
  filter(population_year != "0420")

# 2000, 2010, 2020 -----
# Decennial census years


fetch_combine_decennial <- function(state_name){
  
  x2020 <- tidycensus::get_decennial(
    geography = "county",
    state = state_name,
    year = 2020,
    variables = c(total_pop = "P1_001N")
  ) %>% 
    mutate(population_year = "2020",
           population_data_source = "Decennial Census, Table P1",
           state = state_name,
           population = value)
  
  
  
  x2010 <- get_decennial(
    geography = "county",
    state = state_name,
    year = 2010,
    variables = c(total_pop = "P010001")
  ) %>% 
    mutate(population_year = "2010",
           population_data_source = "Decennial Census, Table P1",
           state = state_name,
           population = value)
  
  x2000 <- get_decennial(
    geography = "county",
    year = 2000,
    state = state_name,
    variable = c(total_pop = "P001001")
  ) %>% 
    mutate(population_year = "2000",
           population_data_source = "Decennial Census, Table P1",
           state = state_name,
           population = value)
  
  bind_rows(x2000,
            x2010,
            x2020) %>% 
    return()
  
}

county_pop_decennial <- bind_rows(
  fetch_combine_decennial("Minnesota"),
  fetch_combine_decennial("Wisconsin")) %>% 
  select(GEOID, population, population_data_source, population_year)

county_pop_intercensal %>% 
  filter(!population_year %in% county_pop_decennial$population_year) %>%
  bind_rows(county_pop_decennial) %>% 
  left_join(county_geography)
