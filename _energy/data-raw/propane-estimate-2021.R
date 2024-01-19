# install.packages('tigris')
# install.packages('tidycensus')
library(tigris)
library(ggplot2)
library(tidyverse)
library(tidycensus)

### replace with cprg_county from R -> data
# load 11 counties
mn_counties <- counties('MN') %>% 
  filter(NAME %in% c('Hennepin','Ramsey','Anoka','Dakota','Carver','Sherburne','Chisago','Scott','Washington'))

wi_counties <- counties('WI') %>% 
  filter(NAME %in% c('St. Croix','Pierce'))

#bind MN and WI together
all_counties <- rows_append(wi_counties,mn_counties) 


prop_ef <- 62.87 + (3.0 * 25) + (0.6 * 298) # CO2, CH4, N2O converted to CO2 equivalent - kg CO2 per mmBtu
# source: https://www.epa.gov/sites/default/files/2021-04/documents/emission-factors_mar2020.pdf

mn_prop_use <- 24000000 # 24 trillion in units of mmBtu (millions BTU) - 2020
wi_prop_use <- 20000000 # 20 trillion in units of mmBtu (millions BTU) - 2020
# source: https://www.eia.gov/consumption/residential/data/2020/state/pdf/ce2.1.st.pdf

### look up codes in ACS
load_variables(year = 2020, dataset = "acs5") %>% mutate(concept_short = substr(concept,1,10)) %>% distinct(concept_short) %>% print(n=10000)
#### house heating fuel
v_heat<- load_variables(year = 2021, dataset = "acs5") %>% mutate(concept_short = substr(concept,1,10)) %>% 
  filter(concept_short == "HOUSE HEAT") %>% print(n=10000)

v2020 <- load_variables(year = 2020, dataset = "pl")

#get number of households in state using propane
mn_prop_hh <- get_acs(geography = 'state',
        variables = 'B25040_003',
        state = 'MN',
        year = 2021) %>% 
  select(estimate)

wi_prop_hh <- get_acs(geography = 'state',
                      variables = 'B25040_003',
                      state = 'WI',
                      year = 2021) %>% 
  select(estimate)


mn_prop_county <- get_acs(geography = 'county',
        variables = 'B25040_003',
        state = 'MN',
        year = 2021) %>% 
  mutate(hh_perc = estimate/as.numeric(mn_prop_hh),
         mmBtu = hh_perc*mn_prop_use,
         co2e = mmBtu * prop_ef) %>% 
  filter(GEOID %in% all_counties$GEOID)
  
mn_prop_county

wi_prop_county <- get_acs(geography = 'county',
                            variables = 'B25040_003',
                            state = 'WI',
                            year = 2021) %>% 
  mutate(hh_perc = estimate/as.numeric(wi_prop_hh),
         mmBtu = hh_perc*wi_prop_use,
         co2e = mmBtu * prop_ef) %>% 
  filter(GEOID %in% all_counties$GEOID)

wi_prop_county

prop_county <- rows_append(mn_prop_county,wi_prop_county)
write_csv(prop_county,
          './CPRG/prop-est-2021.csv')

total_regional_emissions <- sum(prop_county$co2e) # kg CO2 equivalency
total_regional_emissions * 0.001 # metric tonnes CO2 equivalency

mc_emissions <- sum(prop_county$co2e[!prop_county$GEOID %in% c(27025,27141,55093,55109)])
mc_emissions  * 0.001
## this is ~4x higher than 2018 GHG estimates for region, though is very similar if only using CO2 emission factor (e.g.ignore methane and N2O)
## I'm not sure which EIA the 2018 estimate uses, but there's a ~75% increases in estimated propane usage from 2015 to 2020 in midwest (covid?)

county_prop_use

sum(rural_mn$rh_perc)

### need to get estimates from EIA RECS on
### - estimated number of households using propane in midwest
### -- breakdown of rural vs urban
### - avg gallons used per household using
### - 

## Midwest census region has 2.72 million housing units that use propane
## - 1.09 mil in west north central midwest (includes MN)
## - 1.62 mil in east north central midwest (includes WI)

