source("R/_load_pkgs.R")
source("R/cprg_geography.R")


# read in efficiency factors
eff_fac <- readxl::read_excel('_energy/data-raw/ghg-emission-factors-hub-2021.xlsx')
# source: https://www.epa.gov/climateleadership/ghg-emission-factors-hub
### poor formatting but the co2e for propane is:
prop_ef <- as.numeric(eff_fac %>% filter(...2 == 'Propane') %>% select(...4)) + #CO2 emissions per mmBtu of propane used
  as.numeric(eff_fac %>% filter(...2 == 'Propane') %>% select(...5)) * as.numeric(eff_fac[6,4]) + #methane emissions per mmBtu propane scale to CO2 equivalency
  as.numeric(eff_fac %>% filter(...2 == 'Propane') %>% select(...6)) * as.numeric(eff_fac[7,4]) #n20 emissions per mmBtu propane scale to CO2 equivalency

eia2020 <- read.csv('_energy/data-raw/eia-recs-2020.csv')
# source: https://www.eia.gov/consumption/residential/data/2020/state/pdf/ce2.1.st.pdf


# these are the estimated per household values of million btu generation for households that use propane
mn_prop_use <- eia2020[33,15]
wi_prop_use <- eia2020[64,15]


### look up codes in ACS
load_variables(year = 2020, dataset = "acs5") %>% mutate(concept_short = substr(concept,1,10)) %>% distinct(concept_short) %>% print(n=10000)
#### house heating fuel
v_heat<- load_variables(year = 2021, dataset = "acs5") %>% mutate(concept_short = substr(concept,1,10)) %>% 
  filter(concept_short == "HOUSE HEAT") %>% print(n=10000)

#get number of households in each county using propane
mn_prop_hh <- get_acs(geography = 'county',
        variables = 'B25040_003',
        state = 'MN',
        year = 2021) %>% 
  filter(GEOID %in% cprg_county$GEOID) %>% 
  mutate(mmBtu = estimate * as.numeric(mn_prop_use), #multiply average propane use by household be estimated number of households
         CO2e = mmBtu * prop_ef * 0.001) # multiply mmBtu per county by emissions factor

# repeat for WI
wi_prop_hh <- get_acs(geography = 'county',
                      variables = 'B25040_003',
                      state = 'WI',
                      year = 2021) %>% 
  filter(GEOID %in% cprg_county$GEOID) %>% 
  mutate(mmBtu = estimate * as.numeric(wi_prop_use), #multiply average propane use by household be estimated number of households
         CO2e = mmBtu * prop_ef * 0.001) # multiply mmBtu per county by emissions factor and then convert to metric tonnes

#bind data
prop_county <- rows_append(mn_prop_hh,wi_prop_hh)
prop_county

total_regional_emissions <- sum(prop_county$CO2e) # total regional emissions of the 11 county area

saveRDS(prop_county, "_energy/data-raw/propane_use_county.RDS")
