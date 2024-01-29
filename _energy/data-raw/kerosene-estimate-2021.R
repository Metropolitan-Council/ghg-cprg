source("R/_load_pkgs.R")
source("R/cprg_geography.R")

# read in efficiency factors
eff_fac <- readxl::read_excel('_energy/data-raw/ghg-emission-factors-hub-2021.xlsx')

### poor formatting but the co2e for kerosene is:
kero_ef <- as.numeric(eff_fac %>% filter(...2 == 'Kerosene') %>% select(...4)) + #CO2 emissions per mmBtu of kerosene used
  as.numeric(eff_fac %>% filter(...2 == 'Kerosene') %>% select(...5)) * as.numeric(eff_fac[6,4]) + #methane emissions per mmBtu kerosene scale to CO2 equivalency
  as.numeric(eff_fac %>% filter(...2 == 'Kerosene') %>% select(...6)) * as.numeric(eff_fac[7,4]) #n20 emissions per mmBtu kerosene scale to CO2 equivalency

# kerosene mmBtu generation estimates are not provided at the state level for MN and WI due to inadequate sample size. Best approximation I can see is regional usage rates, which may skew high
eia2020 <-  readxl::read_excel('_energy/data-raw/eia-recs-region-2020.xlsx')


# these are the estimated per household values of million btu generation for households that use kerosene or fuel oil.
# WI and MN are in different subregions of Midwest, so using different estimates for each of those
mn_kero_use <- as.numeric(eia2020[7,12])
wi_kero_use <- as.numeric(eia2020[6,12])

### look up codes in ACS
load_variables(year = 2020, dataset = "acs5") %>% mutate(concept_short = substr(concept,1,10)) %>% distinct(concept_short) %>% print(n=10000)
#### house heating fuel
v_heat<- load_variables(year = 2021, dataset = "acs5") %>% mutate(concept_short = substr(concept,1,10)) %>% 
  filter(concept_short == "HOUSE HEAT") %>% print(n=10000)

#get number of households in each county using propane
mn_kero_hh <- get_acs(geography = 'county',
                      variables = 'B25040_005',
                      state = 'MN',
                      year = 2021) %>% 
  filter(GEOID %in% cprg_county$GEOID) %>% 
  mutate(mmBtu = estimate * as.numeric(mn_kero_use), #multiply average propane use by household be estimated number of households
         CO2e = mmBtu * kero_ef * 0.001) # multiply mmBtu per county by emissions factor

# repeat for WI
wi_kero_hh <- get_acs(geography = 'county',
                      variables = 'B25040_005',
                      state = 'WI',
                      year = 2021) %>% 
  filter(GEOID %in% cprg_county$GEOID) %>% 
  mutate(mmBtu = estimate * as.numeric(wi_kero_use), #multiply average propane use by household be estimated number of households
         CO2e = mmBtu * kero_ef * 0.001) # multiply mmBtu per county by emissions factor and then convert to metric tonnes

#bind data
kero_county <- rows_append(mn_kero_hh,wi_kero_hh)
kero_county

total_regional_emissions <- sum(kero_county$CO2e) # total regional emissions of the 11 county area

saveRDS(kero_county, "_energy/data-raw/kerosene_use_county.RDS")
