source("R/_load_pkgs.R")
source("R/global_warming_potential.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")


# read in efficiency factors
eff_fac <- readxl::read_excel("_energy/data-raw/ghg-emission-factors-hub-2021.xlsx")
# source: https://www.epa.gov/climateleadership/ghg-emission-factors-hub

### poor formatting but the co2e for propane is:
propane_efficiency <- 
  # CO2 emissions per mmBtu of propane used
  as.numeric(eff_fac %>% filter(...2 == "Propane") %>% select(...4)) + 
  # methane emissions per mmBtu propane scale to CO2 equivalency
  as.numeric(eff_fac %>% filter(...2 == "Propane") %>% select(...5)) * gwp$ch4 +
  # n20 emissions per mmBtu propane scale to CO2 equivalency
  as.numeric(eff_fac %>% filter(...2 == "Propane") %>% select(...6)) * gwp$n2o

# source: https://www.eia.gov/consumption/residential/data/2020/state/pdf/ce2.1.st.pdf
eia2020 <- read.csv("_energy/data-raw/eia-recs-2020.csv")


# these are the estimated per household values of million btu generation for households that use propane
mn_prop_use <- eia2020[33, 15]
wi_prop_use <- eia2020[64, 15]


### look up codes in ACS
load_variables(year = 2020, dataset = "acs5") %>%
  mutate(concept_short = substr(concept, 1, 10)) %>%
  distinct(concept_short) %>%
  print(n = 10000)

#### house heating fuel
v_heat <- load_variables(year = 2021, dataset = "acs5") %>%
  mutate(concept_short = substr(concept, 1, 10)) %>%
  filter(concept_short == "HOUSE HEAT") %>%
  print(n = 10000)

# get number of households in each county using propane
mn_prop_hh <- get_acs(
  geography = "county",
  variables = "B25040_003",
  state = "MN",
  year = 2021
) %>%
  filter(GEOID %in% cprg_county$GEOID) %>%
  rowwise() %>% 
  mutate(
    # multiply average propane use per household by estimated number of households
    mmBtu = estimate * as.numeric(mn_prop_use),
    # multiply mmBtu per county by emissions factor
    CO2e = mmBtu * propane_efficiency * 0.001
)

# repeat for WI
wi_prop_hh <- get_acs(
  geography = "county",
  variables = "B25040_003",
  state = "WI",
  year = 2021
) %>%
  filter(GEOID %in% cprg_county$GEOID) %>%
  rowwise() %>% 
  mutate(
    # multiply average propane use by household be estimated number of households
    mmBtu = estimate * as.numeric(wi_prop_use), 
    # multiply mmBtu per county by emissions factor, conver to metric tons
    CO2e = mmBtu * propane_efficiency * 0.001
  ) 

# bind data
prop_county <- rows_append(mn_prop_hh, wi_prop_hh)
prop_county

total_regional_emissions <- sum(prop_county$CO2e) # total regional emissions of the 11 county area

saveRDS(prop_county, "_energy/data-raw/propane_use_county.RDS")
