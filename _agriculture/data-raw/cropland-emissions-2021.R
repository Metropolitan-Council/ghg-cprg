source("R/_load_pkgs.R")
source("R/global_warming_potential.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")

## you'll need an API key to use USDA data (https://quickstats.nass.usda.gov/api)
key <- keyring::key_get('usda_key')

ag_soils_residuals <- read_csv('_agriculture/data-raw/ag_soils_residuals.csv')
ag_soils_fertilizer <- read_csv('_agriculture/data-raw/ag_soils_fertilizer.csv')
ag_soils_manure <- read_csv('_agriculture/data-raw/ag_soils_manure.csv')
ag_residual_burning_ch4 <- read_csv('_agriculture/data-raw/ag_residual_burning_ch4.csv')
ag_residual_burning_n2o <- read_csv('_agriculture/data-raw/ag_residual_burning_n2o.csv')
ag_soils_liming <- read_csv('_agriculture/data-raw/ag_soils_liming.csv')
ag_constants <- read_csv('_agriculture/data-raw/ag_constants.csv')
ag_control <- read_csv('_agriculture/data-raw/ag_control.csv')
ag_fertilizer <- read_csv('_agriculture/data-raw/ag_fertilizer.csv')


### create county names matched to USDA format
counties <- toupper(cprg_county$NAME)
counties <- if_else(counties == 'ST. CROIX', "ST CROIX", counties)


#starting with biggest source of emissions - residuals

usda_survey <- tidyUSDA::getQuickstat(
  sector="CROPS",
  group="FIELD CROPS",
  commodity=NULL,
  category= "PRODUCTION",
  domain= NULL,
  county= counties,
  key = key,
  program = "SURVEY",
  data_item = NULL,
  geographic_level = 'COUNTY',
  year = as.character(2005:2021),
  state = c("MINNESOTA","WISCONSIN"),
  geometry = TRUE,
  lower48 = TRUE, 
  weighted_by_area = T) %>% 
  as.data.frame() %>% select(-geometry)


unique(usda_survey$short_desc)

usda_survey %>% filter(grepl("HAYLAGE ",short_desc))
usda_survey %>% filter(grepl("HAY",short_desc), county_name == "PIERCE") %>% select(year, short_desc, Value) %>% arrange(year)

### extracting crops with conversions from EPA tool (corn and soy will likely account for >90% of emissions)

crop_conversions <- left_join(ag_constants[28:44,1:3] %>% 
  row_to_names(row_number = 1),
  ag_control[c(71,74:89), c(1,2,6,10)] %>% 
    row_to_names(row_number = 1)) %>% 
  mutate_at(c(2:6), as.numeric) %>% 
  clean_names()


usda_survey_formatted <- usda_survey %>% 
  filter(short_desc %in% c(
    "BARLEY - PRODUCTION, MEASURED IN BU",
    "BEANS, DRY EDIBLE, INCL CHICKPEAS - PRODUCTION, MEASURED IN CWT",
    "CORN, GRAIN - PRODUCTION, MEASURED IN BU",
    "HAY, ALFALFA - PRODUCTION, MEASURED IN TONS",
    "OATS - PRODUCTION, MEASURED IN BU",
    "SOYBEANS - PRODUCTION, MEASURED IN BU",
    "WHEAT - PRODUCTION, MEASURED IN BU"
  )) %>% 
  select(year,county_name, short_desc, Value) %>% 
### convert names to match ag conversions
  mutate(crop_type =  case_when(
    grepl("BARLEY", short_desc) ~ "Barley",
    grepl("BEANS", short_desc) ~ "Dry Edible Beans",
    grepl("CORN", short_desc) ~ "Corn for Grain",
    grepl("ALFALFA",short_desc) ~ "Alfalfa",
    grepl("OATS", short_desc) ~ "Oats",
    grepl("SOYBEANS",short_desc) ~ "Soybeans",
    grepl("WHEAT",short_desc) ~ "All Wheat",
    TRUE ~ short_desc
  )) %>% #convert all units to metric tons
  left_join(.,crop_conversions, by = c("crop_type" = "crop")) %>% 
  mutate(metric_tons = case_when(
    grepl("Beans", crop_type) ~ Value * .0454, # 1000s of hundredweights to metric tons
    grepl("Alfalfa", crop_type) ~ Value  * 0.9072, # 1000s of tons to metric tons
    TRUE ~ Value * metric_tons_bushel # 1000s of bushels to metric tons
  )
         ) %>% # determine N delivered to soils
  mutate(MT_N_to_soil = if_else(
    crop_type == "Alfalfa", 
    0,  # no N to soil via residue for alfalfa
    metric_tons * residue_crop_mass_ratio  * residue_dry_matter_fraction * fraction_residue_applied * nitrogen_content_of_residue 
  ),
  MT_N_fixation = if_else(
    crop_type %in% c("Alfalfa", "Soybeans", "Dry Edible Beans"),
    metric_tons * (1 + residue_crop_mass_ratio) * residue_dry_matter_fraction * 0.03, # last value is constant of N content of N-fixer biomass
    0
    ))

usda_survey_formatted %>% filter(year == 2006, crop_type == "Alfalfa") %>% pull(Value) %>% sum()

soil_residue_emissions <- usda_survey_formatted %>% 
  filter(!is.na(MT_N_fixation)) %>% 
  group_by(county_name, year) %>% 
  summarize(mt_n_soils = sum(MT_N_to_soil + MT_N_fixation)) %>% 
  mutate(mt_n2o = mt_n_soils * 0.01 * (44/28),
         mt_c2oe = mt_n2o * gwp$n2o)

soil_residue_emissions %>% filter(year == 2021) %>% pull(mt_n2o) %>% sum()
# 450056.4 

ggplot(soil_residue_emissions %>% 
         group_by(year,county_name) %>% 
         summarize(CO2e = sum(mt_c2oe)),
       aes(x = year, y = CO2e, col = county_name)) + geom_line(size = 1.5) + theme_bw()
# quite a bit of scatter - 2013 in particular has major dip that should be investigated
soil_residue_emissions %>% filter(year == 2021) %>%  pull(mt_c2oe) %>% sum()

#### fertilizer data ####
### some creativity is required here. 
### Fertilizer purchases are recorded by MN Dept of Ag - waiting to see if data exists outside of pdfs
### Fertilizer expenses are recorded in USDA 5 year census
### Fertilizer application is estimated in SIT for state, but data source is unclear

### need to extract state fertilizer application estimates by year and then multiply by year constants to convert to organic vs synthetic rates
ag_fert_formatted <- left_join(ag_fertilizer[c(2,4:53),] %>% 
  mutate(`Consumption of Primary Plant Nutrients: Total Nitrogen (Metric Tons)` = replace(`Consumption of Primary Plant Nutrients: Total Nitrogen (Metric Tons)`, 
                        is.na(`Consumption of Primary Plant Nutrients: Total Nitrogen (Metric Tons)`), 
                        "state")) %>%
  row_to_names(1) %>% 
  pivot_longer(cols = -1,
               names_to = "year",
               values_to = "value") %>%
  filter(state %in% c ("MN","WI")) %>% 
  mutate(value = as.numeric(str_replace_all(value,",","")),
         year = as.numeric(year)),
  ag_fertilizer[c(2,56),2:33] %>% 
    row_to_names(1) %>% 
    pivot_longer(cols = 1:32,
                 names_to = "year",
                 values_to = "percent_synthetic") %>% 
    mutate(percent_synthetic = as.numeric(str_replace_all(percent_synthetic,"%",""))/100,
           year = as.numeric(year)),
  by = 'year') %>% 
  mutate(mt_n_synthetic = value * percent_synthetic,
         mt_n_organic = value *(1 - percent_synthetic)) %>% 
  filter(!is.na(value))

ag_fert_formatted


usda_fertilizer_mn_wi <- tidyUSDA::getQuickstat(
  sector="ECONOMICS",
  group="EXPENSES",
  commodity="FERTILIZER TOTALS",
  category= "EXPENSE",
  domain= NULL,
  county= NULL,
  key = key,
  program = "CENSUS",
  data_item = "FERTILIZER TOTALS, INCL LIME & SOIL CONDITIONERS - EXPENSE, MEASURED IN $",
  geographic_level = 'COUNTY',
  year = as.character(2002:2022),
  state = c("MINNESOTA","WISCONSIN"),
  geometry = TRUE,
  lower48 = TRUE, 
  weighted_by_area = T) %>% 
  as.data.frame() %>% select(-geometry)

usda_fertilizer_mn_wi %>% filter(is.na(Value)) %>% arrange(year) %>% select(state_name, county_name, year)
### there are 9 missing values by county. 2 MN, 2 WI in 2002. 2 MN in 2007, 3 WI in 2017. None are counties of focus except Ramsey in 2002, which has limited ag.

usda_fert_prop <- usda_fertilizer_mn_wi %>% 
  filter(!(NAME == "Washington" & state_name == "WISCONSIN")) %>% 
  group_by(state_name, year) %>% 
  mutate(state_total = sum(Value, na.rm = TRUE), fert_prop = Value / state_total) %>% 
  filter(county_name %in% counties) %>% 
  select(year, county_name, fert_prop)

### expand grid and interpolate

fert_prop_interpolated <- left_join( # this creates an empty grid of all year county_name possibilities from 2002-2022
  expand.grid(
    year = seq(2002, 2022, by = 1),
    county_name = unique(usda_fert_prop$county_name)
  ),
  usda_fert_prop) %>%  # merged with our populated fertilizer proportion data, it creates NAs wherever data is missing
  mutate(fert_prop = if_else(year %in% c(2002,2007,2012,2017,2022) & is.na(fert_prop),0, fert_prop)) %>%  # add 0 as Ramsey value in 2002
  group_by(county_name) %>%
  arrange(year) %>%
  mutate(
    fert_prop = zoo::na.approx(fert_prop, na.rm = FALSE), # this function linearly interpolates NAs between known values, following the group_by
    data_type = ifelse(year %in% c(2002,2007,2012,2017,2022), 'census', 'interpolated') # marking whether values are from the census or interpolation
  )

#### merge fertilize proportion estimates with state fertilizer values

county_fertilizer_emissions <- left_join(
  left_join(fert_prop_interpolated, cprg_county %>% 
                                 mutate(county_name = if_else(NAME == "St. Croix",
                                                              "ST CROIX",
                                                              toupper(NAME))) %>% 
                                 as.data.frame() %>% 
                                        select(county_name, STATE_ABB)),
  ag_fert_formatted,
  by = c("STATE_ABB" = "state", "year" = "year")) %>% 
  filter(year >= 2005 & year <= 2021) %>% 
  mutate(mt_n_synthetic_cty = fert_prop * mt_n_synthetic,
         mt_n_organic_cty = fert_prop * mt_n_organic) %>% 
  #### separating mutate as this is where we will calculate emissions from estimated fertilizer application
  mutate(n2o_direct = (mt_n_synthetic_cty * (1 - as.numeric(ag_constants[21,1])) + # unvolatilized N from synthetic fertilizer
                      mt_n_organic_cty * as.numeric(ag_constants[19,1]) * (1 - as.numeric(ag_constants[20,1]))) * # unvolatilized N from organic fertilizer
                      as.numeric(ag_constants[22,1]) * as.numeric(ag_constants[10,1]), ## multiplied by the EF of unvolatilized N and N2O N: N2O
         n2o_indirect = (mt_n_synthetic_cty * as.numeric(ag_constants[21,1]) + # volatilized N from synthetic fertilizer
                      mt_n_organic_cty * as.numeric(ag_constants[19,1]) * as.numeric(ag_constants[20,1])) * # volatized N from organic fertilizer
                      as.numeric(ag_constants[23,1]) * as.numeric(ag_constants[10,1]),  ## multiplied by the EF of volatized N and N2O N: N2O
         mt_n2o = n2o_direct + n2o_indirect,
         mt_co2e = mt_n2o * gwp$n2o) %>% 
  select(year, county_name, data_type, mt_n_synthetic, mt_n_organic, mt_n2o, mt_co2e)

ggplot(county_fertilizer_emissions %>% 
         group_by(year,county_name) %>% 
         summarize(CO2e = sum(mt_co2e)),
       aes(x = year, y = CO2e, col = county_name)) + geom_line(size = 1.5) + theme_bw()
county_fertilizer_emissions %>% filter(year == 2021, !county_name %in% c("SHERBURNE", "CHISAGO", "PIERCE", "ST CROIX")) %>%  pull(mt_co2e) %>% sum()



### soil animals
### ag soils animals pulls N2o emissions from x variables:
# 1) fertilizer runoff/leaching - this was calculated above and needs to be multiplied by EF
# 2) manure runoff/leaching - this is calculated from a K-nitrogen excretion that is derived from animal emissions. will need to pull in livestock data
# 3) indirect n2o emissions from livestock - this is a fraction of the k-nitrogen excretion calculated in step 2
# 4) direct n2o emissions from livestock - this is manure applied to ag soils and left in pasture/paddocks
