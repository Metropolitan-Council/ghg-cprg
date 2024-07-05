source("R/_load_pkgs.R")
source("R/global_warming_potential.R")


cprg_county <- readRDS("_meta/data/cprg_county.RDS")

### load in EPA Ag SIT csvs
enteric <- read_csv('_agriculture/data-raw/enteric-fermentation.csv')
manure_n2o <- read_csv('_agriculture/data-raw/manure-n2o.csv')
manure_ch4 <- read_csv('_agriculture/data-raw/manure-ch4.csv')
nex <- read_csv("_agriculture/data-raw/ag_nex.csv")
ag_control <- read_csv('_agriculture/data-raw/ag_control.csv')

### mpca data on feedlots - currently unused but formatted at end of script
mn_feedlots <- read_csv('_agriculture/data-raw/mn-feedlots.csv') %>% 
  filter(county_name %in% cprg_county$NAME)

## you'll need an API key to use USDA data (https://quickstats.nass.usda.gov/api)
key <- keyring::key_get('usda_key')

### create county names matched to USDA format
counties <- toupper(cprg_county$NAME)
counties <- if_else(counties == 'ST. CROIX', "ST CROIX", counties)


#### Survey data ####
### this is an API to get livestock data (mammals) from the USDA.
### USDA has yearly survey data that provides heads of animals - only cattle appear to be available after 2013.
### USDA also has 5 year census data with more detailed information (2007, 2012, 2017, 2022). Census data will be needed for hogs, sheep, and feedlots
### Cattle survey and census data should be compared in 2017 and 2022
usda_survey <- tidyUSDA::getQuickstat(
  sector="ANIMALS & PRODUCTS",
  group="LIVESTOCK",
  commodity=NULL,
  category= "INVENTORY",
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
  weighted_by_area = T)


### pull out usable data
usda_cattle <- usda_survey %>% 
  filter(!is.na(Value), # exclude missing values, this omits Ramsey county (the UofM has some cattle, but this is low importance)
         commodity_desc == "CATTLE",  # only CATTLE extends to 2021 for survey data, will use census data and interpolation for other livestock
         !grepl("FEED", short_desc)) %>%  ## feedlot data also stops in 2013, so going to use census
  as.data.frame() %>% select(-geometry) #Don't need this to be a spatial object

## there is no non-cattle survey data past 2013. Will need to use inventory data to in-fill other livestock
unique(usda_cattle$short_desc)

#### need to standardize livestock category naming between enteric fermentation data and USDA livestock head count data
enteric_formatted <- enteric  %>% 
  select(c(2,3,6)) %>% ### these columns are year, livestock type, and emission factor. The latter varies across livestock type AND year
  setNames(c("Year","Livestock","Emission_factor_kg_ch4_per_head")) %>%  # provide readable names
  filter(!(is.na(Year) | is.na(Emission_factor_kg_ch4_per_head))) %>% ## bad formatting from Excel leaves many unusable rows
  filter(Year >= 2005) %>% #as per our baseline
  mutate(Emission_factor_kg_ch4_per_head = as.numeric(Emission_factor_kg_ch4_per_head))

## check
unique(enteric_formatted$Livestock)

### USDA data has heads of dairy cattle, beef cattle, and 'cattle including calves' - My interpretation is last category double counts previous
### EPA enteric fermentation has many categories, and multiple estimates for 'replacements' (calves). Taking the average of calves as they are similar

#### change names as needed and summarize
enteric_agg <- enteric_formatted %>% 
  filter(!grepl("mos.",Livestock)) %>% 
  mutate(livestock_type = 
           case_when(
             grepl("Replacement", Livestock) ~ "Calves", ### enteric data differs between age classes of calves but USDA not, so taking average value
             grepl("Feedlot",Livestock) ~ "Feedlot Cattle", ### all feedlot cattle type will be classified as same
             TRUE ~ Livestock
           )) %>% 
  group_by(Year, livestock_type) %>% 
  summarize(kg_ch4_per_head = mean(Emission_factor_kg_ch4_per_head ))

unique(enteric_agg$livestock_type)
unique(usda_cattle$short_desc)

### match livestock labels to enteric and summarize to county-year-livestockType
usda_cattle_agg <- usda_cattle %>%
  mutate(livestock_type = 
           case_when( 
             grepl("MILK", short_desc) ~ "Dairy Cows",
             grepl("BEEF", short_desc) ~ "Beef Cows",
             grepl("FEED", short_desc) ~ "Feedlot Cattle",
             grepl("CALVES",short_desc) ~ "Calves",
             grepl("HOGS", short_desc) ~ "Swine",
             grepl("SHEEP",short_desc) ~ "Sheep",
             TRUE ~ short_desc
           )) %>% 
  group_by(year, county_name, livestock_type) %>% 
  summarise(head_count = sum(Value))


### Calf category appears to be all cattle plus calves. The documentation is scarce, but subtracting away adult cattle appears to be correct.
### Update this matched county generated reports, validating this approach
usda_cattle_corrected <- left_join(usda_cattle_agg,
                      usda_cattle_agg %>%
  filter(grepl("Cows$", livestock_type)) %>%  ## grab any non-calf categories (contains cows- no bull or steer data)
  group_by(year, county_name) %>%
  summarise(cows_sum = sum(head_count)),
  by =  c("year", "county_name")) %>%
  mutate(head_count = ifelse(livestock_type == "Calves", head_count - cows_sum, head_count)) %>% ### subtract the adults from the combined category to leave calves
  select(-cows_sum)

### merge enteric cattle data with head count survey data
cow_burps <- left_join(usda_cattle_corrected, enteric_agg, by = c("year" = "Year", "livestock_type" = "livestock_type")) %>% 
  mutate(kg_ch4 = kg_ch4_per_head * head_count, # total kg methane from livestock_type
         MT_ch4 = kg_ch4 / 1000, # convert kg to metric tons 
         CO2e = MT_ch4 * gwp$ch4) # convert methane to CO2e

cow_burps_survey <- cow_burps %>% 
  select(-kg_ch4)

# create metadata
cow_burps_survey_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "year", class(cow_burps$year), "Year of survey",
    "county_name", class(cow_burps$county_name), "County name",
    "livestock_type", class(cow_burps$livestock_type), "Livestock classification",
    "head_count", class(cow_burps$head_count), "Number of individual (heads) of livestock type",
    "kg_ch4_per_head", class(cow_burps$kg_ch4_per_head), "Methane emission factor per head for specific livestock_year combination",
    "MT_ch4", class(cow_burps$MT_ch4), "Total metric tons of methane emissions from livestock/year/county",
    "CO2e", class(cow_burps$CO2e), "Metric tons of CO2 equivalency",
  )

saveRDS(cow_burps_survey, "./_agriculture/data/county_enteric_fermentation_cattle_survey.rds")
saveRDS(cow_burps_survey_meta, "./_agriculture/data/county_enteric_fermentation_cattle_survey_meta.rds")

#### Census data ####
#### census data is collected every five years (years ending in 2 and 7) and is the only inventory source data for non-cattle past 2012

usda_census <- tidyUSDA::getQuickstat(
  sector="ANIMALS & PRODUCTS",
  group="LIVESTOCK",
  commodity=NULL,
  category= "INVENTORY",
  domain= NULL,
  county= counties,
  key = key,
  program = "CENSUS",
  data_item = NULL,
  geographic_level = 'COUNTY',
  year = as.character(2002:2022),
  state = c("MINNESOTA","WISCONSIN"),
  geometry = TRUE,
  lower48 = TRUE, 
  weighted_by_area = T) %>% 
  as.data.frame() %>% select(-geometry)

### the following code was used to determine which variables and factors were needed to acquire head counts of livestock types
# ### what are the various descriptors that are needed here (metadata is lacking or hard to find)
# unique(usda_census$unit_desc) # "HEAD"       "OPERATIONS"
# unique(usda_census$short_desc) # 32 categories, broadly broken down into operations vs inventory; types of cattle, hogs, sheep, goats
# unique(usda_census$class_desc) # just the species modifier (e.g. 'cows, beef', 'ewes, breeding')
# unique(usda_census$group_desc) #all "LIVESTOCK"
# unique(usda_census$commodity_desc) #"CATTLE" "GOATS"  "HOGS"   "SHEEP" 
# unique(usda_census$statisticcat_desc) #all "INVENTORY"
# unique(usda_census$domaincat_desc) # like short_desc but with range of number of head, e.g. "INVENTORY OF CATTLE ON FEED: (1 TO 19 HEAD)"  
# unique(usda_census$domain_desc) # 8 inventory types (e.g. cattle on feed) and "TOTAL" 
# usda_census %>% filter(domain_desc == "TOTAL", Value != "NA") %>% distinct(short_desc) # "NOT SPECFIED"
# usda_census %>% filter(unit_desc == "HEAD", Value != "NA") %>% distinct(short_desc) # "NOT SPECFIED"
# 
# usda_census %>% filter(county_name == "DAKOTA", year == 2017, commodity_desc == "CATTLE", unit_desc == "HEAD", Value != "NA",
#                        domain_desc == "TOTAL") %>% 
#   group_by(short_desc) %>% 
#   summarize(value = sum(Value))
# comparing to USDA county profile, CATTLE, INCL CALVES - INVENTORY is the total number of cattle, other categories are subsets
# So CATTLE, (EXCL COWS) - INVENTORY is a calf inventory, CATTLE, COWS, BEEF - INVENTORY is adult beef cows, CATTLE, COWS, MILK - INVENTORY
# are dairy cows, and CATTLE, ON FEED - INVENTORY are calves on feed (documentation says explicitly these are not COWS)
# We'll create four categories - dairy cows, beef cows, calves, feedlot cattle


### rename and aggregate variables from short_dec to matchable labels with EPA emissions data
usda_census_agg <- usda_census %>%
  filter(domain_desc == "TOTAL",!is.na(Value), # first avoid including non-targeted labels as there is a lot of nesting and redundancy
         short_desc %in% c("CATTLE, (EXCL COWS) - INVENTORY",
                           "CATTLE, ON FEED - INVENTORY",
                           "CATTLE, COWS, BEEF - INVENTORY",
                           "CATTLE, COWS, MILK - INVENTORY",
                           "GOATS - INVENTORY",
                           "HOGS - INVENTORY",
                           "SHEEP, INCL LAMBS - INVENTORY")) %>% 
  ## change labels to match EPA
  mutate(livestock_type =  
           case_when(
             grepl("MILK", short_desc) ~ "Dairy Cows",
             grepl("BEEF", short_desc) ~ "Beef Cows",
             grepl("FEED", short_desc) ~ "Feedlot Cattle",
             grepl("(EXCL COWS)",short_desc) ~ "Calves",
             grepl("HOGS", short_desc) ~ "Swine",
             grepl("SHEEP",short_desc) ~ "Sheep",
             grepl("GOATS",short_desc) ~ "Goats",
             TRUE ~ short_desc
           )) %>% 
  group_by(year, county_name, livestock_type) %>% 
  summarise(head_count = sum(Value))

### subtract feedlot calves from total calves
usda_census_corrected <- left_join(usda_census_agg,
                                   usda_census_agg %>%
                                     filter(grepl("Feedlot", livestock_type)) %>%
                                     group_by(year, county_name) %>%
                                     summarise(feedlot_sum = sum(head_count)),
                                   by =  c("year", "county_name")) %>%
  mutate(head_count = ifelse(livestock_type == "Calves", head_count - feedlot_sum, head_count)) %>%
  select(-feedlot_sum)

### interpolate between census years for all animal types
census_interpolated <- left_join( # this creates an empty grid of all desired year,livestock combinations
    expand.grid(
    year = seq(2002, 2022, by = 1),
    county_name = unique(usda_census_corrected$county_name),
    livestock_type = unique(usda_census_corrected$livestock_type)
    ),
    usda_census_corrected) %>%  # merged with our populated census data, it creates NAs wherever data is missing
  mutate(head_count = if_else(year %in% c(2002,2007,2012,2017,2022) & is.na(head_count),0, head_count)) %>%  # override census years to be 0 if livestock-county combo is missing
  group_by(county_name, livestock_type) %>%
  arrange(year) %>%
  mutate(
    head_count = zoo::na.approx(head_count, na.rm = FALSE), # this function linearly interpolates NAs between known values, following the group_by
    data_type = ifelse(year %in% c(2002,2007,2012,2017,2022), 'census', 'interpolated') # marking whether values are from the census or interpolation
  )

## merge this with enteric fermentation data by year
animal_burps <- left_join(census_interpolated %>% filter(year >=2005 & year <= 2021), enteric_agg, by = c("year" = "Year", "livestock_type" = "livestock_type")) %>% 
  mutate(kg_ch4 = kg_ch4_per_head * head_count,
         MT_ch4 = kg_ch4 / 1000,
         CO2e = MT_ch4 * gwp$ch4)

# internal check that data seems sensible
ggplot(animal_burps %>% 
         group_by(year,county_name) %>% 
         summarize(CO2e = sum(CO2e)),
       aes(x = year, y = CO2e, col = county_name)) + geom_line(size = 1.5) + theme_bw()

animal_burps %>% filter(year == 2021) %>% ungroup %>%  summarize(CO2e = sum(CO2e))

# save RDS file

animal_burps_census <- animal_burps %>% 
  select(-kg_ch4)

# create metadata
animal_burps_census_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "year", class(animal_burps_census$year), "Year of survey",
    "county_name", class(animal_burps_census$county_name), "County name",
    "livestock_type", class(animal_burps_census$livestock_type), "Livestock classification",
    "head_count", class(animal_burps_census$head_count), "Number of individual (heads) of livestock type",
    "data_type", class(animal_burps_census$data_type), "Data source: interpolated is linear interpolation between census years",
    "kg_ch4_per_head", class(animal_burps_census$kg_ch4_per_head), "Methane emission factor per head for specific livestock_year combination",
    "MT_ch4", class(animal_burps_census$MT_ch4), "Total metric tons of methane emissions from livestock/year/county",
    "CO2e", class(animal_burps_census$CO2e), "Metric tons of CO2 equivalency",
  )

saveRDS(animal_burps_census, "./_agriculture/data/county_enteric_fermentation_census.rds")
saveRDS(animal_burps_census_meta, "./_agriculture/data/county_enteric_fermentation_census_meta.rds")


#### manure and lagoons ####

### get labels from N2O and CH4 emissions

### format ch4 emissions factors
### additional poultry categories that need inventory data
### manure data requires intermediate data between head count and an emission factor such as volatile solids per kg of animal, max potential emissions
### it's opaque in the SIT where these intermediate values are coming from in the Excel workbook, so we're taking a shortcut and dividing the 
### calculated emission total by the head count to get a de facto emission factor. The effect should be the same even if using the intermediate data
### If it becomes apparent counties have unique manure management systems relative to the rest of the state, this should be revisited.

manure_ch4_formatted  <- manure_ch4  %>% 
  select(c(2,3,4,18)) %>% 
  setNames(c("Year","Livestock","Heads_thousands","Metric_tons_ch4")) %>% 
  filter(!(is.na(Year) | is.na(Metric_tons_ch4) | Livestock == "TOTAL")) %>% #filter out formatting and summary rows
  filter(Year >= 2005) %>% 
  mutate(heads = as.numeric(str_remove_all(Heads_thousands,",")) * 1000, mt_ch4 = as.numeric(str_remove_all(Metric_tons_ch4,",")), 
  Emission_factor_mt_ch4_per_head = mt_ch4/heads) %>% # here is where the de facto emission factor per year-livestock is calculated
  mutate(livestock_type = 
           case_when(
             grepl("Replacement", Livestock) ~ "Calves",
             grepl("Feedlot",Livestock) ~ "Feedlot Cattle",
             grepl("Hens",Livestock) ~ "Layers",
             grepl("Chickens",Livestock) ~ "Layers",
             grepl("Sheep",Livestock) ~ "Sheep",
             grepl("Swine",Livestock) ~ "Swine",
             grepl("Market",Livestock) ~ "Swine",
             TRUE ~ Livestock
           )) %>% 
  group_by(Year, livestock_type) %>% 
  summarize(mt_ch4_per_head = mean(Emission_factor_mt_ch4_per_head))

### format n20 emissions factors
### similarly skipping intermediate steps here like in ch4 step above

manure_n2o_formatted <- manure_n2o  %>% 
  select(c(2,3,4,16)) %>% 
  setNames(c("Year","Livestock","Heads_thousands","kg_n2o")) %>% 
  filter(!(is.na(Year) | is.na(kg_n2o) | Livestock == "TOTAL")) %>% 
  filter(Year >= 2005) %>% 
  mutate(heads = as.numeric(str_remove_all(Heads_thousands,",")) * 1000, kg_n2o = as.numeric(str_remove_all(kg_n2o,",")), 
         Emission_factor_kg_n2o_per_head = kg_n2o/heads) %>% 
  mutate(livestock_type = 
           case_when(
             grepl("Replacement", Livestock) ~ "Calves",
             grepl("Feedlot",Livestock) ~ "Feedlot Cattle",
             grepl("Hens",Livestock) ~ "Layers",
             grepl("Chickens",Livestock) ~ "Layers",
             grepl("Sheep",Livestock) ~ "Sheep",
             grepl("Swine",Livestock) ~ "Swine",
             grepl("Market",Livestock) ~ "Swine",
             TRUE ~ Livestock
           )) %>% 
  group_by(Year, livestock_type) %>% 
  summarize(kg_n2o_per_head = mean(Emission_factor_kg_n2o_per_head)) 

# combine data
animal_poops <- left_join(
  left_join(census_interpolated %>% filter(year >=2005 & year <= 2021), manure_ch4_formatted, 
                       by = c("year" = "Year", "livestock_type" = "livestock_type")),
  manure_n2o_formatted, by = c("year" = "Year", "livestock_type" = "livestock_type")
  ) %>% 
  #goats don't have n2o estimates, maybe use sheep? for now zero as they will have no practical effect on totals
  mutate(kg_n2o_per_head = if_else(is.na(kg_n2o_per_head),0,kg_n2o_per_head)) %>% 
  mutate(MT_ch4 = mt_ch4_per_head * head_count,
         MT_n2o = (kg_n2o_per_head * head_count) / 1000,
         CO2e = (MT_ch4 * gwp$ch4) + (MT_n2o * gwp$n2o)) %>% 
  ungroup()

ggplot(animal_poops %>% 
         group_by(year,county_name) %>% 
         summarize(CO2e = sum(CO2e)),
       aes(x = year, y = CO2e, col = county_name)) + geom_line(size = 1.5) + theme_bw()


animal_poops %>% filter(year == 2021) %>% ungroup %>%  summarize(CO2e = sum(CO2e))

#### Poultry ####
#### repeat process for poultry

usda_poultry <- tidyUSDA::getQuickstat(
  sector="ANIMALS & PRODUCTS",
  group="POULTRY",
  commodity=NULL,
  category= "INVENTORY",
  domain= NULL,
  county= counties,
  key = key,
  program = "CENSUS",
  data_item = NULL,
  geographic_level = 'COUNTY',
  year = as.character(2002:2022),
  state = c("MINNESOTA","WISCONSIN"),
  geometry = TRUE,
  lower48 = TRUE, 
  weighted_by_area = T) %>% 
  as.data.frame() %>% select(-geometry)

### rename and aggregate
usda_poultry_agg <- usda_poultry %>%
  filter(domain_desc == "TOTAL",!is.na(Value),
         short_desc %in% c("CHICKENS, BROILERS - INVENTORY",
                           "CHICKENS, LAYERS - INVENTORY",
                           "CHICKENS, PULLETS, REPLACEMENT - INVENTORY",
                           "CHICKENS, ROOSTERS - INVENTORY",
                           "TURKEYS - INVENTORY")) %>% 
  mutate(livestock_type = 
           case_when(
             grepl("BROILERS", short_desc) ~ "Broilers",
             grepl("LAYERS", short_desc) ~ "Layers",
             grepl("PULLETS", short_desc) ~ "Pullets",
             grepl("ROOSTERS",short_desc) ~ "Broilers",
             grepl("TURKEYS", short_desc) ~ "Turkeys",
             TRUE ~ short_desc
           )) %>% 
  group_by(year, county_name, livestock_type) %>% 
  summarise(head_count = sum(Value))


### interpolate between census years for all animal types
### of note is that poultry farms in the region are often sparse enough that USDA doesn't report data to protect individual farmer data
### there are two spikes of poultry counts (Turkeys in Dakota in 2007 and Broilers in Sherburne in 2017)
### whether these are real spikes, data errors, or surrounding years are masked due to the data privacy issue is unclear, but these spikes have
### no discernible impact on the emission totals for these counties as cattle dominate animal emissions

poultry_interpolated <- left_join( # this creates an empty grid of all desired year,livestock combinations
  expand.grid(
    year = seq(2002, 2022, by = 1),
    county_name = unique(usda_poultry_agg$county_name),
    livestock_type = unique(usda_poultry_agg$livestock_type)
  ),
  usda_poultry_agg) %>%  # merged with our populated census data, it creates NAs wherever data is missing
  mutate(head_count = if_else(year %in% c(2002,2007,2012,2017,2022) & is.na(head_count),0, head_count)) %>%  # override census years to be 0 if livestock-county combo is missing
  group_by(county_name, livestock_type) %>%
  arrange(year) %>%
  mutate(
    head_count = zoo::na.approx(head_count, na.rm = FALSE), # this function linearly interpolates NAs between known values, following the group_by
    data_type = ifelse(year %in% c(2002,2007,2012,2017,2022), 'census', 'interpolated') # marking whether values are from the census or interpolation
  )

ggplot(poultry_interpolated %>% 
         group_by(year,county_name) %>% 
         summarize(poultry_total = sum(head_count)),
       aes(x = year, y = poultry_total, col = county_name)) + geom_line(size = 1.5) + theme_bw()

bird_poops <- left_join(
  left_join(poultry_interpolated %>% filter(year >=2005 & year <= 2021), manure_ch4_formatted, 
            by = c("year" = "Year", "livestock_type" = "livestock_type")),
  manure_n2o_formatted, by = c("year" = "Year", "livestock_type" = "livestock_type")
) %>% 
  #goats don't have n2o estimates, maybe use sheep? for now zero
  mutate(kg_n2o_per_head = if_else(is.na(kg_n2o_per_head),0,kg_n2o_per_head)) %>% 
  mutate(MT_ch4 = mt_ch4_per_head * head_count,
         MT_n2o = (kg_n2o_per_head * head_count) / 1000,
         CO2e = (MT_ch4 * gwp$ch4) + (MT_n2o * gwp$n2o)) %>% 
  ungroup()

## here we can see the negligible impact of poultry overall relative to regional emissions
ggplot(bird_poops %>% 
         group_by(year,county_name) %>% 
         summarize(CO2e = sum(CO2e)),
       aes(x = year, y = CO2e, col = county_name)) + geom_line(size = 1.5) + theme_bw()

# create summary manure emissions
livestock_poops <- rows_append(animal_poops,bird_poops)

### create RDS file for manure 

# create metadata
livestock_poops_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "year", class(livestock_poops$year), "Year of survey",
    "county_name", class(livestock_poops$county_name), "County name",
    "livestock_type", class(livestock_poops$livestock_type), "Livestock classification",
    "head_count", class(livestock_poops$head_count), "Number of individual (heads) of livestock type",
    "data_type", class(livestock_poops$data_type), "Data source: interpolated is linear interpolation between census years",
    "mt_ch4_per_head ", class(livestock_poops$mt_ch4_per_head), "Methane emission factor per head for specific livestock_year combination",
    "kg_n2o_per_head ", class(livestock_poops$kg_n2o_per_head), "N2O emission factor per head for specific livestock_year combination",
    "MT_ch4", class(livestock_poops$MT_ch4), "Total metric tons of methane emissions from livestock/year/county",
    "MT_n2o", class(livestock_poops$MT_ch4), "Total metric tons of nitrous oxide emissions from livestock/year/county",
    "CO2e", class(livestock_poops$CO2e), "Metric tons of CO2 equivalency",
  )

saveRDS(livestock_poops, "./_agriculture/data/county_manure_emissions.rds")
saveRDS(livestock_poops_meta, "./_agriculture/data/county_manure_emissions_meta.rds")

county_burps <- animal_burps %>% group_by(year,county_name) %>% summarize(CO2e = sum(CO2e))
county_poops <- livestock_poops %>% group_by(year,county_name) %>% summarize(CO2e = sum(CO2e))

county_livestock <- rows_append(county_burps %>% mutate(source = 'enteric_fermentation'),
                                county_poops %>% mutate(source = 'manure_emissions')) 

ggplot(county_livestock %>% 
         group_by(year,county_name) %>% 
         summarize(CO2e = sum(CO2e)),
       aes(x = year, y = CO2e, col = county_name)) + geom_line(size = 1.5) + theme_bw()

county_livestock %>% filter(year == 2021) %>% summarize(CO2e = sum(CO2e))
county_livestock %>% filter(year == 2021, !county_name %in% c("ST CROIX", "SHERBURNE", "PIERCE", "CHISAGO")) %>% summarize(CO2e = sum(CO2e))

ggplot(county_livestock %>% 
         group_by(year,source) %>% 
         summarize(CO2e = sum(CO2e)),
       aes(x = year, y = CO2e, fill = source)) + geom_area() + theme_bw()

### calculating 'Total K-Nitrogen excreted' for ag-soils-animals calculation
### this is one of the intermediate steps skipped in calculating N2O emissions from manure above, but is necessary for soil runoff/leaching calc

### pull out typical animal mass by year
tam_cattle <- pivot_longer(nex[2:40,1:11], cols = 2:11, names_to = "livestock_type", values_to = "mass_kg") %>% 
  rename(year = `Typical Animal Mass (Kg)`) %>% 
  mutate(mass_kg = as.numeric(mass_kg)) %>% 
  filter(year >=2005 & year <= 2021)

### pull our non-cattle weight from control data

tam_other <- ag_control[50:65,1:2] %>% 
  rename(livestock_type = `State Inventory Tool - Carbon Dioxide, Methane, and Nitrous Oxide Emissions from Agriculture Module\nVersion 2024.1`,
         mass_kg = ...2) %>% 
  mutate(livestock_type = case_when(
    grepl("Swine",livestock_type) ~ "Swine",
    grepl("Market",livestock_type) ~ "Swine",
    grepl("Sheep",livestock_type) ~ "Sheep",
    grepl("Chickens",livestock_type) ~ "Layers",
    TRUE ~ livestock_type)) %>% 
  filter(!is.na(mass_kg)) %>% 
  group_by(livestock_type) %>% 
  summarize(mass_kg = mean(as.numeric(mass_kg)))
  
tam <- rows_append(tam_cattle %>% filter(livestock_type == "Calves"), #only need calves for K-N calc
                   tam_other %>% 
                     crossing(year = 2005:2021))

# pull out cattle nitrogen excreted by head per year - mn and wi
nex_cattle <- pivot_longer(nex[,14:47] %>% row_to_names(1), 
                           cols = 3:34, names_to = "year", values_to = "kg_nex_head_yr") %>% 
  filter(state %in% c ("MN","WI")) %>% 
  mutate(livestock_type = case_when(
    grepl("_OF_",Animal) ~ "Feedlot Cattle",
    grepl("Dairy_Cow",Animal) ~ "Dairy Cows",
    grepl("Beef_NOF",Animal) ~ "Beef Cows",
    TRUE ~ Animal)) %>% 
  filter(livestock_type %in% c("Feedlot Cattle", "Beef Cows", "Dairy Cows")) %>% 
  group_by(state,year, livestock_type) %>% 
  summarize(kg_nex_head_yr = mean(kg_nex_head_yr))

# pull out other livestock nitrogen excreted by head per year - mn and wi. note these are in per day and per kg animal
nex_other <- pivot_longer(nex[,53:69] %>% row_to_names(1), 
                          cols = 2:17, names_to = "livestock_type", values_to = "kg_nex_day_kg_animal") %>% 
  mutate(livestock_type = case_when(
    grepl("calf",livestock_type) ~ "Calves",
    grepl("goats",livestock_type) ~ "Goats",
    grepl("swine",livestock_type) ~ "Swine",
    grepl("sheep",livestock_type) ~ "Sheep",
    grepl("broilers",livestock_type) ~ "Broilers",
    grepl("layers",livestock_type) ~ "Layers",
    grepl("pullets",livestock_type) ~ "Pullets",
    grepl("turkeys",livestock_type) ~ "Turkeys",
    TRUE ~ livestock_type),
    Year = as.numeric(Year)) %>% 
  filter(Year >= 2005) %>% 
  left_join(., tam, by = c("Year" = "year", "livestock_type" = "livestock_type")) %>% 
  mutate(kg_nex_head_yr = mass_kg * kg_nex_day_kg_animal * 365)

KN_excretion <- usda_census_agg 


### code below is beginning of MPCA feedlot permitting data. Feedlot data seemed to grossly undercount heads of livestock compared to USDA data
### shelving this for now but is more granular in detail and should be reconciled at later date to understand difference
### make the feedlot data long form
mn_feedlots_long <- mn_feedlots %>% 
  mutate(start_year = year(as.Date(start_d_reg)),
         end_year = year(as.Date(end_d_reg))) %>%
  filter(!is.na(start_year)) %>% 
  rowwise() %>%
  mutate(years = list(seq(start_year, end_year))) %>%
  unnest(years) %>% 
  pivot_longer(cols = c(
    "cattle_dl", "heifer_d", "calf_d", "cattle_db", "steer_b", "heifer_b", "cow_calf_b", "calf_b", 
    "bull_mature", "veal_calf", "swine_big", "swine_medium", "swine_little", "horse", "sheep", 
    "chx_lm", "chx_b_big", "chx_b_little", "chx_l_big", "chx_l_little", "turkey_big", "turkey_little", "duck", "duck_lm", 
    "alpacas", "bison", "bison_calf", "camels", "deer", "donkey_mule", "elk", "emus_ostriches", "peacocks", "fowl", 
    "foxes", "geese", "goats", "goats_small", "ponies", "llamas", "mink", "rabbits", "reindeer_caribou", "unknown"
  ),
  names_to = "animal",
  values_to = "count",
  values_drop_na = TRUE
  ) %>%
  filter(count >0) %>% 
  group_by(county_name, years, animal) %>% 
  summarize(county_count = sum(count)) 



  

