### compile all ctu electricity emissions

source("R/_load_pkgs.R")

## ctu and county data
cprg_ctu <- read_rds("_meta/data/cprg_ctu.RDS") %>%
  filter(!county_name %in% c("Chisago", "Sherburne", "St. Croix", "Pierce")) 
cprg_county <- read_rds("_meta/data/cprg_county.RDS") %>%
  filter(!county_name %in% c("Chisago", "Sherburne", "St. Croix", "Pierce")) 
ctu_population <- read_rds("_meta/data/ctu_population.RDS") %>% 
  left_join(cprg_county %>% st_drop_geometry() %>% select(geoid, county_name)) %>%
  filter(!county_name %in% c("Chisago", "Sherburne", "St. Croix", "Pierce")) 

# assign cities to counties where majority of population is
ctu_county_unique <- ctu_population %>%
  group_by(ctu_name, ctu_class) %>%
  filter(ctu_population == max(ctu_population)) %>%
  ungroup() %>% 
  distinct(geoid, ctuid, ctu_name, ctu_class, county_name)

# county activity data
county_mwh <- readRDS("_energy/data/minnesota_county_elec_ActivityAndEmissions.rds")

## create storage frame of unique city and utility combos with all years
ctu_utility_year <- readRDS("_energy/data/ctu_utility_intersect.rds") %>% 
  cross_join(data.frame(inventory_year = c(2007:2023))) %>% 
  mutate(residential_mwh = NA,
         business_mwh = NA,
         total_mwh = NA) %>% 
  rename(utility = utility_name)

## load formatted SQL utility data
sql_elec <- readRDS("_energy/data/ctu_electricity_emissions_2015_2018.rds") %>% 
  mutate(ctu_class = if_else(grepl("Twp.", ctu_name), "TOWNSHIP", "CITY"),
         ctu_name = str_replace_all(ctu_name, " Twp.", ""),
         ctu_name = str_replace_all(ctu_name, "St. ", "Saint "),
         ctu_class = if_else(ctu_name %in% c("Credit River", "Empire"),
                             "CITY",
                             ctu_class),
         utility = data_source) %>% 
  filter(units_emissions == "Metric tons CO2",
         !is.na(mwh_per_year)) %>%  # removes duplicates
 mutate(sector = if_else(customer_class == "Residential",
                         "Residential",
                         "Business")) %>%
  group_by(ctu_name, emissions_year, utility, sector) %>%
  summarise(mwh_per_year = sum(mwh_per_year, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = sector, values_from = mwh_per_year, 
              names_glue = "{tolower(sector)}_mwh") %>% 
  mutate(total_mwh = replace_na(business_mwh, 0) + replace_na(residential_mwh, 0))

## load and format connexus data
connexus <- readRDS("_energy/data/connexus_activityData_2014_2023.rds") %>% 
  filter(!is.na(mwh_delivered)) %>% 
  rename(emissions_year = year) %>% 
  #mutate(mwh_delivered = mwh_delivered * 10e-4) %>%  # kwh listed instead of mwh
  mutate(sector = case_when(
    sector == "Residential" ~ "Residential",
    sector == "Residential/Commercial/Industrial" ~ "Total",
    TRUE ~ "Business"))  %>% 
  group_by(ctu_name, emissions_year, utility, sector) %>%
  summarise(mwh_per_year = sum(mwh_delivered, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = sector, values_from = mwh_per_year, 
              names_glue = "{tolower(sector)}_mwh") %>% 
  mutate(total_mwh = replace_na(business_mwh, 0) + replace_na(residential_mwh, 0))


### load and format xcel data  
xcel <- readRDS("_energy/data/Xcel_activityData_2015_2023.rds") %>% 
  filter(!is.na(mwh_delivered)) %>% 
  rename(emissions_year = year) %>% 
  mutate(sector = case_when(
    sector_mapped == "residential" ~ "Residential",
    TRUE ~ "Business")) %>% 
  group_by(ctu_name, emissions_year, utility, sector) %>%
  summarise(mwh_per_year = sum(mwh_delivered, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = sector, values_from = mwh_per_year, 
              names_glue = "{tolower(sector)}_mwh") %>% 
  mutate(total_mwh = replace_na(business_mwh, 0) + replace_na(residential_mwh, 0))

  


#load in municipal utility data
munis <- readRDS("_energy/data/MNelecMunis_activityData_2014_2023.rds") %>% 
  filter(!is.na(mwh_delivered)) %>% 
  rename(emissions_year = year) %>% 
  mutate(sector = case_when(
    sector_mapped == "residential" ~ "Residential",
    TRUE ~ "Business")) %>% 
  group_by(ctu_name, emissions_year, utility, sector) %>%
  summarise(mwh_per_year = sum(mwh_delivered, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = sector, values_from = mwh_per_year, 
              names_glue = "{tolower(sector)}_mwh") %>% 
  mutate(total_mwh = replace_na(business_mwh, 0) + replace_na(residential_mwh, 0))

# function: sequentially load data while keeping NAs
merge_electricity_data <- function(base_df, new_data) {
  base_df %>%
    left_join(new_data %>% rename(inventory_year = emissions_year), 
              by = c("ctu_name", "inventory_year", "utility")) %>%
    mutate(
      residential_mwh = if_else(!is.na(residential_mwh.y), residential_mwh.y, residential_mwh.x),
      business_mwh = if_else(!is.na(business_mwh.y), business_mwh.y, business_mwh.x),
      total_mwh = if_else(!is.na(total_mwh.y), total_mwh.y, total_mwh.x)
    ) %>%
    select(-ends_with(".x"), -ends_with(".y"))  # Remove duplicate columns
}

## load each dataset sequentially (deliberately override previous sql data with our data requests)

#make sure names conform
anti_join(sql_elec, ctu_utility_year, by = "utility") %>%
  distinct(utility) %>% arrange(utility)
sort(unique(ctu_utility_year$utility))
sql_elec <- sql_elec %>% 
  mutate(utility = case_when(
    utility == "City of Chaska" ~ "City of Chaska Electric Department",
    utility == "Wright-Hennepin Coop Electric Assn" ~ "Wright Hennepin Electric Cooperative",
    TRUE ~ utility
  ))

ctu_utility_year <- merge_electricity_data(ctu_utility_year, sql_elec)
ctu_utility_year <- merge_electricity_data(ctu_utility_year, connexus)
ctu_utility_year <- merge_electricity_data(ctu_utility_year, xcel)

anti_join(munis, ctu_utility_year, by = "utility") %>%
  distinct(utility) %>% arrange(utility)
sort(unique(ctu_utility_year$utility))

munis <- munis %>% 
  mutate(utility = case_when(
    utility == "City of Chaska" ~ "City of Chaska Electric Department",
    utility == "City of Anoka" ~ "Anoka Municipal Utility",
    utility == "City of North St Paul" ~ "City of North Saint Paul Electric Utilities",
    TRUE ~ utility
  ))

ctu_utility_year <- merge_electricity_data(ctu_utility_year, munis)


### compare to rii totals and supplement where possible

# get a list of city-year combos we think are complete

ctu_year_complete <- ctu_utility_year %>%
  group_by(ctu_name, ctu_class, inventory_year) %>%
  filter(!any(is.na(total_mwh))) %>%
  summarize(total_mwh = sum(total_mwh)) %>% 
  ungroup() 
  #115/210 ctus

  
rii <-read_rds("_energy/data/rii_electricity_2007_2023.rds")

# merge with ctu_year_complete
rii_ctu_comp <- inner_join(ctu_year_complete,
                          rii %>% 
                            group_by(ctu_name, ctu_class, year) %>% 
                            summarize(rii_mwh = sum(mwh_delivered)) %>% 
                            ungroup(),
                          by = c("ctu_name", "ctu_class",
                                 "inventory_year" = "year"))  

ggplot(data = rii_ctu_comp, aes(x = total_mwh, y = rii_mwh)) +
  geom_point() + geom_abline (slope=1) + theme_bw()
## fairly tight, a few notable departure

rii_ctu_comp %>% filter(
  abs(total_mwh - rii_mwh) > 10000)


### put RII data in for city-years without any utility data (skip partial sets)

rii_wide <- rii %>% 
  mutate(sector_use = if_else(sector == "Residential",
                              "Residential",
                              "Business")) %>% 
  select(-sector) %>% 
  pivot_wider(names_from = sector_use, values_from = mwh_delivered, 
              names_glue = "{tolower(sector_use)}_mwh") %>% 
  mutate(total_mwh = replace_na(business_mwh, 0) + replace_na(residential_mwh, 0))

# id cities with NO utility data

empty_city_years <- ctu_utility_year %>%
  group_by(ctu_name, ctu_class,inventory_year) %>%
  summarise(all_na = all(is.na(business_mwh) & is.na(residential_mwh) & is.na(total_mwh)), .groups = "drop") %>%
  filter(all_na) %>%
  select(ctu_name,ctu_class, inventory_year)

#pull out rii data matching above

rii_fill <- empty_city_years %>%
  left_join(rii_wide %>% rename(inventory_year = year), 
            by = c("ctu_name", "ctu_class", "inventory_year")) %>%
  select(ctu_name, ctu_class, inventory_year, utility, business_mwh, residential_mwh, total_mwh) %>%
  mutate(total_mwh = replace_na(business_mwh, 0) + replace_na(residential_mwh, 0)) %>% 
  filter(!(is.na(business_mwh) | is.na(residential_mwh)))

ctu_utility_year <- ctu_utility_year %>%
  anti_join(rii_fill %>% select(ctu_name, ctu_class, inventory_year), 
            by = c("ctu_name", "ctu_class", "inventory_year")) %>% 
  bind_rows(.,rii_fill) 

## save output file

saveRDS(ctu_utility_year,
        "_energy/data/ctu_utility_mwh.RDS")
