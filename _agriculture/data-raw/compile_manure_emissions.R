source("R/_load_pkgs.R")
source("R/global_warming_potential.R")
#source("_agriculture/data-raw/_fetch_usda_key.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")

### load in livestock count data
livestock <- read_rds("_agriculture/data/usda_census_data.rds") %>% 
  left_join(., cprg_county %>% select(STATE, NAME) %>% st_drop_geometry(),
            by = c("county_name" = "NAME"))

### load in formatted activity data connecting livestock to manure emissions
vs <- read_rds("_agriculture/data/volatile_solids.rds")
nex <- read_rds("_agriculture/data/nitrogen_excretion.rds")
mcf <- read_rds("_agriculture/data/methane_conversion_factor_livestock.rds")

# formatted files
ag_constants <- readRDS("_agriculture/data/ag_constants.rds")

## convert to named vector for easier indexing
ag_constants_vec <- ag_constants %>%
  dplyr::select(short_text, value) %>%
  tibble::deframe()

ag_manure_mgmt <- readRDS("_agriculture/data/manure_management_systems.rds")


#### manure and lagoons ####

### get labels from N2O and CH4 emissions

### format ch4 emissions factors
### additional poultry categories that need inventory data
### manure data requires intermediate data between head count and an
###  emission factor such as volatile solids per kg of animal, max potential emissions
###  
### it's opaque in the SIT where these intermediate values are coming from
###  in the Excel workbook, so we're taking a shortcut and dividing the
### calculated emission total by the head count to get a de facto emission factor. 
### The effect should be the same even if using the intermediate data
### 
### If it becomes apparent counties have unique manure management systems 
### relative to the rest of the state, this should be revisited.
### UPDATE: Better understanding now of where values are coming from in Excel 
### Workbook so this shortcut can be revised to be more explicit and therefore flexible

### pull out and format Bo (max potential emissions (ch4/ kg vs))

Bo <- ag_constants %>% 
  filter(grepl("Bo",description)) %>% 
  mutate(
    livestock_type = case_when(
      grepl("swine", short_text) ~ "Swine",
      grepl("Feedlot", short_text) ~ "Feedlot Cattle",
      grepl("pullets", short_text) ~ "Pullets",
      grepl("broilers", short_text) ~ "Broilers",
      grepl("hens", short_text) ~ "Layers",
      grepl("sheep", short_text) ~ "Sheep",
      grepl("turkeys", short_text) ~ "Turkeys",
      grepl("goats", short_text) ~ "Goats",
      TRUE ~ short_text
    )
  ) %>%
  group_by(livestock_type) %>%
  summarize(Bo = mean(as.numeric(value)))


# calculate ch4 emissions from manure management
manure_ch4 <- left_join(livestock,
                        vs,
                        by = c("year" = "year",
                               "livestock_type" = "livestock_type",
                               "STATE" ="state")) %>% 
  filter(year >= 2005 & year <= 2021) %>% 
  left_join(., Bo) %>% 
  left_join(., mcf,
            by = c("year" = "year", "STATE" = "state", "livestock_type" = "livestock_type")) %>% 
  mutate(mt_ch4 = head_count * mt_vs_head_yr * Bo * mcf_percent * ag_constants_vec["kg_m3"],
         mt_co2e = mt_ch4 * gwp$ch4) %>% 
  group_by(year, county_name) %>% 
  summarize(mt_ch4 = sum(mt_ch4), mt_co2e = sum(mt_co2e))


### format n20 emissions factors

### first need to calculate n2o emissions from lagoons and solid state storage

#select management types that use lagoons
lagoon <- ag_manure_mgmt %>% 
  filter(mgmt_system %in% c("Anaerobic Lagoon",
                            "Liquid/Slurry",
                            "Liquid/ Slurry") | 
           (mgmt_system == "Deep Pit" &
              livestock_type == "Swine")) %>% 
  mutate(state = if_else(state == "MN", "Minnesota", "Wisconsin")) %>% 
  group_by(year, state, livestock_type) %>% 
  summarize(lagoon_perc = sum(percentage))

solids <- ag_manure_mgmt %>% 
  filter(mgmt_system %in% c("Solid Storage",
                            "Dry Lot",
                            "Poultry without bedding",
                            "Litter",
                            "On Feed (PRP)") |
           (mgmt_system == "Deep Pit" &
              livestock_type == "Dairy Cows")) %>% 
  mutate(state = if_else(state == "MN", "Minnesota", "Wisconsin")) %>% 
  group_by(year, state, livestock_type) %>% 
  summarize(solid_perc = sum(percentage))


manure_n2o <- left_join(livestock %>% 
                          filter(year >= 2005 & year <= 2021),
                        nex %>% 
                          filter(year >= 2005 & year <= 2021) %>% 
                          mutate(state = if_else(state == "MN",
                                                 "Minnesota",
                                                 "Wisconsin")),
                        by = c("STATE" = "state", "year" = "year", 
                               "livestock_type" = "livestock_type")) %>% 
  left_join(., lagoon, by = c("STATE" = "state", "year" = "year", 
                              "livestock_type" = "livestock_type")) %>% 
  left_join(., solids, by = c("STATE" = "state", "year" = "year", 
                              "livestock_type" = "livestock_type")) %>% 
  replace(is.na(.), 0) %>%
  mutate(mt_n2o_lagoon = head_count * 
                         kg_nex_head_yr *  # to kg_nex_yr
                         (1 - ag_constants_vec["VolPercent"]) * # % unvolatized
                         lagoon_perc * # in a lagoon
                         ag_constants_vec["LiquidEF"] *  #N2O-N2 emissions
                         ag_constants_vec["N2O_N2"] / #N2O emissions (kg)
                         1000,# to mt
         mt_n2o_solid = head_count * 
           kg_nex_head_yr *  # to kg_nex_yr
           (1 - ag_constants_vec["VolPercent"]) * # % unvolatized
           solid_perc * # stored as solids
           ag_constants_vec["SolidEF"] *  #N2O-N2 emissions
           ag_constants_vec["N2O_N2"] / #N2O emissions (kg)
           1000, # to mt
         mt_n2o = mt_n2o_lagoon + mt_n2o_solid,# total
         mt_co2e = mt_n2o * gwp$n2o) 
# %>% 
#   group_by(year, county_name) %>% 
#   summarize(mt_n2o = sum(mt_n2o), mt_co2e = sum(mt_co2e))
           
### calculating 'Total K-Nitrogen excreted' for ag-soils-animals calculation
### this is one of the intermediate steps skipped in calculating N2O emissions from manure above, but is necessary for soil runoff/leaching calc

KN_excretion_runoff <- left_join(
  livestock %>% filter(year >= 2005),
  nex %>% 
    filter(year >= 2005 & year <= 2021) %>% 
    mutate(state = if_else(state == "MN",
                           "Minnesota",
                           "Wisconsin")), by = c("STATE" = "state",
              "livestock_type" = "livestock_type",
               "year" = "year")) %>%
  mutate(total_kn_excretion_kg = head_count * kg_nex_head_yr)

nex_runoff_emissions <- KN_excretion_runoff %>%
  group_by(year, county_name, data_type) %>%
  summarize(mt_total_kn_excretion = sum(total_kn_excretion_kg / 1000)) %>%
  mutate(
    # multiply total k-n excretion by volatization percent and then leaching EF
    mt_n = mt_total_kn_excretion * (1 - ag_constants_vec["VolPercent"]) * ag_constants_vec["LeachEF"], 
    mt_n2o = mt_n * ag_constants_vec["LeachEF2"] * ag_constants_vec["N2O_N2"],
    mt_co2e = mt_n2o * gwp$n2o
  )

##### manure management system emissions

manure_mgmt_perc <- ag_manure_mgmt %>%
  mutate(management_type = case_when(
    managed == "Yes" ~ "Managed",
    mgmt_system %in% c("Pasture", "PRP", "Dry Lot", "Range", "Pasture, Range & Paddock") ~ "Pasture_range",
    mgmt_system == "Daily Spread" ~ "Daily_spread"
  )) %>%
  group_by(year, state, livestock_type, management_type) %>%
  summarize(percentage = sum(percentage))

manure_mgmt_perc <- manure_mgmt_perc %>% 
  mutate(state = if_else(state == "MN", "Minnesota", "Wisconsin"))
  

manure_soils <- left_join(KN_excretion_runoff %>% filter(year != 2022),
                          manure_mgmt_perc %>% filter(management_type == "Managed") %>%
                            dplyr::select(-management_type) %>%
                            rename(percent_managed = percentage),
                          by = c(
                            "year" = "year",
                            "livestock_type" = "livestock_type",
                            "STATE" = "state"
                          )
) %>%
  left_join(., manure_mgmt_perc %>% filter(management_type == "Daily_spread") %>%
              dplyr::select(-management_type) %>%
              rename(percent_daily_spread = percentage),
            by = c(
              "year" = "year",
              "livestock_type" = "livestock_type",
              "STATE" = "state"
            )
  ) %>%
  left_join(., manure_mgmt_perc %>% filter(management_type == "Pasture_range") %>%
              dplyr::select(-management_type) %>%
              rename(percent_pasture = percentage),
            by = c(
              "year" = "year",
              "livestock_type" = "livestock_type",
              "STATE" = "state"
            )
  ) %>%
  ### some livestock_type have manure management determined from other sources or within SIT workbook
  mutate(
    percent_managed = case_when(
      livestock_type %in% c("Broilers", "Pullets") ~ 1,
      # sheep have unclear math happening in SIT, with ratio flipping from 
      # 1/3 to 2/3 depending on whether on feed. Taking middle value.
      livestock_type %in% c("Sheep") ~ 0.5,
      TRUE ~ percent_managed
    ),
    percent_pasture = case_when(
      livestock_type %in% c("Calves") ~ 1,
      # sheep have unclear math happening in SIT, with ratio flipping from 
      # 1/3 to 2/3 depending on whether on feed. Taking middle value.
      livestock_type %in% c("Sheep") ~ 0.5, 
      TRUE ~ percent_pasture
    ),
    managed_nex = total_kn_excretion_kg * percent_managed,
    pasture_nex = total_kn_excretion_kg * percent_pasture,
    daily_spread_nex = total_kn_excretion_kg * percent_daily_spread
  ) %>%
  replace(is.na(.), 0) 
## lots of NAs for livestock without certain manure management, need it to be zero for next step


manure_soils_emissions <- manure_soils %>%
  mutate(
    MT_n2o_manure_application = (managed_nex + daily_spread_nex) *
      (1 - ag_constants_vec["VolPercent_Indirect"]) *
      ag_constants_vec["NonVolEF"] /
      1000 *
      ag_constants_vec["N2O_N2"],
    MT_n2o_pasture = pasture_nex * ag_constants_vec["prpEF"] / 1000 *
      ag_constants_vec["N2O_N2"],
    MT_co2e_manure_application = MT_n2o_manure_application * gwp$n2o,
    MT_co2e_pasture = MT_n2o_pasture * gwp$n2o
  )

manure_soils_emissions_county <- manure_soils_emissions %>%
  mutate(co2e_combined = MT_co2e_manure_application + MT_co2e_pasture) %>%
  group_by(year, county_name) %>%
  summarize(co2e = sum(co2e_combined))

manure_soils_emissions_county %>%
  filter(year == 2021) %>%
  pull(co2e) %>%
  sum()
# 86113
# for later: make test for this value

### compile all emissions for export

manure_emissions <- bind_rows(
  manure_ch4 %>% group_by(year, county_name) %>% 
    summarize(mt_co2e = sum(mt_co2e), mt_gas = sum(mt_ch4)) %>%
    mutate(category = "livestock", source = "manure_management", gas_type = "ch4"),
  manure_n2o %>% group_by(year, county_name) %>% 
    summarize(mt_co2e = sum(mt_co2e), mt_gas = sum(mt_n2o)) %>%
    mutate(category = "livestock", source = "manure_management", gas_type = "n2o"),
  manure_soils_emissions %>% group_by(year, county_name) %>%
    summarize(mt_co2e = sum(MT_co2e_manure_application + MT_co2e_pasture),
              mt_gas = sum(MT_n2o_manure_application + MT_n2o_pasture)
    ) %>%
    mutate(category = "livestock", 
           source = "direct_manure_soil_emissions",
           gas_type = "n2o"),
  nex_runoff_emissions %>%
    group_by(year, county_name) %>% summarize(
      mt_co2e = sum(mt_co2e),
      mt_gas = sum(mt_n2o)
    ) %>%
    mutate(category = "livestock", source = "indirect_manure_runoff_emissions", gas_type = "n2o")
) %>%
  filter(year != 2022) %>%
  replace(is.na(.), 0)  ## Anoka has missing enteric fermentation data from 2018-2021. They should have some livestock according to online USDA, revisit.


manure_emissions_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "year", class(livestock_emissions$year), "Year of survey",
    "county_name", class(livestock_emissions$county_name), "County name",
    "MT_co2e", class(livestock_emissions$MT_co2e), "Metric tons of CO2 equivalency",
    "MT_gas", class(livestock_emissions$MT_gas), "Total metric tons of gas emitted from source",
    "category", class(livestock_emissions$category), "Subsector category",
    "source", class(livestock_emissions$category), "Detailed description of emission source",
    "gas_type", class(livestock_emissions$gas_type), "Greenhouse gas emitted from source",
  )

saveRDS(livestock_emissions, "./_agriculture/data/county_livestock_emissions_2005_2021.rds")
saveRDS(livestock_emissions_meta, "./_agriculture/data/county_livestock_emissions_2005_2021_meta.rds")

