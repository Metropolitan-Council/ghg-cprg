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


manure_ch4 <- left_join(livestock,
                        vs,
                        by = c("year" = "year",
                               "livestock_type" = "livestock_type",
                               "STATE" ="state")) %>% 
  filter(year >= 2005 & year <= 2021)


 
  
  manure_ch4 %>%
  dplyr::select(c(2, 3, 4, 18)) %>%
  setNames(c("Year", "Livestock", "Heads_thousands", "Metric_tons_ch4")) %>%
  filter(!(is.na(Year) | is.na(Metric_tons_ch4) | Livestock == "TOTAL")) %>% # filter out formatting and summary rows
  filter(Year >= 2005) %>%
  mutate(
    heads = as.numeric(str_remove_all(Heads_thousands, ",")) * 1000, mt_ch4 = as.numeric(str_remove_all(Metric_tons_ch4, ",")),
    Emission_factor_mt_ch4_per_head = mt_ch4 / heads
  ) %>% # here is where the de facto emission factor per year-livestock is calculated
  mutate(
    livestock_type =
      case_when(
        grepl("Replacement", Livestock) ~ "Calves",
        grepl("Feedlot", Livestock) ~ "Feedlot Cattle",
        grepl("Hens", Livestock) ~ "Layers",
        grepl("Chickens", Livestock) ~ "Layers",
        grepl("Sheep", Livestock) ~ "Sheep",
        grepl("Swine", Livestock) ~ "Swine",
        grepl("Market", Livestock) ~ "Swine",
        TRUE ~ Livestock
      )
  ) %>%
  group_by(Year, livestock_type) %>%
  summarize(mt_ch4_per_head = mean(Emission_factor_mt_ch4_per_head))

### format n20 emissions factors
### similarly skipping intermediate steps here like in ch4 step above

manure_n2o_formatted <- manure_n2o %>%
  dplyr::select(c(2, 3, 4, 16)) %>%
  setNames(c("Year", "Livestock", "Heads_thousands", "kg_n2o")) %>%
  filter(!(is.na(Year) | is.na(kg_n2o) | Livestock == "TOTAL")) %>%
  filter(Year >= 2005) %>%
  mutate(
    heads = as.numeric(str_remove_all(Heads_thousands, ",")) * 1000, kg_n2o = as.numeric(str_remove_all(kg_n2o, ",")),
    Emission_factor_kg_n2o_per_head = kg_n2o / heads
  ) %>%
  mutate(
    livestock_type =
      case_when(
        grepl("Replacement", Livestock) ~ "Calves",
        grepl("Feedlot", Livestock) ~ "Feedlot Cattle",
        grepl("Hens", Livestock) ~ "Layers",
        grepl("Chickens", Livestock) ~ "Layers",
        grepl("Sheep", Livestock) ~ "Sheep",
        grepl("Swine", Livestock) ~ "Swine",
        grepl("Market", Livestock) ~ "Swine",
        TRUE ~ Livestock
      )
  ) %>%
  group_by(Year, livestock_type) %>%
  summarize(kg_n2o_per_head = mean(Emission_factor_kg_n2o_per_head))

# combine data
animal_poops <- left_join(
  left_join(census_interpolated %>% filter(year >= 2005 & year <= 2021), manure_ch4_formatted,
            by = c("year" = "Year", "livestock_type" = "livestock_type")
  ),
  manure_n2o_formatted,
  by = c("year" = "Year", "livestock_type" = "livestock_type")
) %>%
  # goats don't have n2o estimates, maybe use sheep? for now zero as they will have no practical effect on totals
  mutate(kg_n2o_per_head = if_else(is.na(kg_n2o_per_head), 0, kg_n2o_per_head)) %>%
  mutate(
    MT_ch4 = mt_ch4_per_head * head_count,
    MT_n2o = (kg_n2o_per_head * head_count) / 1000,
    CO2e = (MT_ch4 * gwp$ch4) + (MT_n2o * gwp$n2o)
  ) %>%
  ungroup()


# # Check plots
# ggplot(
#   animal_poops %>%
#     group_by(year, county_name) %>%
#     summarize(CO2e = sum(CO2e)),
#   aes(x = year, y = CO2e, col = county_name)
# ) +
#   geom_line(size = 1.5) +
#   theme_bw()
# 
# 
# animal_poops %>%
#   filter(year == 2021) %>%
#   ungroup() %>%
#   summarize(CO2e = sum(CO2e))



ggplot(
  poultry_interpolated %>%
    group_by(year, county_name) %>%
    summarize(poultry_total = sum(head_count)),
  aes(x = year, y = poultry_total, col = county_name)
) +
  geom_line(size = 1.5) +
  theme_bw()

bird_poops <- left_join(
  left_join(poultry_interpolated %>% filter(year >= 2005 & year <= 2021), manure_ch4_formatted,
            by = c("year" = "Year", "livestock_type" = "livestock_type")
  ),
  manure_n2o_formatted,
  by = c("year" = "Year", "livestock_type" = "livestock_type")
) %>%
  # goats don't have n2o estimates, maybe use sheep? for now zero
  mutate(kg_n2o_per_head = if_else(is.na(kg_n2o_per_head), 0, kg_n2o_per_head)) %>%
  mutate(
    MT_ch4 = mt_ch4_per_head * head_count,
    MT_n2o = (kg_n2o_per_head * head_count) / 1000,
    CO2e = (MT_ch4 * gwp$ch4) + (MT_n2o * gwp$n2o)
  ) %>%
  ungroup()

# ## here we can see the negligible impact of poultry overall relative to regional emissions (y-axis)
# ggplot(
#   bird_poops %>%
#     group_by(year, county_name) %>%
#     summarize(CO2e = sum(CO2e)),
#   aes(x = year, y = CO2e, col = county_name)
# ) +
#   geom_line(size = 1.5) +
#   theme_bw()

# create summary manure emissions
livestock_poops <- rows_append(animal_poops, bird_poops)

### create RDS file for manure



county_burps <- animal_burps %>%
  group_by(year, county_name) %>%
  summarize(CO2e = sum(CO2e))
county_poops <- livestock_poops %>%
  group_by(year, county_name) %>%
  summarize(CO2e = sum(CO2e))

county_livestock <- rows_append(
  county_burps %>% mutate(source = "enteric_fermentation"),
  county_poops %>% mutate(source = "manure_emissions")
)

ggplot(
  county_livestock %>%
    group_by(year, county_name) %>%
    summarize(CO2e = sum(CO2e)),
  aes(x = year, y = CO2e, col = county_name)
) +
  geom_line(size = 1.5) +
  theme_bw()

county_livestock %>%
  filter(year == 2021) %>%
  summarize(CO2e = sum(CO2e))
# 784599
county_livestock %>%
  filter(year == 2021, !county_name %in% c("ST CROIX", "SHERBURNE", "PIERCE", "CHISAGO")) %>%
  summarize(CO2e = sum(CO2e))
# 401422

ggplot(
  county_livestock %>%
    group_by(year, source) %>%
    summarize(CO2e = sum(CO2e)),
  aes(x = year, y = CO2e, fill = source)
) +
  geom_area() +
  theme_bw()

### calculating 'Total K-Nitrogen excreted' for ag-soils-animals calculation
### this is one of the intermediate steps skipped in calculating N2O emissions from manure above, but is necessary for soil runoff/leaching calc


KN_excretion_runoff <- left_join(
  rows_append(census_interpolated, poultry_interpolated) %>% filter(year >= 2005),
  as.data.frame(cprg_county) %>% dplyr::select(NAME, STATE_ABB) %>%
    mutate(county_name = if_else(NAME == "St. Croix", "ST CROIX", toupper(NAME))) %>%
    dplyr::select(-NAME)
) %>%
  left_join(., nex_formatted, by = c("STATE_ABB" = "state",
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


manure_soils <- left_join(KN_excretion_runoff %>% filter(year != 2022),
                          manure_mgmt_perc %>% filter(management_type == "Managed") %>%
                            dplyr::select(-management_type) %>%
                            rename(percent_managed = percentage),
                          by = c(
                            "year" = "year",
                            "livestock_type" = "livestock_type",
                            "STATE_ABB" = "state"
                          )
) %>%
  left_join(., manure_mgmt_perc %>% filter(management_type == "Daily_spread") %>%
              dplyr::select(-management_type) %>%
              rename(percent_daily_spread = percentage),
            by = c(
              "year" = "year",
              "livestock_type" = "livestock_type",
              "STATE_ABB" = "state"
            )
  ) %>%
  left_join(., manure_mgmt_perc %>% filter(management_type == "Pasture_range") %>%
              dplyr::select(-management_type) %>%
              rename(percent_pasture = percentage),
            by = c(
              "year" = "year",
              "livestock_type" = "livestock_type",
              "STATE_ABB" = "state"
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
# 90788
# for later: make test for this value

### compile all emissions for export

livestock_emissions <- bind_rows(
  cow_burps %>% group_by(year, county_name) %>%
    summarize(MT_co2e = sum(CO2e), MT_gas = sum(MT_ch4)) %>%
    mutate(category = "livestock", source = "enteric_fermentation", gas_type = "ch4"),
  livestock_poops %>% group_by(year, county_name) %>% 
    summarize(MT_co2e = sum(CO2e), MT_gas = sum(MT_ch4)) %>%
    mutate(category = "livestock", source = "manure_management", gas_type = "ch4"),
  livestock_poops %>% group_by(year, county_name) %>%
    summarize(MT_co2e = sum(CO2e), MT_gas = sum(MT_n2o)) %>%
    mutate(category = "livestock", source = "manure_management", gas_type = "n2o"),
  manure_soils_emissions %>% group_by(year, county_name) %>%
    summarize(MT_co2e = sum(MT_co2e_manure_application + MT_co2e_pasture),
              MT_gas = sum(MT_n2o_manure_application + MT_n2o_pasture)
    ) %>%
    mutate(category = "livestock", 
           source = "direct_manure_soil_emissions",
           gas_type = "n2o"),
  nex_runoff_emissions %>%
    group_by(year, county_name) %>% summarize(
      MT_co2e = sum(mt_co2e),
      MT_gas = sum(mt_n2o)
    ) %>%
    mutate(category = "livestock", source = "indirect_manure_runoff_emissions", gas_type = "n2o")
) %>%
  filter(year != 2022) %>%
  replace(is.na(.), 0) %>% ## Anoka has missing enteric fermentation data from 2018-2021. They should have some livestock according to online USDA, revisit.
  mutate(county_name = if_else(county_name == "ST CROIX", "St. Croix",
                               str_to_sentence(county_name))) # match case to other files

livestock_emissions_meta <-
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

# MPCA feedlot permitting data -----
### code below is beginning of MPCA feedlot permitting data. Feedlot data seemed
###  to grossly undercount heads of livestock compared to USDA data
### shelving this for now but is more granular in detail and should be reconciled
###  at later date to understand difference
### make the feedlot data long form
### UPDATE: now understanding the USDA data better, there are many cattle
###  not in feedlots, so the 1/3 count here seems consistent.
mn_feedlots_long <- mn_feedlots %>%
  mutate(
    start_year = year(as.Date(start_d_reg)),
    end_year = year(as.Date(end_d_reg))
  ) %>%
  filter(!is.na(start_year)) %>%
  rowwise() %>%
  mutate(years = list(seq(start_year, end_year))) %>%
  unnest(years) %>%
  pivot_longer(
    cols = c(
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
  filter(count > 0) %>%
  group_by(county_name, years, animal) %>%
  summarize(county_count = sum(count))
