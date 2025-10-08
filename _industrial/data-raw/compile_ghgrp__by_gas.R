#### Script to read in and process EPA GHG FLIGHT data
source("R/_load_pkgs.R")
source("R/global_warming_potential.R")

cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_ctu <- readRDS("_meta/data/cprg_ctu.RDS")

county_emissions <- read_rds("_meta/data/cprg_county_emissions.RDS")

## industrial factor hub
industrial_hub <- readRDS("_meta/data/epa_ghg_factor_hub.RDS") %>%
  extract2("industrial_combustion") %>%
  clean_names()

### download flight data: https://ghgdata.epa.gov/ghgp/main.do
ghgrp_files <- list.files(file.path(here::here(), "_industrial/data-raw/ghgrp"))

#subpart_c emissions
ind_fuel_combustion <- readRDS("./_industrial/data/fuel_combustion_emissions_by_gas.rds")

#### There is no EPA flight records for Pierce or St. Croix counties in WI

ghgrp <- lapply(as.character(2011:2023), function(y) {
  read_excel(
    file.path(
      here::here(),
      paste0(
        "_industrial/data-raw/ghgrp/ghgp_data_",
        y,
        ".xlsx"
      )
    ),
    sheet = 1,
    skip = 3
  ) %>%
    clean_names() %>%
    filter(state %in% c("MN", "WI")) %>%
    select(-(last_col(offset = 2):last_col())) %>%
    mutate(county_name = str_remove(str_to_title(county), " County")) %>%
    filter(county_name %in% cprg_county$county_name) %>%
    mutate(
      unit_emissions = "Metric tons of CO2e",
      inventory_year = y
    )
}) %>%
  bind_rows() %>% 
  # remove sources/gases where there is only NAs
  select(where(~ !all(is.na(.)))) %>% 
  mutate(petroleum_and_natural_gas_systems_transmission_compression = 
           as.numeric(petroleum_and_natural_gas_systems_transmission_compression)) %>% 
  # removing HERC and biogenic co2 emissions right off the bat
  filter(facility_id != 1004795) %>% 
  select(-biogenic_co2_emissions_metric_tons)

### split out emissions by gas type
## start by pulling out sources with only fuel combustion as we'll get them in subpart c analysis

gas_cols <- colnames(ghgrp)[15:25]
source_cols <- colnames(ghgrp)[26:38]

## look at crosstabs of emissions sources (i.e. does everything also have stationary combustion?)
emission_sources <- ghgrp %>%
  select(stationary_combustion:industrial_waste_landfills) %>% 
  mutate(across(everything(), ~ as.integer(!is.na(.) & . > 0))) %>% 
  as.matrix()

cross_counts <- t(emission_sources) %*% emission_sources
cross_counts[1:10, 1:10] 
### everything has stationary combustion which will need to be subtracted away to deal with
### utility natural gas delivery

## pulling out refinery emissions

refinery_gas <- ghgrp %>% 
  filter(grepl("Y", industry_type_subparts)) %>%  #refinery subpart
  pivot_longer(
    cols = all_of(gas_cols),
    names_to = "gas_type",
    values_to = "value_emissions"
  ) %>% 
  filter(!is.na(value_emissions)) %>% 
  mutate(
    mt_gas = case_when(
      grepl("co2",gas_type) ~ value_emissions / gwp$co2,
      grepl("ch4",gas_type) ~ value_emissions / 25, # they used IPCC 4th assessment values, so can't use stored values
      grepl("n2o",gas_type) ~ value_emissions /298), # they used IPCC 4th assessment values, so can't use stored values
    metric_tons_co2e = case_when(#homogenize names
      grepl("co2",gas_type) ~ mt_gas * gwp$co2,
      grepl("ch4",gas_type) ~ mt_gas * gwp$ch4,
      grepl("n2o",gas_type) ~ mt_gas * gwp$n2o),
    units_emissions = case_when(#homogenize names
    grepl("co2",gas_type) ~ "Metric tons CO2",
    grepl("ch4",gas_type) ~ "Metric tons CH4",
    grepl("n2o",gas_type) ~ "Metric tons N2O"),
    inventory_year = as.numeric(inventory_year)) %>% 
  group_by(inventory_year,
         facility_id,
         county_name,
         units_emissions
  ) %>% 
  summarize(mt_gas = sum(mt_gas, na.rm = TRUE),
            metric_tons_co2e = sum(metric_tons_co2e, na.rm = TRUE))

#### non-refinery industrial ####

ghgrp_other <- ghgrp %>% 
  filter(!grepl("Y", industry_type_subparts), #remove refineries
         !grepl("D", industry_type_subparts), #remove power plants
         !(grepl("Waste", industry_type_sectors) & is.na(industrial_waste_landfills))) %>%   #remove municipal waste
  # remove facilities with only stationary combustion
  filter(
    rowSums(
      across(electricity_generation:industrial_waste_landfills,
             ~ as.integer(!is.na(.) & . > 0))
    ) > 0)
  
## convert ghgrp_other to mt of gas from mt_co2e
ghgrp_gas <- ghgrp_other %>%
  select(1:5,11:25, 39:41) %>%
  pivot_longer(
    cols = all_of(gas_cols),
    names_to = "gas_type",
    values_to = "value_emissions"
  ) %>%
  filter(!is.na(value_emissions)) %>%
  mutate(gas_type = str_replace_all(gas_type, "_emissions", ""),  
  mt_gas = case_when(
    gas_type == "co2_non_biogenic" ~ value_emissions / gwp$co2,
    gas_type == "methane_ch4" ~ value_emissions / 25, # they used IPCC 4th assessment values, so can't use stored values
    gas_type == "nitrous_oxide_n2o" ~ value_emissions /298, # they used IPCC 4th assessment values, so can't use stored values
    gas_type == "sf6" ~ value_emissions / gwp$sf6,
    gas_type == "nf3" ~ value_emissions / gwp$nf3,
    gas_type == "hfc" ~ value_emissions / gwp$hfc,
    gas_type == "pfc" ~ value_emissions / gwp$cf4,
    gas_type == "other_fully_fluorinated_ghg" ~ value_emissions / ((gwp$cf4 + gwp$nf3) / 2), # best guess for what these might be based on NAICS codes
    gas_type == "hfe" ~ value_emissions / 200, # ChatGPT estimate based on HFE-7xxx compounds, typically used in electronics manufacture
    TRUE ~ NA # unID'ed compounds - nothing to be done if we don't know what they are
    ))

### subtract away combustion emissions

process_gas <- left_join(
  ghgrp_gas %>% 
    mutate(units_emissions = case_when(#homogenize names
      grepl("co2",gas_type) ~ "Metric tons CO2",
      grepl("ch4",gas_type) ~ "Metric tons CH4",
      grepl("n2o",gas_type) ~ "Metric tons N2O",
      gas_type == "hfc" ~ "Metric tons HFC",
      gas_type == "hfe" ~ "Metric tons HFE",
      gas_type == "pfc" ~ "Metric tons PFC",
      gas_type == "sf6" ~ "Metric tons SF6",
      gas_type == "nf3" ~ "Metric tons NF3",
      gas_type == "other_fully_fluorinated_ghg" ~ "Metric tons Other Fully Fluorinated Gas",
      gas_type %in% c("very_short_lived_compounds", "other_gh_gs_metric_tons_co2e") ~ "Other/Unclassified",
      TRUE ~ "Other/Unclassified"
    ),
    inventory_year = as.numeric(inventory_year)) %>% 
    group_by(facility_id, county_name, inventory_year, units_emissions) %>% 
    summarize(mt_gas = sum(mt_gas, na.rm = TRUE)) %>% 
    ungroup(),  
  ind_fuel_combustion %>% 
    filter(units_emissions != "avg_activity") %>% 
    mutate(units_emissions = case_when(#homogenize names
      grepl("co2",units_emissions) ~ "Metric tons CO2",
      grepl("ch4",units_emissions) ~ "Metric tons CH4",
      grepl("n2o",units_emissions) ~ "Metric tons N2O")) %>% 
    group_by(facility_id, county_name, reporting_year, units_emissions) %>% 
    summarize(mt_gas = sum(values_emissions)) %>% 
    ungroup(),
  join_by(facility_id, 
          county_name,
          inventory_year == reporting_year,
          units_emissions)
) %>% 
  mutate(mt_gas = mt_gas.x - coalesce(mt_gas.y, 0),
         mt_gas = if_else(mt_gas < 0, 0, mt_gas),
         metric_tons_co2e = case_when(
           grepl("CH4", units_emissions) ~ mt_gas * gwp$ch4,
           grepl("N2O", units_emissions) ~ mt_gas * gwp$n2o,
           grepl("CO2", units_emissions) ~ mt_gas * gwp$co2,
           grepl("HFC", units_emissions) ~ mt_gas * gwp$hfc,
           grepl("HFE", units_emissions) ~ mt_gas * 200, # assuming average of hfe-7xxxx compound typical in electronics manufacturing
           grepl("PFC", units_emissions) ~ mt_gas * gwp$cf4,
           grepl("SF6", units_emissions) ~ mt_gas * gwp$sf6,
           grepl("NF3", units_emissions) ~ mt_gas * gwp$nf3,
           grepl("Fluorinated", units_emissions) ~ mt_gas * ((gwp$cf4 + gwp$nf3) / 2),
         )) %>% 
  filter(!is.na(metric_tons_co2e)) %>% 
  select(-mt_gas.x,
         -mt_gas.y)

# omit natural gas emissions from this batch
ind_fuel_gas <- ind_fuel_combustion %>% 
  filter(units_emissions != "avg_activity",
         general_fuel_type != "Natural Gas",
         doublecount == "No",
         !grepl("Y", industry_type_subparts),
         facility_id != 1004795) %>% 
  mutate(units_emissions = case_when(#homogenize names
    grepl("co2",units_emissions) ~ "Metric tons CO2",
    grepl("ch4",units_emissions) ~ "Metric tons CH4",
    grepl("n2o",units_emissions) ~ "Metric tons N2O")
    ) %>% 
  group_by(facility_id, county_name, reporting_year, units_emissions) %>% 
  summarize(mt_gas = sum(values_emissions)) %>% 
  ungroup() %>% 
  mutate(metric_tons_co2e = case_when(
           grepl("CH4", units_emissions) ~ mt_gas * gwp$ch4,
           grepl("N2O", units_emissions) ~ mt_gas * gwp$n2o,
           grepl("CO2", units_emissions) ~ mt_gas * gwp$co2)) %>% 
  rename(inventory_year = reporting_year)

#mpca data to fill in smaller emitters

epa_cities <- c(ind_fuel_combustion %>% 
  mutate(ctu_name = str_replace_all(city_name, "Saint", "St.")) %>% 
  distinct(ctu_name),
  ind_fuel_combustion %>% distinct(city_name))

industrial_fuel_gas_mpca <- readRDS("_industrial/data/mpca_fuel_emissions_by_gas.RDS") %>%
  filter(fuel_type != "Natural Gas",
         !ctu_name %in% epa_cities$ctu_name,
         !ctu_name %in% epa_cities$city_name) %>%
  group_by(inventory_year, county_name, unit_emissions) %>%
  summarize(mt_gas = sum(value_emissions)) %>%
  ungroup() %>%
  rename(
    units_emissions = unit_emissions
  ) %>%
  mutate(metric_tons_co2e = case_when(
    grepl("CH4", units_emissions) ~ mt_gas * gwp$ch4,
    grepl("N2O", units_emissions) ~ mt_gas * gwp$n2o,
    grepl("CO2", units_emissions) ~ mt_gas * gwp$co2
  )) %>%
  ungroup() 


industrial_gas <- bind_rows(refinery_gas %>% 
                              mutate(emission_source = "Refinery emissions"),
                            process_gas%>% 
                              mutate(emission_source = "Process emissions"),
                            ind_fuel_gas%>% 
                              mutate(emission_source = "Combustion emissions - EPA"),
                            industrial_fuel_gas_mpca%>% 
                              mutate(emission_source = "Combustion emissions - MPCA")) %>% 
  left_join(cprg_county %>% 
            st_drop_geometry() %>% 
            select(geoid, county_name))

industrial_gas %>%
  filter(inventory_year == 2022) %>%
  group_by(county_name) %>%
  summarize(value_emissions = sum(metric_tons_co2e)) %>% 
  arrange(value_emissions)

county_emissions %>%
  filter(
    value_emissions > 0,
    emissions_year == 2022,
    sector %in% c("Industrial", "Commercial"),
    category != "Building Fuel",
    category != "Electricity"
  ) %>%
  group_by(county_name) %>%
  summarize(value_emissions = sum(value_emissions)) %>% 
  arrange(value_emissions)

industrial_gas %>%
  filter(inventory_year == 2022) %>% 
  pull(metric_tons_co2e) %>% sum()

county_emissions %>%
  filter(
    value_emissions > 0,
    emissions_year == 2022,
    sector %in% c("Industrial", "Commercial"),
    category != "Building Fuel",
    category != "Electricity"
  ) %>% 
  pull(value_emissions) %>% sum()

5875372 / 6052870
# we are accounting for 97% of emissions. 
# There are some expected differences due to uncertain PFC IDs and therefore GWP, and outdated GWPs used in GHGRP
# additionally, some gases are not ID'ed so they are not able to be translated our of CO2e and are lost in the gas analysis

industrial_gas_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "inventory_year", class(industrial_gas$inventory_year), "Year of survey",
    "facility_id", class(industrial_gas$geoid), "Facility ID - EPA",
    "geoid", class(industrial_gas$geoid), "County GEOID",
    "county_name", class(industrial_gas$county_name), "County name",
    "emission_source", class(industrial_gas$emission_source), "Industrial source of emissions",
    "mt_gas ", class(industrial_gas$mt_gas), "Numerical value of emissions in metric tons",
    "units_emissions", class(industrial_gas$units_emissions), "Units and gas type of emissions",
    "metric_tons_co2e", class(industrial_gas$metric_tons_co2e ), "Metric tons of gas in CO2 equivalency"
  )

saveRDS(industrial_gas, "./_industrial/data/industrial_emissions_by_gas.rds")
saveRDS(industrial_gas_meta, "./_industrial/data/industrial_emissions_by_gas.rds_meta.rds")
