source("R/_load_pkgs.R")
source("R/global_warming_potential.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")

## you'll need an API key to use USDA data (https://quickstats.nass.usda.gov/api)
key <- keyring::key_get("usda_key")

### load in saved SIT workbook sheets
ag_soils_residuals <- read_csv("_agriculture/data-raw/ag_soils_residuals.csv")
ag_soils_fertilizer <- read_csv("_agriculture/data-raw/ag_soils_fertilizer.csv")
ag_soils_manure <- read_csv("_agriculture/data-raw/ag_soils_manure.csv")
ag_residual_burning_ch4 <- read_csv("_agriculture/data-raw/ag_residual_burning_ch4.csv")
ag_residual_burning_n2o <- read_csv("_agriculture/data-raw/ag_residual_burning_n2o.csv")
ag_soils_liming <- read_csv("_agriculture/data-raw/ag_soils_liming.csv")
ag_constants <- read_csv("_agriculture/data-raw/ag_constants.csv")
ag_control <- read_csv("_agriculture/data-raw/ag_control.csv")
ag_fertilizer <- read_csv("_agriculture/data-raw/ag_fertilizer.csv")

# code should be revamped throughout to use the formatted ag constants data for cleaner analysis, but for now using at end of script
# formatted files
ag_constants_formatted <- readRDS("_agriculture/data/ag_constants_formatted.rds")
## convert to named vector for easier indexing
ag_constants_vec <- ag_constants_formatted %>%
  select(short_text, value) %>%
  tibble::deframe()

### create county names matched to USDA format
counties <- toupper(cprg_county$NAME)
counties <- if_else(counties == "ST. CROIX", "ST CROIX", counties)


# starting with biggest source of emissions - residuals

# grab field crop production data by county
usda_survey <- tidyUSDA::getQuickstat(
  sector = "CROPS",
  group = "FIELD CROPS",
  commodity = NULL,
  category = "PRODUCTION",
  domain = NULL,
  county = counties,
  key = key,
  program = "SURVEY",
  data_item = NULL,
  geographic_level = "COUNTY",
  year = as.character(2005:2021),
  state = c("MINNESOTA", "WISCONSIN"),
  geometry = TRUE,
  lower48 = TRUE,
  weighted_by_area = T
) %>%
  as.data.frame() %>%
  select(-geometry)

### extracting crops with conversions from EPA tool
### (corn and soy will likely account for >90% of emissions)
### this is now more neatly presented in ag_constants_formatted 
### but code needs to be rewritten
crop_conversions <- left_join(
  ag_constants[28:44, 1:3] %>%
    row_to_names(row_number = 1),
  ag_control[c(71, 74:89), c(1, 2, 6, 10)] %>%
    row_to_names(row_number = 1)
) %>%
  mutate_at(c(2:6), as.numeric) %>%
  clean_names()

### select crops for region
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
  select(year, county_name, short_desc, Value) %>%
  ### convert names to match ag conversions
  mutate(crop_type = case_when(
    grepl("BARLEY", short_desc) ~ "Barley",
    grepl("BEANS", short_desc) ~ "Dry Edible Beans",
    grepl("CORN", short_desc) ~ "Corn for Grain",
    grepl("ALFALFA", short_desc) ~ "Alfalfa",
    grepl("OATS", short_desc) ~ "Oats",
    grepl("SOYBEANS", short_desc) ~ "Soybeans",
    grepl("WHEAT", short_desc) ~ "All Wheat",
    TRUE ~ short_desc
  )) %>% # convert all units to metric tons
  left_join(., crop_conversions, by = c("crop_type" = "crop")) %>%
  mutate(metric_tons = case_when(
    grepl("Beans", crop_type) ~ Value * .0454, # hundredweights to metric tons
    grepl("Alfalfa", crop_type) ~ Value * 0.9072, # tons to metric tons
    TRUE ~ Value * metric_tons_bushel # bushels to metric tons
  )) %>% # determine N delivered to soils
  mutate(
    MT_N_to_soil = if_else(
      crop_type == "Alfalfa",
      0, # no N to soil via residue for alfalfa
      metric_tons * residue_crop_mass_ratio * residue_dry_matter_fraction * fraction_residue_applied * nitrogen_content_of_residue
    ),
    MT_N_fixation = if_else(
      crop_type %in% c("Alfalfa", "Soybeans", "Dry Edible Beans"),
      metric_tons * (1 + residue_crop_mass_ratio) * residue_dry_matter_fraction * 0.03,
      # last value is constant of N content of N-fixer biomass
      0
    )
  )


soil_residue_emissions <- usda_survey_formatted %>%
  filter(!is.na(MT_N_fixation)) %>%
  group_by(county_name, year) %>%
  summarize(mt_n_soils = sum(MT_N_to_soil + MT_N_fixation)) %>%
  mutate(
    mt_n2o = mt_n_soils * 0.01 * (44 / 28),
    mt_c2oe = mt_n2o * gwp$n2o
  )

# ### check
# ggplot(
#   soil_residue_emissions %>%
#     group_by(year, county_name) %>%
#     summarize(CO2e = sum(mt_c2oe)),
#   aes(x = year, y = CO2e, col = county_name)
# ) +
#   geom_line(size = 1.5) +
#   theme_bw()
# # quite a bit of scatter - 2013 in particular has major dip that should be investigated
# soil_residue_emissions %>%
#   filter(year == 2021) %>%
#   pull(mt_c2oe) %>%
#   sum()

#### fertilizer data ####
### some creativity is required here.
### Fertilizer purchases are recorded by MN Dept of Ag - 
### waiting to see if data exists outside of pdfs
### Fertilizer expenses are recorded in USDA 5 year census
### Fertilizer application is estimated in SIT for state, but data source is unclear

### need to extract state fertilizer application estimates by year 
### and then multiply by year constants to convert to organic vs synthetic rates
ag_fert_formatted <- left_join(
  ag_fertilizer[c(2, 4:53), ] %>% # begin formatting to machine readable
    mutate(`Consumption of Primary Plant Nutrients: Total Nitrogen (Metric Tons)` = replace(
      `Consumption of Primary Plant Nutrients: Total Nitrogen (Metric Tons)`,
      is.na(`Consumption of Primary Plant Nutrients: Total Nitrogen (Metric Tons)`),
      "state"
    )) %>%
    row_to_names(1) %>%
    pivot_longer(
      cols = -1,
      names_to = "year",
      values_to = "value"
    ) %>%
    filter(state %in% c("MN", "WI")) %>%
    mutate(
      value = as.numeric(str_replace_all(value, ",", "")),
      year = as.numeric(year)
    ),
  ag_fertilizer[c(2, 56), 2:33] %>% # percentage fertilizer amount that is synthetic
    row_to_names(1) %>%
    pivot_longer(
      cols = 1:32,
      names_to = "year",
      values_to = "percent_synthetic"
    ) %>%
    mutate(
      percent_synthetic = as.numeric(str_replace_all(percent_synthetic, "%", "")) / 100,
      year = as.numeric(year)
    ),
  by = "year"
) %>%
  mutate(
    mt_n_synthetic = value * percent_synthetic,
    mt_n_organic = value * (1 - percent_synthetic)
  ) %>%
  filter(!is.na(value))

# ag_fert_formatted # check your dataframe

### pull in county values of fertilizer purchased.
###  Will use this to apportion fertilizer applied (above)
usda_fertilizer_mn_wi <- tidyUSDA::getQuickstat(
  sector = "ECONOMICS",
  group = "EXPENSES",
  commodity = "FERTILIZER TOTALS",
  category = "EXPENSE",
  domain = NULL,
  county = NULL,
  key = key,
  program = "CENSUS",
  data_item = "FERTILIZER TOTALS, INCL LIME & SOIL CONDITIONERS - EXPENSE, MEASURED IN $",
  geographic_level = "COUNTY",
  year = as.character(2002:2022),
  state = c("MINNESOTA", "WISCONSIN"),
  geometry = TRUE,
  lower48 = TRUE,
  weighted_by_area = T
) %>%
  as.data.frame() %>%
  select(-geometry)

# usda_fertilizer_mn_wi %>%
#   filter(is.na(Value)) %>%
#   arrange(year) %>%
#   select(state_name, county_name, year)
### there are 9 missing values by county. 
### 2 MN, 2 WI in 2002. 2 MN in 2007, 3 WI in 2017.
###  None are counties of focus except Ramsey in 2002, which has limited ag.

# get proportion of county fertilizer purchase to state
usda_fert_prop <- usda_fertilizer_mn_wi %>%
  filter(!(NAME == "Washington" & state_name == "WISCONSIN")) %>% 
  # remove washington co, WI
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
  usda_fert_prop
) %>% # merged with our populated fertilizer proportion data, it creates NAs wherever data is missing
  mutate(fert_prop = if_else(year %in% c(2002, 2007, 2012, 2017, 2022) & is.na(fert_prop), 0, fert_prop)) %>% # add 0 as Ramsey value in 2002
  group_by(county_name) %>%
  arrange(year) %>%
  mutate(
    fert_prop = zoo::na.approx(fert_prop, na.rm = FALSE), # this function linearly interpolates NAs between known values, following the group_by
    data_type = ifelse(year %in% c(2002, 2007, 2012, 2017, 2022), "census", "interpolated") # marking whether values are from the census or interpolation
  )

#### merge fertilize proportion estimates with state fertilizer values
county_fertilizer_emissions <- left_join(
  left_join(fert_prop_interpolated, cprg_county %>%
    mutate(county_name = if_else(NAME == "St. Croix",
      "ST CROIX",
      toupper(NAME)
    )) %>%
    as.data.frame() %>%
    select(county_name, STATE_ABB)),
  ag_fert_formatted,
  by = c("STATE_ABB" = "state", "year" = "year")
) %>%
  filter(year >= 2005 & year <= 2021) %>%
  mutate(
    mt_n_synthetic_cty = fert_prop * mt_n_synthetic,
    mt_n_organic_cty = fert_prop * mt_n_organic
  ) %>%
  #### separating mutate as this is where we will calculate emissions from estimated fertilizer application
  mutate(
    n2o_direct = (mt_n_synthetic_cty * (1 - as.numeric(ag_constants[21, 1])) + # unvolatilized N from synthetic fertilizer
      mt_n_organic_cty * as.numeric(ag_constants[19, 1]) * (1 - as.numeric(ag_constants[20, 1]))) * # unvolatilized N from organic fertilizer
      as.numeric(ag_constants[22, 1]) * as.numeric(ag_constants[10, 1]), ## multiplied by the EF of unvolatilized N and N2O N: N2O
    n2o_indirect = (mt_n_synthetic_cty * as.numeric(ag_constants[21, 1]) + # volatilized N from synthetic fertilizer
      mt_n_organic_cty * as.numeric(ag_constants[19, 1]) * as.numeric(ag_constants[20, 1])) * # volatized N from organic fertilizer
      as.numeric(ag_constants[23, 1]) * as.numeric(ag_constants[10, 1]), ## multiplied by the EF of volatized N and N2O N: N2O
    mt_n2o = n2o_direct + n2o_indirect,
    mt_co2e = mt_n2o * gwp$n2o
  ) %>%
  select(year, county_name, data_type, mt_n_synthetic_cty, mt_n_organic_cty, mt_n2o, mt_co2e)

### check
# ggplot(
#   county_fertilizer_emissions %>%
#     group_by(year, county_name) %>%
#     summarize(CO2e = sum(mt_co2e)),
#   aes(x = year, y = CO2e, col = county_name)
# ) +
#   geom_line(size = 1.5) +
#   theme_bw()
# county_fertilizer_emissions %>%
#   filter(year == 2021, !county_name %in% c("SHERBURNE", "CHISAGO", "PIERCE", "ST CROIX")) %>%
#   pull(mt_co2e) %>%
#   sum()

### there is an additional estimate from the ag soils-animal worksheet that uses fertilizer data to estimate emissions from runoff and leaching.
### we should seek out additional documentation to better described these direct and indirect emissions,
### but language seems to imply fertilizer that stays on field vs that transported (into waterways, off cropland vegetation) perhaps
county_fertilizer_runoff_emissions <- county_fertilizer_emissions %>%
  select("year", "county_name", "data_type", "mt_n_synthetic_cty", "mt_n_organic_cty") %>%
  mutate(
    mt_n2o = (mt_n_synthetic_cty + mt_n_organic_cty) * ag_constants_vec["LeachEF"] * ag_constants_vec["LeachEF2"] * ag_constants_vec["N2O_N2"],
    mt_co2e = mt_n2o * gwp$n2o
  )

cropland_emissions <- bind_rows(
  soil_residue_emissions %>% select(year, county_name, MT_gas = mt_n2o, MT_co2e = mt_c2oe) %>%
    mutate(category = "cropland", source = "crop_residue_emissions", gas_type = "n2o"),
  county_fertilizer_emissions %>% select(year, county_name, MT_gas = mt_n2o, MT_co2e = mt_co2e) %>%
    mutate(category = "cropland", source = "crop_fertilizer_emissions", gas_type = "n2o"),
  county_fertilizer_runoff_emissions %>% select(year, county_name, MT_gas = mt_n2o, MT_co2e = mt_co2e) %>%
    mutate(category = "cropland", source = "runoff_fertilizer_emissions", gas_type = "n2o")
) %>%
  filter(year != 2022) %>%
  mutate(county_name = if_else(county_name == "ST CROIX", "St. Croix", str_to_sentence(county_name))) # match case to other files

cropland_emissions_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "year", class(cropland_emissions$year), "Year of survey",
    "county_name", class(cropland_emissions$county_name), "County name",
    "MT_co2e", class(cropland_emissions$MT_co2e), "Metric tons of CO2 equivalency",
    "MT_gas", class(cropland_emissions$MT_gas), "Total metric tons of gas emitted from source",
    "category", class(cropland_emissions$category), "Subsector category",
    "source", class(cropland_emissions$category), "Detailed description of emission source",
    "gas_type", class(cropland_emissions$gas_type), "Greenhouse gas emitted from source",
  )

saveRDS(cropland_emissions, "./_agriculture/data/county_cropland_emissions_2005_2021.rds")
saveRDS(cropland_emissions_meta, "./_agriculture/data/county_cropland_emissions_2005_2021_meta.rds")
