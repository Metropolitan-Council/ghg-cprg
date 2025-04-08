source("R/_load_pkgs.R")
source("R/global_warming_potential.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")

#### fertilizer data ####
### some creativity is required here.
### Fertilizer purchases are recorded by MN Dept of Ag -
### waiting to see if data exists outside of pdfs
### Fertilizer expenses are recorded in USDA 5 year census
fert_prop <- readRDS("./_agriculture/data/county_fertilizer_proportion.rds")
ctu_fert_prop <- readRDS("_agriculture/data/ctu_fertilizer_proportion.rds")

# formatted files
ag_constants <- readRDS("_agriculture/data/ag_constants.rds")

## convert to named vector for easier indexing
ag_constants_vec <- ag_constants %>%
  dplyr::select(short_text, value) %>%
  tibble::deframe()

### Fertilizer application is estimated in SIT for state, but data source is unclear

### need to extract state fertilizer application estimates by year
### and then multiply by year constants to convert to organic vs synthetic rates
state_fertilizer <- readxl::read_xlsx("_agriculture/data-raw/ag-module.xlsx",
  sheet = "FertilizerData",
  range = "A3:AG54"
) %>%
  filter(!row_number() == 1) %>%
  rename(state = `...1`) %>%
  filter(state %in% c("MN", "WI")) %>%
  pivot_longer(
    cols = -1, names_to = "year",
    values_to = "metric_tons_n_applied",
    values_transform = list(metric_tons_n_applied = as.numeric)
  ) %>%
  left_join(
    .,
    # percentage fertilizer amount that is synthetic
    readxl::read_xlsx("_agriculture/data-raw/ag-module.xlsx",
      sheet = "FertilizerData",
      range = "A3:AG60"
    ) %>%
      filter(!row_number() %in% c(1:53)) %>%
      rename(fertilizer_type = `...1`) %>%
      pivot_longer(
        cols = -1, names_to = "year",
        values_to = "percentage",
        values_transform = list(percentage = as.numeric)
      ) %>%
      # just take synthetic value for now
      filter(fertilizer_type == "Synthetic")
  ) %>%
  mutate(
    mt_n_synthetic = metric_tons_n_applied * percentage,
    mt_n_organic = metric_tons_n_applied * (1 - percentage),
    year = as.numeric(year)
  ) %>%
  filter(!is.na(metric_tons_n_applied))


#### merge fertilize proportion estimates with state fertilizer values
county_fertilizer_emissions <- left_join(fert_prop, cprg_county %>%
  st_drop_geometry() %>%
  select(geoid, state_abb)) %>%
  left_join(.,
    state_fertilizer,
    by = c("state_abb" = "state", "inventory_year" = "year")
  ) %>%
  filter(inventory_year >= 2005) %>%
  mutate(
    mt_n_synthetic_cty = fertilizer_proportion * mt_n_synthetic,
    mt_n_organic_cty = fertilizer_proportion * mt_n_organic
  ) %>%
  #### separating mutates as this is where we will calculate emissions
  #### from estimated fertilizer application
  mutate(
    # unvolatilized N from synthetic fertilizer
    n2o_direct = (mt_n_synthetic_cty * (1 - ag_constants_vec["VolSyn"]) +
      # un-volatilized N from organic fertilizer
      mt_n_organic_cty * ag_constants_vec["NOrg"] * (1 - ag_constants_vec["VolOrg"])) *
      ## multiplied by the EF of un-volatilized N and N2O N: N2O
      ag_constants_vec["EF_Dir"] * ag_constants_vec["N2O_N2"],
    # volatilized N from synthetic fertilizer
    n2o_indirect = (mt_n_synthetic_cty * ag_constants_vec["VolSyn"] +
      # volatized N from organic fertilizer
      mt_n_organic_cty * ag_constants_vec["NOrg"] * ag_constants_vec["VolOrg"]) *
      ## multiplied by the EF of volatized N and N2O N: N2O
      ag_constants_vec["Vol_EF"] * ag_constants_vec["N2O_N2"],
    mt_n2o = n2o_direct + n2o_indirect,
    mt_co2e = mt_n2o * gwp$n2o
  ) %>%
  select(
    inventory_year, geoid, county_name, data_type, mt_n_synthetic_cty, mt_n_organic_cty,
    n2o_direct, n2o_indirect, # KS retain these
    mt_n2o, mt_co2e
  )

# save intermediate activity data RDS

county_fertilizer_activity <- county_fertilizer_emissions %>%
  select(1:6) %>%
  pivot_longer(5:6,
    names_to = "fertilizer_type",
    values_to = "metric_tons_applied"
  ) %>%
  mutate(fertilizer_type = case_when(
    grepl("synth", fertilizer_type) ~ "Synthetic fertilizer",
    grepl("orga", fertilizer_type) ~ "Organic fertilizer"
  ))

saveRDS(county_fertilizer_activity, "./_agriculture/data/county_fertilizer_activity.rds")

### check
ggplot(
  county_fertilizer_emissions %>%
    group_by(inventory_year, county_name) %>%
    summarize(CO2e = sum(mt_co2e)),
  aes(x = inventory_year, y = CO2e, col = county_name)
) +
  geom_line(size = 1.5) +
  theme_bw()
county_fertilizer_emissions %>%
  filter(inventory_year == 2021, !county_name %in% c("SHERBURNE", "CHISAGO", "PIERCE", "ST CROIX")) %>%
  pull(mt_co2e) %>%
  sum()


### there is an additional estimate from the ag soils-animal worksheet that uses fertilizer data to estimate emissions from runoff and leaching.
### we should seek out additional documentation to better described these direct and indirect emissions,
### but language seems to imply fertilizer that stays on field vs that transported (into waterways, off cropland vegetation) perhaps
county_fertilizer_runoff_emissions <- county_fertilizer_emissions %>%
  select(inventory_year, geoid, county_name, data_type, mt_n_synthetic_cty, mt_n_organic_cty) %>%
  ## KS: Minor correction
  ## the Ag_Soils-Animals worksheet uses the estimates of unvolatized synthetic
  ## and organic fertilizer, not the total fertilizer use of each kind
  mutate(
    # unvolatilized N from synthetic fertilizer
    mt_uv_n_synthetic_cty = mt_n_synthetic_cty * (1 - ag_constants_vec["VolSyn"]),
    # unvolatilized N from organic fertilizer
    mt_uv_n_organic_cty = mt_n_organic_cty * ag_constants_vec["NOrg"] * (1 - ag_constants_vec["VolOrg"]),
    mt_n2o = (mt_uv_n_synthetic_cty + mt_uv_n_organic_cty) *
      ag_constants_vec["LeachEF"] * ag_constants_vec["LeachEF2"] *
      ag_constants_vec["N2O_N2"],
    mt_co2e = mt_n2o * gwp$n2o
  )
#
#   mutate(
#     mt_n2o = (mt_n_synthetic_cty + mt_n_organic_cty) *
#       ag_constants_vec["LeachEF"] * ag_constants_vec["LeachEF2"] *
#       ag_constants_vec["N2O_N2"],
#     mt_co2e = mt_n2o * gwp$n2o
#   )

fertilizer_emissions <- bind_rows(
  county_fertilizer_emissions %>% ungroup() %>%
    select(inventory_year, geoid, value_emissions = mt_n2o, mt_co2e) %>%
    mutate(category = "cropland", source = "Onsite_fertilizer_emissions", units_emissions = "Metric tons N2O"),
  county_fertilizer_runoff_emissions %>% ungroup() %>%
    select(inventory_year, geoid, value_emissions = mt_n2o, mt_co2e) %>%
    mutate(category = "cropland", source = "Runoff_fertilizer_emissions", units_emissions = "Metric tons N2O")
) %>%
  mutate(
    sector = "Agriculture",
    category = str_to_sentence(category),
    source = str_to_sentence(gsub("_", " ", source)),
    data_source = "USDA fertilizer purchase census and EPA SIT fertilizer application data",
    factor_source = "EPA SIT"
  ) %>%
  select(
    geoid, inventory_year, sector, category, source,
    data_source, factor_source, value_emissions, units_emissions, mt_co2e
  )

fertilizer_emissions_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "geoid", class(fertilizer_emissions$geoid), "County GEOID",
    "inventory_year", class(fertilizer_emissions$inventory_year), "Year of survey",
    "sector", class(fertilizer_emissions$sector), "Emissions sector. One of Transportation, Energy, Waste, Nature, Agriculture",
    "category", class(fertilizer_emissions$category), "Category of emissions within given sector",
    "source", class(fertilizer_emissions$source), "Source of emissions. Most detailed sub-category in this table",
    "data_source", class(fertilizer_emissions$data_source), "Activity data source",
    "factor_source", class(fertilizer_emissions$factor_source), "Emissions factor data source",
    "value_emissions", class(fertilizer_emissions$value_emissions), "Numerical value of emissions",
    "units_emissions", class(fertilizer_emissions$units_emissions), "Units and gas type of emissions",
    "mt_co2e", class(fertilizer_emissions$mt_co2e), "Metric tons of gas in CO2 equivalency"
  )

saveRDS(fertilizer_emissions, "./_agriculture/data/fertilizer_emissions.rds")
saveRDS(fertilizer_emissions_meta, "./_agriculture/data/fertilizer_emissions_meta.rds")

### repeat the same for CTUs

ctu_fertilizer_emissions <- left_join(
  ctu_fert_prop %>%
    mutate(state = if_else(state_name == "Minnesota",
      "MN",
      "WI"
    )),
  state_fertilizer,
  by = c("state", "inventory_year" = "year")
) %>%
  filter(inventory_year >= 2005) %>%
  mutate(
    mt_n_synthetic_cty = ctu_fertilizer_proportion * mt_n_synthetic,
    mt_n_organic_cty = ctu_fertilizer_proportion * mt_n_organic
  ) %>%
  #### separating mutates as this is where we will calculate emissions
  #### from estimated fertilizer application
  mutate(
    # unvolatilized N from synthetic fertilizer
    n2o_direct = (mt_n_synthetic_cty * (1 - ag_constants_vec["VolSyn"]) +
      # un-volatilized N from organic fertilizer
      mt_n_organic_cty * ag_constants_vec["NOrg"] * (1 - ag_constants_vec["VolOrg"])) *
      ## multiplied by the EF of un-volatilized N and N2O N: N2O
      ag_constants_vec["EF_Dir"] * ag_constants_vec["N2O_N2"],
    # volatilized N from synthetic fertilizer
    n2o_indirect = (mt_n_synthetic_cty * ag_constants_vec["VolSyn"] +
      # volatized N from organic fertilizer
      mt_n_organic_cty * ag_constants_vec["NOrg"] * ag_constants_vec["VolOrg"]) *
      ## multiplied by the EF of volatized N and N2O N: N2O
      ag_constants_vec["Vol_EF"] * ag_constants_vec["N2O_N2"],
    mt_n2o = n2o_direct + n2o_indirect,
    mt_co2e = mt_n2o * gwp$n2o
  ) %>%
  select(
    inventory_year, ctu_id, ctu_name, ctu_class, county_name, data_type, mt_n_synthetic_cty, mt_n_organic_cty,
    n2o_direct, n2o_indirect,
    mt_n2o, mt_co2e
  )

# save intermediate activity data RDS

ctu_fertilizer_activity <- ctu_fertilizer_emissions %>%
  select(1:8) %>%
  pivot_longer(7:8,
    names_to = "fertilizer_type",
    values_to = "metric_tons_applied"
  ) %>%
  mutate(fertilizer_type = case_when(
    grepl("synth", fertilizer_type) ~ "Synthetic fertilizer",
    grepl("orga", fertilizer_type) ~ "Organic fertilizer"
  ))

saveRDS(ctu_fertilizer_activity, "./_agriculture/data/ctu_fertilizer_activity.rds")


ctu_fertilizer_runoff_emissions <- ctu_fertilizer_emissions %>%
  select(inventory_year, ctu_id, ctu_name, ctu_class, county_name, data_type, mt_n_synthetic_cty, mt_n_organic_cty) %>%
  ## KS: Minor correction
  ## the Ag_Soils-Animals worksheet uses the estimates of unvolatized synthetic
  ## and organic fertilizer, not the total fertilizer use of each kind
  mutate(
    # unvolatilized N from synthetic fertilizer
    mt_uv_n_synthetic_cty = mt_n_synthetic_cty * (1 - ag_constants_vec["VolSyn"]),
    # unvolatilized N from organic fertilizer
    mt_uv_n_organic_cty = mt_n_organic_cty * ag_constants_vec["NOrg"] * (1 - ag_constants_vec["VolOrg"]),
    mt_n2o = (mt_uv_n_synthetic_cty + mt_uv_n_organic_cty) *
      ag_constants_vec["LeachEF"] * ag_constants_vec["LeachEF2"] *
      ag_constants_vec["N2O_N2"],
    mt_co2e = mt_n2o * gwp$n2o
  )

ctu_total_fertilizer_emissions <- bind_rows(
  ctu_fertilizer_emissions %>% ungroup() %>%
    select(inventory_year, ctu_id, ctu_name, ctu_class, county_name, value_emissions = mt_n2o, mt_co2e) %>%
    mutate(category = "cropland", source = "Onsite_fertilizer_emissions", units_emissions = "Metric tons N2O"),
  ctu_fertilizer_runoff_emissions %>% ungroup() %>%
    select(inventory_year, ctu_id, ctu_name, ctu_class, county_name, value_emissions = mt_n2o, mt_co2e) %>%
    mutate(category = "cropland", source = "Runoff_fertilizer_emissions", units_emissions = "Metric tons N2O")
) %>%
  mutate(
    sector = "Agriculture",
    category = str_to_sentence(category),
    source = str_to_sentence(gsub("_", " ", source)),
    data_source = "USDA fertilizer purchase census and EPA SIT fertilizer application data",
    factor_source = "EPA SIT"
  ) %>%
  select(
    ctu_id, ctu_name, ctu_class, county_name, inventory_year, sector, category, source,
    data_source, factor_source, value_emissions, units_emissions, mt_co2e
  )

saveRDS(ctu_total_fertilizer_emissions, "./_agriculture/data/ctu_fertilizer_emissions.rds")
