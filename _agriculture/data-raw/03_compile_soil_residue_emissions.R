source("R/_load_pkgs.R")
source("R/global_warming_potential.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")

crops <- read_rds("_agriculture/data/county_crop_production.rds")
ctu_crops <- read_rds("_agriculture/data/ctu_usda_crop_data.rds")

ag_constants <- readRDS("_agriculture/data/ag_constants_formatted.rds")
## convert to named vector for easier indexing
ag_constants_vec <- ag_constants %>%
  select(short_text, value) %>%
  tibble::deframe()

### format needed variables for emission estimates
ag_values <- left_join(
  ag_constants %>%
    filter(grepl("rcmr", short_text)) %>%
    mutate(
      crop_type = str_replace(short_text, "_rcmr", ""),
      value_rcmr = value
    ) %>%
    select(value_rcmr, crop_type),
  ag_constants %>%
    filter(grepl("rdmf", short_text)) %>%
    mutate(
      crop_type = str_replace(short_text, "_rdmf", ""),
      value_rdmf = value
    ) %>%
    select(value_rdmf, crop_type),
  by = c("crop_type")
) %>%
  left_join(.,
    ag_constants %>%
      filter(grepl("fra", short_text)) %>%
      mutate(
        crop_type = str_replace(short_text, "_fra", ""),
        value_fra = value
      ) %>%
      select(value_fra, crop_type),
    by = c("crop_type")
  ) %>%
  left_join(.,
    ag_constants %>%
      filter(grepl("ncr", short_text)) %>%
      mutate(
        crop_type = str_replace(short_text, "_ncr", ""),
        value_ncr = value
      ) %>%
      select(value_ncr, crop_type),
    by = c("crop_type")
  ) %>%
  mutate(crop_type = if_else(crop_type == "beans",
    "dry beans",
    crop_type
  ))


# determine N delivered to soils
soil_residue_emissions <- crops %>%
  left_join(., ag_values) %>%
  mutate(
    MT_N_to_soil = if_else(
      crop_type == "alfalfa",
      0, # no N to soil via residue for alfalfa
      metric_tons * value_rcmr * value_rdmf * value_fra * value_ncr
    ),
    MT_N_fixation = if_else(
      crop_type %in% c("alfalfa", "soybeans", "dry beans"),
      metric_tons * (1 + value_rcmr) * value_rdmf * ag_constants_vec["N_content_legume"],
      # last value is constant of N content of N-fixer biomass
      0
    )
  ) %>%
  filter(!is.na(MT_N_fixation)) %>%
  group_by(geoid, inventory_year) %>%
  summarize(mt_n_soils = sum(MT_N_to_soil + MT_N_fixation)) %>%
  mutate(
    # clarify how this is being converted
    mt_n2o = mt_n_soils * ag_constants_vec["EF_Dir"] * ag_constants_vec["N2O_N2"],
    mt_co2e = mt_n2o * gwp$n2o
  ) %>%
  # format to style guide
  rename(value_emissions = mt_n2o) %>%
  mutate(
    sector = "Agriculture",
    category = "Cropland",
    source = "Soil residue emissions",
    units_emissions = "Metric tons N2O",
    data_source = "USDA crop production survey",
    factor_source = "EPA SIT"
  ) %>%
  select(
    geoid, inventory_year, sector, category, source,
    data_source, factor_source, value_emissions, units_emissions, mt_co2e
  )

soil_residue_emissions_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "inventory_year", class(soil_residue_emissions$inventory_year), "Year of survey",
    "geoid", class(soil_residue_emissions$geoid), "County GEOID",
    "sector", class(soil_residue_emissions$sector), "Emissions sector. One of Transportation, Energy, Waste, Nature, Agriculture",
    "category", class(soil_residue_emissions$category), "Category of emissions within given sector",
    "source", class(soil_residue_emissions$source), "Source of emissions. Most detailed sub-category in this table",
    "data_source", class(soil_residue_emissions$data_source), "Activity data source",
    "factor_source", class(soil_residue_emissions$factor_source), "Emissions factor data source",
    "value_emissions", class(soil_residue_emissions$value_emissions), "Numerical value of emissions",
    "units_emissions", class(soil_residue_emissions$units_emissions), "Units and gas type of emissions",
    "mt_co2e", class(soil_residue_emissions$mt_co2e), "Metric tons of gas in CO2 equivalency"
  )

saveRDS(soil_residue_emissions, "./_agriculture/data/soil_residue_emissions.rds")
saveRDS(soil_residue_emissions_meta, "./_agriculture/data/soil_residue_emissions_meta.rds")


### repeat for CTUs

ctu_soil_residue_emissions <- ctu_crops %>%
  left_join(., ag_values) %>%
  mutate(
    MT_N_to_soil = if_else(
      crop_type == "alfalfa",
      0, # no N to soil via residue for alfalfa
      ctu_metric_tons  * value_rcmr * value_rdmf * value_fra * value_ncr
    ),
    MT_N_fixation = if_else(
      crop_type %in% c("alfalfa", "soybeans", "dry beans"),
      ctu_metric_tons * (1 + value_rcmr) * value_rdmf * ag_constants_vec["N_content_legume"],
      # last value is constant of N content of N-fixer biomass
      0
    )
  ) %>%
  filter(!is.na(MT_N_fixation)) %>%
  group_by(ctu_id, ctu_name, ctu_class, county_name, inventory_year) %>%
  summarize(mt_n_soils = sum(MT_N_to_soil + MT_N_fixation)) %>%
  mutate(
    # clarify how this is being converted
    mt_n2o = mt_n_soils * ag_constants_vec["EF_Dir"] * ag_constants_vec["N2O_N2"],
    mt_co2e = mt_n2o * gwp$n2o
  ) %>%
  # format to style guide
  rename(value_emissions = mt_n2o) %>%
  mutate(
    sector = "Agriculture",
    category = "Cropland",
    source = "Soil residue emissions",
    units_emissions = "Metric tons N2O",
    data_source = "USDA crop production survey",
    factor_source = "EPA SIT"
  ) %>%
  select(
    ctu_id, ctu_name, ctu_class, county_name, inventory_year, sector, category, source,
    data_source, factor_source, value_emissions, units_emissions, mt_co2e
  )

saveRDS(ctu_soil_residue_emissions, "./_agriculture/data/ctu_soil_residue_emissions.rds")
