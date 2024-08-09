source("R/_load_pkgs.R")
source("_energy/data-raw/_energy_emissions_factors.R")
source("R/plot_county_emissions.R")

#add to lockfile once finalized
library(stringr)

cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_ctu_point <- readRDS("_meta/data/cprg_ctu.RDS") %>%
  st_make_valid() %>%
  mutate(geometry = st_simplify(geometry, preserveTopology = TRUE),
         CTU_NAME = str_to_title(CTU_NAME)
         ) %>%
  clean_names() %>%
  group_by(ctu_name,
           county_nam,
           #ctu_class,
           state) %>%
  summarize(geometry = st_centroid(st_union(geometry))) %>%
  ungroup() 
  

# NREL SLOPE energy consumption and expenditure data download, cleaning, and viz

# 1 Mmbtu is 0.293071 MWH
mmbtu_to_mwh <- 0.293071

# 1000 cubic feet is 1.038 MMBtu
# https://www.naturalgasintel.com/natural-gas-converter/
# 1 mmbtu is 1 mcf
mmbtu_to_mcf <- 1

# download from NREL directly
download.file("https://gds-files.nrel.gov/slope/energy_consumption_expenditure_business_as_usual.zip",
  destfile = "_energy/data-raw/nrel_slope/energy_consumption_expenditure_business_as_usual.zip"
)
unzip("_energy/data-raw/nrel_slope/energy_consumption_expenditure_business_as_usual.zip",
  exdir = "_energy/data-raw/nrel_slope/"
)


nrel_slope_cprg_county <- read.csv("_energy/data-raw/nrel_slope/energy_consumption_expenditure_business_as_usual_county.csv") %>%
  clean_names() %>%
  inner_join(cprg_county,
             by = c(
               "state_name" = "STATE",
               "county_name" = "NAME"
             )
  ) %>%
  mutate(source = ifelse(source == "ng", "Natural gas", "Electricity")) %>%
  select(-geometry)

nrel_slope_cprg_city <- cprg_ctu_point %>%
  left_join(read.csv("_energy/data-raw/nrel_slope/energy_consumption_expenditure_business_as_usual_city.csv") %>% 
              clean_names() %>%
              mutate(city_name = str_replace_all(city_name, "St\\.", "Saint")),
             by = c(
               "state" = "state_name",
               "ctu_name" = "city_name"
             )
  ) %>%
  mutate(source = ifelse(source == "ng", "Natural gas", "Electricity"))


write.csv(nrel_slope_cprg_county, here("_energy",
                                       "data-raw",
                                       "nrel_slope",
                                       "MNWI",
                                       "nrel_slope_cprg_county.csv")
          )

write.csv(nrel_slope_cprg_city, here("_energy",
                 "data-raw",
                 "nrel_slope",
                 "MNWI",
                 "nrel_slope_cprg_city.csv")
          )

st_write(nrel_slope_cprg_city %>% select(-expenditure_us_dollars), here("_energy",
                                    "data-raw",
                                    "nrel_slope",
                                    "MNWI",
                                    "nrel_slope_cprg_city.shp"),
         append = FALSE
         )


# city-level

# nrel_slope_state <- read.csv("_energy/data-raw/nrel_slope/energy_consumption_expenditure_business_as_usual_state.csv") %>%
#   clean_names()


nrel_emissions_inv_county <- bind_rows(
  # electricity emissions
  nrel_slope_cprg_county %>%
    #Emission INVENTORY is < 2025, forecasts is >= 2025
    filter(year < 2025) %>%
    filter(source == "Electricity") %>%
    rowwise() %>%
    mutate(
      # convert mmbtu to Mwh
      consumption_mwh = consumption_mm_btu * mmbtu_to_mwh,
      # apply emission factor and convert to metric tons
      co2 = (consumption_mwh * eGRID_MROW_emissionsFactor_CO2) %>%
        units::as_units("lb") %>%
        units::set_units("ton") %>%
        as.numeric(),
      ch4 = (consumption_mwh * eGRID_MROW_emissionsFactor_CH4) %>%
        units::as_units("lb") %>%
        units::set_units("ton") %>%
        as.numeric(),
      n2o = (consumption_mwh * eGRID_MROW_emissionsFactor_N2O) %>%
        units::as_units("lb") %>%
        units::set_units("ton") %>%
        as.numeric(),
      co2e =
        co2 +
          (ch4 * gwp$n2o) +
          (n2o * gwp$n2o)
    ),
  # natural gas emissions
  nrel_slope_cprg_county %>%
    filter(source == "Natural gas") %>%
    rowwise() %>%
    mutate(
      # convert mmbtu to mcf
      consumption_mcf = consumption_mm_btu * mmbtu_to_mcf,
      # apply emission factor and convert to metric tons
      co2 = (consumption_mcf * epa_emissionsHub_naturalGas_factor_lbsCO2_perMCF) %>%
        units::as_units("lb") %>%
        units::set_units("ton") %>%
        as.numeric(),
      ch4 = (consumption_mcf * epa_emissionsHub_naturalGas_factor_lbsCH4_perMCF) %>%
        units::as_units("lb") %>%
        units::set_units("ton") %>%
        as.numeric(),
      n2o = (consumption_mcf * epa_emissionsHub_naturalGas_factor_lbsN2O_perMCF) %>%
        units::as_units("lb") %>%
        units::set_units("ton") %>%
        as.numeric(),
      co2e =
        co2 +
          (ch4 * gwp$n2o) +
          (n2o * gwp$n2o)
    )
) %>%
  mutate(
    category = ifelse(sector == "residential", "Residential", "Non-residential"),
    sector_raw = sector,
    sector = "Energy"
  )

# find county proportions by year and source
nrel_emissions_region <- nrel_emissions_inv_county %>%
  group_by(year, sector, sector_raw, category, source) %>%
  summarize(
    consumption_mm_btu = sum(consumption_mm_btu),
    expenditure_us_dollars = sum(expenditure_us_dollars),
    co2e = sum(co2e)
  )

nrel_emissions_region %>%
  filter(
    year == 2021,
    source == "Electricity"
  ) %>%
  group_by(year, source, sector_raw) %>%
  summarize(co2e = sum(co2e))

# plot_ly(
#   data = nrel_emissions_region %>%
#     filter(year == 2021),
#   x = ~sector_raw,
#   y = ~co2e,
#   color = ~source
# )


nrel_slope_proportions <- nrel_emissions_inv_county %>%
  group_by(county_name, year, source) %>%
  select(county_name, year, source, sector_raw, co2e) %>%
  pivot_wider(
    names_from = sector_raw,
    values_from = co2e
  ) %>%
  rowwise() %>%
  summarize(
    total = commercial + residential + industrial,
    commercial = commercial / total,
    industrial = industrial / total,
    residential = residential / total,
    .groups = "keep"
  ) %>%
  filter(year == 2021) %>%
  ungroup() %>%
  mutate(county = county_name) %>%
  select(-total, -county_name)

saveRDS(nrel_emissions_inv_county, "_energy/data-raw/nrel_slope/nrel_emissions_inv_county.RDS")
saveRDS(nrel_slope_proportions, "_energy/data-raw/nrel_slope/nrel_slope_proportions.RDS")

# plot_ly(
#   data = nrel_emissions_region %>%
#     filter(source == "Electricity"),
#   x = ~year,
#   y = ~co2e,
#   color = ~str_to_sentence(category),
#   type = "bar"
# ) %>%
#   plotly_layout(
#     main_title = "Electricity",
#     subtitle = "",
#     x_title = "Year",
#     y_title = "Metric tones CO<sub>2</sub>e",
#     legend_title = "Sector"
#   )
#
#
# plot_ly(
#   data = nrel_emissions_region %>%
#     filter(source == "Natural gas"),
#   x = ~year,
#   y = ~co2e,
#   color = ~str_to_sentence(category),
#   type = "bar"
# ) %>%
#   plotly_layout(
#     main_title = "Natural gas",
#     subtitle = "",
#     x_title = "Year",
#     y_title = "Metric tones CO<sub>2</sub>e",
#     legend_title = "Sector"
#   )
