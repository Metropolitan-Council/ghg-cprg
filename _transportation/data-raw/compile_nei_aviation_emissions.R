#### This script processes EPA Air Emissions point data to account for airport emissions

source("R/_load_pkgs.R")
source("R/global_warming_potential.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")

epa_airport <- readRDS("_transportation/data-raw/epa/air_emissions_modeling/point_mn_wi.RDS")
nei_airport <- readRDS("_transportation/data-raw/epa/air_emissions_modeling/air_emissions_modeling_mn_wi/point/2022hd_airports_2022_point_top51_adjusted_ATL_data_20250428_27may2025_v0.RDS")
msp_emissions <- readRDS("_transportation/data/aviation_emissions.rds")

epa_airport_cprg <- cprg_county %>% 
  st_drop_geometry() %>% 
  select(geoid, county_name) %>% 
  left_join(epa_airport %>% 
              select(region_cd, 
                     facility_id, 
                     unit_id, 
                     unit_type_code,
                     facility_name, 
                     scc, 
                     scc6, 
                     poll, 
                     emissions_short_tons,
                     calc_year),
            by = c("geoid" = "region_cd")) %>% 
  filter(scc6 %in% unique(nei_airport$scc6))

### load in state data to anchor to

mpca_aviation <-
  read_rds("_meta/data/mpca_ghg_inv_2022.RDS") %>%
  filter(Source == "Aviation") %>%
  mutate(inventory_year = as.numeric(inventory_year)) %>%
  group_by(Sector, Source, inventory_year) %>%
  summarise(state_mt_co2e = sum(co2e))


airport_co2 <- epa_airport_cprg %>% 
  filter(poll == "CO2",
         facility_id != 6151711) %>% #MSP intl
  group_by(county_name, calc_year) %>% 
  summarize(tons_co2 = sum(emissions_short_tons)) %>% 
  ungroup() %>% 
  mutate(mt_co2 = as.numeric(tons_co2 * units::as_units("short_ton") %>%
      units::set_units("metric_ton")
  ),
  county_share = mt_co2 / mpca_aviation$state_mt_co2e[mpca_aviation$inventory_year == 2020]
  ) %>% 
  cross_join(mpca_aviation %>% 
               filter(inventory_year >= 2005) %>% 
               select(inventory_year, state_mt_co2e)) %>% 
  mutate(inferred_co2 = state_mt_co2e * county_share)
  

airport_out <- airport_co2 %>%
  select(geog_name = county_name, inventory_year, value_emissions = inferred_co2) %>%
  mutate(
    sector = "Transportation",
    category = "Aviation",
    source = "Reliever airport",
    units_emissions = "Metric tons CO2e",
    data_source = case_when(
      inventory_year == 2020 ~ "National Emissions Inventory: Point",
      TRUE~ "MPCA state aviation emission proportional analysis"
    ),
    factor_source = "EPA GHG Emission Factor Hub"
  )

airport_emissions_meta <- tibble::tribble(
  ~"Column", ~"Class", ~"Description",
  "sector", class(airport_out$sector), "Emissions sector",
  "category", class(airport_out$category), "Emissions subsector category",
  "source", class(airport_out$source), "Emissions source",
  "inventory_year", class(airport_out$inventory_year), "Inventory year of emissions",
  "value_emissions", class(airport_out$value_emissions), "Numeric value of emissions",
  "units_emissions", class(airport_out$units_emissions), "Units of emissions",
  "geog_name", class(airport_out$geog_name), "Geographic location",
  "data_source", class(airport_out$data_source), "Source of activity data used to calculate emissions",
  "factor_source", class(airport_out$factor_source), "Source of emission factor for translating activity to emissions"
)

saveRDS(airport_out, "./_transportation/data/reliever_airport_emissions.rds")
saveRDS(airport_emissions_meta, "./_transportation/data/reliever_airport_emissions_meta.rds")
