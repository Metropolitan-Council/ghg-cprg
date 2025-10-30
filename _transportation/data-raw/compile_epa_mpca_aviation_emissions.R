#### This script processes EPA Air Emissions point data to account for airport emissions
#### other than MSP international

source("R/_load_pkgs.R")
source("R/global_warming_potential.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")

air_emis_point <- readRDS("_transportation/data-raw/epa/air_emissions_modeling/point_mn_wi.RDS")
air_emis_nonroad <- readRDS("_transportation/data-raw/epa/air_emissions_modeling/air_emissions_modeling_mn_wi/nonroad/nonroad_ff10_2022v2_by2022_MOVES_ROC_AE6_07may2025_nf_v3.RDS")
air_emis_airport <- readRDS("_transportation/data-raw/epa/air_emissions_modeling/air_emissions_modeling_mn_wi/point/2022hd_airports_2022_point_top51_adjusted_ATL_data_20250428_27may2025_v0.RDS")
msp_emissions <- readRDS("_transportation/data/aviation_emissions.rds")
scc_combine <- readRDS(file.path(here::here(), "_transportation/data/scc_combine.RDS"))

air_codes <- scc_combine %>%
  filter(
    scc6 %in% c(
      air_emis_airport$scc6 %>% unique(),
      air_emis_nonroad$scc6 %>% unique()
    ),
    vehicle_type != "Pleasure craft"
  )

air_emis_point_cprg <- cprg_county %>%
  st_drop_geometry() %>%
  select(geoid, county_name) %>%
  left_join(
    air_emis_point %>%
      select(
        region_cd,
        facility_id,
        unit_id,
        unit_type_code,
        facility_name,
        scc,
        scc6,
        poll,
        emissions_short_tons,
        calc_year,
        file_location
      ),
    by = c("geoid" = "region_cd")
  ) %>%
  filter(scc6 %in% unique(air_codes$scc6)) %>%
  mutate(
    data_source = "Air Emissions Modeling",
    dataset = "point_mn_wi.RDS",
    process_source = "_transportation/data-raw/epa_air_emissions_modeling_point.R"
  ) %>%
  left_join(scc_combine)


### load in state data to anchor to

mpca_aviation <-
  read_rds("_meta/data/mpca_ghg_inv_2022.RDS") %>%
  filter(Source == "Aviation") %>%
  mutate(inventory_year = as.numeric(inventory_year)) %>%
  group_by(Sector, Source, inventory_year) %>%
  summarise(state_mt_co2e = sum(co2e), .groups = "keep") %>%
  mutate(
    data_source = "MPCA state aviation emission proportional analysis",
    dataset = "mpca_ghg_inv_2022.RDS",
    process_source = "_meta/data-raw/02_compile_mpca_ghg_inventory_2022.R"
  )



airport_co2 <- air_emis_point_cprg %>%
  filter(
    poll == "CO2",
    facility_id != 6151711
  ) %>% # MSP intl
  group_by(county_name, calc_year, poll) %>%
  summarize(tons_co2 = sum(emissions_short_tons), .groups = "keep") %>%
  ungroup() %>%
  mutate(
    mt_co2 = as.numeric(tons_co2 * units::as_units("short_ton") %>%
      units::set_units("metric_ton")),
    county_share = mt_co2 / mpca_aviation$state_mt_co2e[mpca_aviation$inventory_year == 2020]
  ) %>%
  cross_join(mpca_aviation %>%
    ungroup() %>%
    filter(inventory_year >= 2005) %>%
    select(inventory_year, state_mt_co2e)) %>%
  mutate(inferred_co2 = state_mt_co2e * county_share)


reliever_airport_emissions <- airport_co2 %>%
  select(geog_name = county_name, inventory_year, value_emissions = inferred_co2) %>%
  unique() %>%
  left_join(
    air_emis_point_cprg %>%
      mutate(inventory_year = as.numeric(calc_year)) %>%
      select(inventory_year, data_source) %>%
      unique() %>%
      bind_rows(
        mpca_aviation %>%
          ungroup() %>%
          select(inventory_year, data_source) %>%
          filter(inventory_year != 2020) %>%
          unique()
      ),
    relationship = "many-to-many",
    by = "inventory_year"
  ) %>%
  unique() %>%
  mutate(
    sector = "Transportation",
    category = "Aviation",
    source = "Reliever airport",
    units_emissions = "Metric tons CO2e",
    factor_source = "EPA GHG Emission Factor Hub"
  )

reliever_airport_emissions_meta <- tibble::tribble(
  ~"Column", ~"Class", ~"Description",
  "sector", class(reliever_airport_emissions$sector), "Emissions sector",
  "category", class(reliever_airport_emissions$category), "Emissions subsector category",
  "source", class(reliever_airport_emissions$source), "Emissions source",
  "inventory_year", class(reliever_airport_emissions$inventory_year), "Inventory year of emissions",
  "value_emissions", class(reliever_airport_emissions$value_emissions), "Numeric value of emissions",
  "units_emissions", class(reliever_airport_emissions$units_emissions), "Units of emissions",
  "geog_name", class(reliever_airport_emissions$geog_name), "Geographic location",
  "data_source", class(reliever_airport_emissions$data_source), "Source of activity data used to calculate emissions",
  "factor_source", class(reliever_airport_emissions$factor_source), "Source of emission factor for translating activity to emissions"
)

saveRDS(reliever_airport_emissions, "./_transportation/data/reliever_airport_emissions.rds")
saveRDS(reliever_airport_emissions_meta, "./_transportation/data/reliever_airport_emissions_meta.rds")


reliever_source_set <- reliever_airport_emissions %>%
  left_join(
    air_emis_point_cprg %>%
      ungroup() %>%
      mutate(inventory_year = as.numeric(calc_year)) %>%
      select(inventory_year, data_source, dataset, process_source, scc6_desc, file_location) %>%
      unique() %>%
      bind_rows(
        mpca_aviation %>%
          ungroup() %>%
          select(inventory_year, data_source, dataset, process_source) %>%
          unique()
      ),
    relationship = "many-to-many",
    by = join_by(inventory_year, data_source)
  ) %>%
  select(-value_emissions) %>%
  unique() %>%
  group_by(
    inventory_year, data_source, sector, category,
    source, units_emissions, factor_source, dataset, process_source,
    scc6_desc, file_location
  ) %>%
  summarize(geographies = paste0(geog_name, collapse = ", "), .groups = "keep")


saveRDS(reliever_source_set, "./_transportation/data/reliever_source_set.RDS")
