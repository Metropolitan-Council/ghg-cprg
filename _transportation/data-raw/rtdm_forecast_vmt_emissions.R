# using outputs from our regional travel demand model,
# create a VMT forecast for each CTU and county in the region.
# The core work for this data exists in another repository.
# Contact us for more detail.
#
source("R/_load_pkgs.R")
ctu_population_meta <- read_rds("_meta/data/ctu_population_meta.RDS")
cprg_county_meta <- read_rds("_meta/data/cprg_county_meta.RDS")
pollutant_key <- readRDS("_transportation/data/pollutant_key.RDS")
source("R/global_warming_potential.R")
epa_onroad_emissions_compile_meta <- readRDS("_transportation/data/epa_onroad_emissions_compile_meta.RDS")


# annualization factor castigliegoCarbonFreeBoston2019
annualization_factor <- 340

ctu_vmt_forecast <- readRDS("_transportation/data-raw/metc_travel_model/ctu_vmt_forecast.RDS") %>%
  mutate(vmt_year = ifelse(vmt_year == 2025, 2023, as.numeric(vmt_year))) %>%
  mutate(
    annualization_factor = annualization_factor,
    network_vmt_annual = network_vmt * annualization_factor,
    network_truck_annual = network_truck_vmt * annualization_factor,
    network_passenger_annual = network_passenger_vmt * annualization_factor
  )

county_vmt_forecast <- readRDS("_transportation/data-raw/metc_travel_model/county_vmt_forecast.RDS") %>%
  mutate(vmt_year = ifelse(vmt_year == 2025, 2023, as.numeric(vmt_year))) %>%
  mutate(
    annualization_factor = annualization_factor,
    network_vmt_annual = network_vmt * annualization_factor,
    network_truck_annual = network_truck_vmt * annualization_factor,
    network_passenger_annual = network_passenger_vmt * annualization_factor
  )


# create metadata tables
ctu_forecast_meta <- ctu_population_meta %>%
  filter(Column %in% names(ctu_vmt_forecast)) %>%
  bind_rows(
    tribble(
      ~Column, ~Class, ~Description,
      "ctu_name_full", class(ctu_vmt_forecast$ctu_name_full), "CTU name and class",
      "vmt_year", class(ctu_vmt_forecast$vmt_year), "VMT forecast year",
      "network_passenger_vmt", class(ctu_vmt_forecast$network_passenger_vmt), "Passenger vehicle-miles traveled, daily",
      "network_truck_vmt", class(ctu_vmt_forecast$network_truck_vmt), "Truck vehicle-miles traveled, daily",
      "network_vmt", class(ctu_vmt_forecast$network_vmt), "Total vehicle-miles traveled, daily",
      "annualization_factor", class(ctu_vmt_forecast$annualization_factor), "Factor used to expand daily VMT to annual VMT. Source: Carbon Free Boston Study, 2019",
      "network_vmt_annual", class(ctu_vmt_forecast$network_vmt_annual), "Total vehicle-miles traveled, annual",
      "network_passenger_annual", class(ctu_vmt_forecast$network_passenger_annual), "Passenger vehicle-miles traveled, annual",
      "network_truck_annual", class(ctu_vmt_forecast$network_truck_annual), "Truck vehicle-miles traveled, annual",
    )
  ) %>%
  arrange(match(Column, names(ctu_vmt_forecast)))


county_vmt_forecast_meta <- cprg_county_meta %>%
  filter(Column %in% names(county_vmt_forecast)) %>%
  bind_rows(
    ctu_forecast_meta %>%
      filter(Column %in% names(county_vmt_forecast))
  ) %>%
  bind_rows(
    tribble(
      ~Column, ~Class, ~Description,
      "county_id", class(county_vmt_forecast$county_id), "Three digit county identifier",
    )
  ) %>%
  arrange(match(Column, names(county_vmt_forecast)))



saveRDS(ctu_vmt_forecast, "_transportation/data/rtdm_forecast_ctu.RDS")
saveRDS(ctu_forecast_meta, "_transportation/data/rtdm_forecast_ctu_meta.RDS")

saveRDS(county_vmt_forecast, "_transportation/data/rtdm_forecast_county.RDS")
saveRDS(county_vmt_forecast_meta, "_transportation/data/rtdm_forecast_county_meta.RDS")

# county-level emissions from model outputs, fed into MOVES, and then output from MOVES
# Note Wright and Sherburne counties only include the urbanized portions, not the entire county.
# This means that interpolating from EPA emissions would be inappropriate
# so for now, we will remove them until we get better data points
co_emis_for <- readRDS("_transportation/data-raw/metc_travel_model/moves_emissions_forecast.RDS") %>%
  filter(
    !is.na(geoid),
    !county_name %in% c("Wright", "Sherburne"),
    !is.na(county_name)
  ) %>%
  mutate(inventory_year = ifelse(inventory_year == 2025, 2023, as.numeric(inventory_year))) %>%
  filter(scenario != "No_Build_2050") %>%
  select(-state_abb, -cprg_area, -statefp, -state_name)


county_emissions_forecast <- co_emis_for %>%
  # only use our GHGs
  filter(pollutantID %in% c(90, 5, 6)) %>%
  # multiply to go from daily to annual
  mutate(EMISSIONS = EMISSIONS * annualization_factor) %>%
  select(-pollutantID, -pollutantName, -globalWarmingPotential, -pollutantDisplayGroupID, -county_id) %>%
  pivot_wider(
    names_from = NEIPollutantCode,
    values_from = EMISSIONS
  ) %>%
  rowwise() %>%
  clean_names() %>%
  mutate(
    co2_co2_equivalent =
      sum(co2, (ch4 * gwp$ch4), (n2o * gwp$n2o), na.rm = TRUE),
    emissions_metric_tons_co2e = co2_co2_equivalent / 1000000,
    emissions_metric_tons_co2e_exclude_n2o =
      sum(co2, (ch4 * gwp$ch4), na.rm = TRUE) / 1000000
  ) %>%
  select(county_id, county_name, geoid, inventory_year, emissions_metric_tons_co2e) %>%
  ungroup()


county_emissions_forecast_meta <- readRDS("_transportation/data/onroad_emissions_meta.RDS") %>%
  filter(Column %in% names(county_emissions_forecast)) %>%
  bind_rows(
    ctu_population_meta %>%
      filter(Column %in% names(county_emissions_forecast))
  ) %>%
  arrange(match(Column, names(county_emissions_forecast))) %>%
  unique()


saveRDS(county_emissions_forecast, "_transportation/data/rtdm_county_emissions_forecast.RDS")
saveRDS(county_emissions_forecast_meta, "_transportation/data/rtdm_county_emissions_forecast_meta.RDS")


# create collar county estimates
onroad_emissions <- readRDS("_transportation/data/onroad_emissions.RDS")
demographic_forecast_11_county <-readRDS("_meta/data/demographic_forecast_11_county.rds")


onroad_emissions_summary <- onroad_emissions %>%
  unique() %>%
  group_by(county_name, geoid, emissions_year, data_source) %>%
  summarize(
    emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e),
    .groups = "keep"
  )

target_counties <- demographic_forecast_11_county %>% 
  filter(!county_name %in% c("Anoka",
                             "Carver",
                             "Dakota",
                             "Hennepin",
                             "Ramsey",
                             "Scott",
                             "Washington")) %>% 
  select(county_name, geoid) %>% 
  unique()

demographic_forecast_11_county_wide <- demographic_forecast_11_county %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  filter(inventory_year == "2050") 

# find most similar county in core metro for each target county needed
target_county_pairs <- purrr::map_dfr(
  target_counties$county_name,
  function(target_county){
    
    demographic_forecast_11_county_wide %>% 
      ungroup() %>% 
      mutate(
        target_county_name = target_county,
        target_households = demographic_forecast_11_county_wide$total_households[demographic_forecast_11_county_wide$county_name == target_county],
        target_pop = demographic_forecast_11_county_wide$total_pop[demographic_forecast_11_county_wide$county_name == target_county],
        target_jobs = demographic_forecast_11_county_wide$total_jobs[demographic_forecast_11_county_wide$county_name == target_county],
        distance = sqrt((total_pop - target_pop)^2 + (total_jobs - target_jobs)^2 +  (total_households - target_households)^2)
      ) %>%
      filter(county_name != target_county,
             county_name %in% c("Anoka",
                                "Carver",
                                "Dakota",
                                "Hennepin",
                                "Ramsey",
                                "Scott",
                                "Washington")) %>%
      arrange(distance) %>% 
      slice(1) %>% 
      select(geoid, county_name, target_county_name)
  })

# find the expected percent change in each pair county from 2022 to 2050
county_emissions_forecast_change <- county_emissions_forecast %>% 
  arrange(county_name, inventory_year) %>% 
  group_by(county_name) %>% 
  mutate(growth = (emissions_metric_tons_co2e - lag(emissions_metric_tons_co2e))/lag(emissions_metric_tons_co2e)) %>% 
  filter(county_name %in% target_county_pairs$county_name,
         inventory_year == max(inventory_year)) %>% 
  select(-inventory_year, -emissions_metric_tons_co2e)

# generate 2050 emissions value for each target county
collar_county_emissions_change <- onroad_emissions_summary %>% 
  ungroup() %>% 
  select(geoid, county_name, emissions_year, emissions_metric_tons_co2e) %>% 
  # get target county data for most recent year
  filter(geoid %in% target_counties$geoid,
         emissions_year == max(emissions_year)) %>% 
  # join with the pair county change factor
  left_join(county_emissions_forecast_change %>% 
              left_join(target_county_pairs, by = c("county_name", "geoid")) %>% 
              select(-geoid),
            by = c("county_name" = "target_county_name")) %>% 
  select(-county_name.y, -county_id) %>% 
  # multiply the change factor by most recent emissions to get 2050 emissions projection
  mutate(emissions_metric_tons_co2e = emissions_metric_tons_co2e * (1 - abs(growth)),
         emissions_year = 2050) %>% 
  select(-growth)

saveRDS(collar_county_emissions_change, "_transportation/data/rtdm_collar_county_emissions_change.RDS")
