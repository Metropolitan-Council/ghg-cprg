# compile EPA emissions datasets and interpolate between them

source("R/_load_pkgs.R")
source("R/global_warming_potential.R")
source("_transportation/data-raw/epa_source_classification_codes.R")
source("_meta/data-raw/county_geography.R")
library(imputeTS)

if(Sys.info()["user"][[1]] == "rotenle"){
  cli::cli_alert_warning("All plotly plots will launch in Firefox DE")
  options(browser = "/usr/bin/open -a 'Firefox Developer Edition'",
          viewer = NULL)
}

# create a combined SCC index for use across all data sources
scc_combine <- scc_complete_road %>% 
  select(scc6, scc6_desc) %>% 
  unique() %>% 
  bind_rows(scc_equates %>% 
              select(scc6, scc6_desc) %>% 
              unique()) %>% 
  unique() %>% 
  filter(!is.na(scc6_desc)) %>% 
  mutate(
    fuel_type = stringr::str_split(
      scc6_desc,
      pattern = ";",
      simplify = TRUE
    )[, 1] %>% 
      str_trim(),
    vehicle_type = stringr::str_split(
      scc6_desc,
      pattern = ";",
      simplify = TRUE
    )[, 2] %>% 
      str_trim()
  ) %>% 
  left_join(
    scc_complete_road %>% 
      select(scc6_desc,
             alt_mode, alt_mode_truck) %>% 
      unique()
  )


# read in base datasets -----
epa_nei_onroad <- readRDS("_transportation/data-raw/epa/nei_onroad_emissions.RDS") %>% 
  filter(cprg_area == TRUE) %>% 
  left_join(scc_complete_road %>% 
              select(scc, scc6) %>% 
              unique())

epa_equates <- readRDS("_transportation/data-raw/epa/air_emissions_modeling/EQUATES/equates_cprg.RDS") %>% 
  filter(cprg_area == TRUE,
         emis_type == "RPD")

epa_emismod <- 
  bind_rows(readRDS("_transportation/data-raw/epa/air_emissions_modeling/onroad_mn_wi_19_22.RDS"),
            readRDS("_transportation/data-raw/epa/air_emissions_modeling/onroad_mn_wi_15_18.RDS"),
            readRDS("_transportation/data-raw/epa/air_emissions_modeling/onroad_mn_wi_11_14.RDS")) %>% 
  mutate(geoid = region_cd) %>%
  left_join(counties_light) %>%
  filter(cprg_area == TRUE,
         emis_type == "RPD") %>% 
  left_join(scc_combine)

# start summarizing datasets -----
epa_nei_onroad_summary <- epa_nei_onroad %>% 
  group_by(geoid, county_name, nei_inventory_year,
           pollutant_code, scc6) %>%
  summarize(emissions_tons = sum(total_emissions),
            .groups = "keep") %>%
  mutate(ann_value_grams = emissions_tons %>%
           units::as_units("ton") %>%
           units::set_units("gram") %>%
           as.numeric()) %>%
  select(-emissions_tons) %>%
  pivot_wider(
    names_from = pollutant_code,
    values_from = ann_value_grams
  ) %>%
  clean_names() %>%
  rowwise() %>%
  mutate(
    co2_co2_equivalent =
      sum(co2, (ch4 * gwp$ch4), na.rm = T),
    emissions_metric_tons_co2e = co2_co2_equivalent / 1000000
  ) %>%
  select(nei_inventory_year, geoid, county_name, 
         co2, emissions_metric_tons_co2e,
         everything()) %>% 
  left_join(scc_combine)


epa_emismod_summary <- epa_emismod %>% 
  select(-equates_path) %>% 
  group_by(geoid, county_name, calc_year, poll, scc6,
           scc6_desc) %>%
  summarize(emissions_short_tons = sum(emissions_short_tons),
            .groups = "keep") %>%
  mutate(ann_value_grams = emissions_short_tons %>%
           units::as_units("short_ton") %>%
           units::set_units("gram") %>%
           as.numeric()) %>%
  select(-emissions_short_tons) %>%
  pivot_wider(
    names_from = poll,
    values_from = ann_value_grams
  ) %>%
  clean_names() %>%
  rowwise() %>%
  mutate(
    co2_co2_equivalent =
      sum(co2, (ch4 * gwp$ch4), na.rm = T),
    emissions_metric_tons_co2e = co2_co2_equivalent / 1000000
  ) %>%
  select(calc_year, geoid, county_name, co2,
         emissions_metric_tons_co2e, everything())


# compile EQUATES data  
epa_equates_summary <- epa_equates %>% 
  select(-equates_path) %>% 
  group_by(geoid, county_name, calc_year, poll, scc6) %>%
  summarize(emissions_short_tons = sum(emissions_short_tons),
            .groups = "keep") %>%
  mutate(ann_value_grams = emissions_short_tons %>%
           units::as_units("short_ton") %>%
           units::set_units("gram") %>%
           as.numeric()) %>%
  select(-emissions_short_tons) %>%
  pivot_wider(
    names_from = poll,
    values_from = ann_value_grams
  ) %>%
  clean_names() %>%
  rowwise() %>%
  mutate(
    co2_co2_equivalent =
      sum(co2, (ch4 * gwp$ch4), na.rm = T),
    emissions_metric_tons_co2e = co2_co2_equivalent / 1000000
  ) %>%
  select(calc_year, geoid, county_name,
         emissions_metric_tons_co2e, everything()) %>% 
  left_join(scc_combine)

epa_equates_summary_interp <- epa_equates_summary %>% 
  # some of the vehicle/fuel type combinations only have PM2.5 and PM10
  # not any other pollutant types
  # remove these from the dataset
  filter(!is.na(emissions_metric_tons_co2e),
         !is.na(co2),
         ! scc6 %in% c("220352",
                       "220343",
                       "220361",
                       "220354")
  ) %>% 
  mutate(data_source = "EQUATES") %>% 
  group_by(
    geoid, county_name, 
    scc6,
    scc6_desc,
    fuel_type,
    vehicle_type
  ) %>%
  mutate(emissions_year = as.numeric(calc_year)) %>% 
  complete(emissions_year = 2002:2019) %>% 
  mutate(
    emissions_metric_tons_co2e = na_kalman(emissions_metric_tons_co2e, smooth = TRUE, type = "trend"),
    co2 = na_kalman(co2, smooth = TRUE, type = "trend"),
    ch4 = na_kalman(ch4, smooth = TRUE, type = "trend"),
    co2_co2_equivalent = na_kalman(co2_co2_equivalent, smooth = TRUE, type = "trend"),
    # n2o = na_kalman(n2o, smooth = TRUE, type = "trend"),
    co = na_kalman(co, smooth = TRUE, type = "trend"),
    no = na_kalman(no, smooth = TRUE, type = "trend"),
    nox = na_kalman(nox, smooth = TRUE, type = "trend"),
    pm10_pri = na_kalman(pm10_pri, smooth = TRUE, type = "trend"),
    pm25_pri = na_kalman(pm25_pri, smooth = TRUE, type = "trend"),
    voc = na_kalman(voc, smooth = TRUE, type = "trend"),
    interpolation = ifelse(is.na(data_source), "Interpolated",
                           "Original"),
    data_source  = "EQUATES"
  ) %>%
  mutate(across(where(is.numeric), ~ifelse(. < 0, 0, .))) %>% 
  mutate(co2_co2_equivalent_recalc =
           sum(co2, (ch4 * gwp$ch4), na.rm = T),
         emissions_metric_tons_co2e_recalc = co2_co2_equivalent / 1000000) %>% 
  select(1:7, data_source, interpolation, emissions_metric_tons_co2e, everything())

# check
# do the re-calculated total emissions equal (within a given tolerance)
# equal the interpolated total emissions?
epa_equates_summary_interp %>% 
  select(geoid, county_name, emissions_year, 
         interpolation, scc6, emissions_metric_tons_co2e, emissions_metric_tons_co2e_recalc) %>% 
  unique() %>% 
  mutate(value_compare = equals(round(emissions_metric_tons_co2e, digits = 0), 
                                round(emissions_metric_tons_co2e_recalc, digits = 0))) %>% 
  filter(value_compare == FALSE)


# combine specific years to get a full time series -----
epa_emissions_combine <- bind_rows(
  epa_nei_onroad_summary %>% 
    # only fetch NEI for 2020
    filter(nei_inventory_year == "2020") %>% 
    mutate(data_source = "National Emissions Inventory",
           emissions_year = as.numeric(nei_inventory_year),
           interpolation = "Original"),
  epa_emismod_summary %>% 
    # get air emissions modelingn for 2021 and 2022
    filter(calc_year %in% c("2021", "2022")) %>% 
    mutate(data_source = "Air Emissions Modeling",
           emissions_year = as.numeric(calc_year),
           interpolation = "Original"),
  # use EQUATES for all other years
  epa_equates_summary_interp) %>% 
  ungroup() %>% 
  select(emissions_year, data_source, interpolation, geoid, county_name, scc6, 
         co2, n2o, ch4, emissions_metric_tons_co2e, 
         co, no, nox, pm10_pri, pm25_pri) %>%
  unique() %>% 
  left_join(scc_combine) %>% 
  arrange(emissions_year)


epa_emissions_summary <- epa_emissions_combine %>% 
  group_by(emissions_year, geoid, county_name, data_source) %>% 
  summarize(emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e),
            # co2 = sum(co2),
            # ch4 = sum(ch4),
            # n2o = sum(n2o),
            # pm10_pri = sum(pm10_pri),
            # pm25_pri = sum(pm25_pri),
            # co = sum(co),
            # no = sum(no),
            # nox = sum(nox),
            .groups = "keep")


epa_emissions_summary_vehicle <- epa_emissions_combine %>% 
  group_by(emissions_year, geoid, county_name, data_source, vehicle_type, alt_mode_truck) %>% 
  summarize(emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e),
            # co2 = sum(co2),
            # ch4 = sum(ch4),
            # n2o = sum(n2o),
            # pm10_pri = sum(pm10_pri),
            # pm25_pri = sum(pm25_pri),
            # co = sum(co),
            # no = sum(no),
            # nox = sum(nox),
            .groups = "keep")


epa_emissions_summary_alt_mode_truck <- epa_emissions_combine %>% 
  group_by(emissions_year, geoid, county_name, data_source, alt_mode_truck) %>% 
  summarize(emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e),
            # co2 = sum(co2),
            # ch4 = sum(ch4),
            # n2o = sum(n2o),
            # pm10_pri = sum(pm10_pri),
            # pm25_pri = sum(pm25_pri),
            # co = sum(co),
            # no = sum(no),
            # nox = sum(nox),
            .groups = "keep")

# basic plots ---- 

epa_emissions_summary %>% 
  group_by(county_name) %>% 
  plot_ly(
    x = ~emissions_year,
    y = ~emissions_metric_tons_co2e,
    color = ~county_name
  ) %>% 
  add_markers(
    marker = list(size = 10)
  ) %>% 
  add_lines(
    marker = list(
      size = 3
    )
  ) %>% 
  plotly_layout()


epa_emissions_summary_alt_mode_truck %>% 
  filter(county_name == "Hennepin") %>% 
  group_by(alt_mode_truck) %>% 
  plot_ly(
    x = ~emissions_year,
    y = ~emissions_metric_tons_co2e,
    color = ~alt_mode_truck
  ) %>% 
  add_markers(
    marker = list(size = 10,
                  opacity = 0.7)
  ) %>% 
  add_lines(
    showlegend = FALSE,
    marker = list(
      size = 3,
      opacity = 0.7
    )) %>% 
  plotly_layout(
    main_title = "Hennepin County",
    subtitle = "Vehicle type"
  )

# create indeces of known data available by year -----
emissions_year_scc_index <- epa_emissions_combine %>% 
  select(emissions_year, scc6, scc6_desc, fuel_type, vehicle_type,
         alt_mode, alt_mode_truck) %>% 
  unique()

emissions_year_pollutant_index <- epa_emissions_combine %>% 
  select(emissions_year,data_source, interpolation, co2, ch4, n2o, emissions_metric_tons_co2e, co,
         no, nox, pm10_pri, pm25_pri) %>%
  pivot_longer(cols = c(co2, ch4, n2o, emissions_metric_tons_co2e, co,
                        no, nox, pm10_pri, pm25_pri),
               values_drop_na = TRUE) %>% 
  select(-value) %>% 
  unique()


