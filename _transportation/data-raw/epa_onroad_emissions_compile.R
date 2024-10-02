# compile EPA emissions datasets and interpolate between them

source("R/_load_pkgs.R")
source("R/global_warming_potential.R")
source("_meta/data-raw/county_geography.R")
# remove large object
rm(county_geography)

scc_combine <- readRDS("_transportation/data/scc_combine.RDS")
scc_combine_meta <- readRDS("_transportation/data/scc_combine_meta.RDS")

pollutant_key <- readRDS("_transportation/data/pollutant_key.RDS")
pollutant_key_meta <- readRDS("_transportation/data/pollutant_key_meta.RDS")

cprg_county_meta <- read_rds("_meta/data/cprg_county_meta.RDS")

if (Sys.info()["user"][[1]] == "rotenle") {
  cli::cli_alert_warning("All plotly's will launch in Firefox DE")
  options(
    browser = "/usr/bin/open -a 'Firefox Developer Edition'",
    viewer = NULL
  )
}


# CNG school buses, motor homes, and short haul trucks
# have very low data availability
# we will remove them from our datasets
scc6_remove <- c(
  "220352", # single unit short haul
  "220343", # school buses
  "220361", # combination short haul
  "220354" # motor homes
)

# read in base datasets -----
# All these were compiled from SMOKE flat files
# waiting to hear back from CMAS forum on blank emis_type column
# https://forum.cmascenter.org/t/blank-emis-type/5244

# first, NEI inventory from  _transportation/data-raw/epa_nei_smoke_ff.R
epa_nei_onroad <- readRDS("_transportation/data-raw/epa/nei/epa_nei_smoke_ff.RDS") %>%
  mutate(
    geoid = region_cd,
    pollutant_code = poll
  ) %>%
  left_join(counties_light, by = join_by(geoid)) %>%
  filter(
    cprg_area == TRUE,
    emis_type %in% c("RPD", "")
  ) %>%
  left_join(scc_combine, by = join_by(scc6)) %>%
  filter(!scc6 %in% scc6_remove)

# next EQUATES from  _transportation/data-raw/epa_equates_read.R
epa_equates <- readRDS("_transportation/data-raw/epa/air_emissions_modeling/EQUATES/equates_cmas_mn_wi.RDS") %>%
    mutate(geoid = region_cd) %>%
    left_join(counties_light, by = join_by(geoid)) %>%
    filter(
      cprg_area == TRUE,
      emis_type %in% c("RPD", "")
    ) %>%
    left_join(scc_combine, by = join_by(scc6)) %>%
    filter(!scc6 %in% scc6_remove)

# finally air emissions modeling from  _transportation/data-raw/epa_air_emissions_modeling_onroad.R
epa_emismod <- read_rds("_transportation/data-raw/epa/air_emissions_modeling/onroad_mn_wi.RDS") %>%
  mutate(geoid = region_cd) %>%
  left_join(counties_light, by = join_by(geoid)) %>%
  filter(
    cprg_area == TRUE,
    emis_type %in% c("RPD", "")
  ) %>%
  left_join(scc_combine, by = join_by(scc6)) %>%
  filter(!scc6 %in% scc6_remove)

# summarize datasets -----
# aggregate each dataset up to scc6, pollutant_code
epa_nei_onroad_summary <- epa_nei_onroad %>%
  group_by(geoid, county_name, calc_year, poll, scc6, scc6_desc) %>%
  summarize(
    emissions_short_tons = sum(emissions_short_tons),
    .groups = "keep"
  ) %>%
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
      sum(co2, (ch4 * gwp$ch4), (n2o * gwp$n2o), na.rm = TRUE),
    emissions_metric_tons_co2e = co2_co2_equivalent / 1000000,
    emissions_metric_tons_co2e_exclude_n2o =  
      sum(co2, (ch4 * gwp$ch4), na.rm = TRUE) / 1000000
  ) %>%
  select(
    calc_year, geoid, county_name,
    emissions_metric_tons_co2e,
    everything()
  )

epa_emismod_summary <- epa_emismod %>%
  group_by(geoid, county_name, calc_year, poll, scc6, scc6_desc) %>%
  summarize(
    emissions_short_tons = sum(emissions_short_tons),
    .groups = "keep"
  ) %>%
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
      sum(co2, (ch4 * gwp$ch4), (n2o * gwp$n2o), na.rm = TRUE),
    emissions_metric_tons_co2e = co2_co2_equivalent / 1000000,
    emissions_metric_tons_co2e_exclude_n2o =  
      sum(co2, (ch4 * gwp$ch4), na.rm = TRUE) / 1000000
  ) %>%
  select(
    calc_year, geoid, county_name,
    emissions_metric_tons_co2e,
    everything()
  )

# compile EQUATES data
# note that we don't have n2o from EQUATES, except years 2018-2019
# https://forum.cmascenter.org/t/nitrous-oxide-n2o-availability-in-equates-county-level/5199
epa_equates_summary <- epa_equates %>%
  group_by(geoid, county_name, calc_year, poll, scc6) %>%
  summarize(
    emissions_short_tons = sum(emissions_short_tons),
    .groups = "keep"
  ) %>%
  # convert to grams
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
      sum(co2, (ch4 * gwp$ch4), (n2o * gwp$n2o), na.rm = TRUE),
    emissions_metric_tons_co2e = co2_co2_equivalent / 1000000,
    emissions_metric_tons_co2e_exclude_n2o =  
      sum(co2, (ch4 * gwp$ch4), na.rm = TRUE) / 1000000
  ) %>%
  select(
    calc_year, geoid, county_name,
    emissions_metric_tons_co2e, everything()
  ) %>%
  left_join(scc_combine, by = "scc6")


# combine specific years to get a full time series -----
epa_emissions_combine <- bind_rows(
  epa_nei_onroad_summary %>%
    # only use NEI for 2020
    filter(calc_year == "2020") %>%
    # add identifying columns
    mutate(
      data_source = "National Emissions Inventory",
      emissions_year = as.numeric(calc_year),
      interpolation = "Original"
    ),
  # get air emissions model for 2021 and 2022
  epa_emismod_summary %>%
    filter(calc_year %in% c("2021", "2022")) %>%
    mutate(
      data_source = "Air Emissions Modeling",
      emissions_year = as.numeric(calc_year),
      interpolation = "Original"
    ),
  # use EQUATES for all other years
  epa_equates_summary %>% 
    mutate(data_source = "EQUATES",
      emissions_year = as.numeric(calc_year),
           interpolation = "Original")
) %>%
  ungroup() %>%
  select(
    emissions_year, data_source, interpolation,
    geoid, county_name, scc6,
    co2, n2o, ch4, emissions_metric_tons_co2e,
    co, no, nox, pm10_pri, pm25_pri, so2, nh3,
    voc
  ) %>%
  unique() %>%
  left_join(scc_combine, by = "scc6") %>%
  arrange(emissions_year)

# summarize combined datasets -----
epa_emissions_summary <- epa_emissions_combine %>%
  group_by(emissions_year, geoid, county_name, data_source) %>%
  summarize(
    emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e),
    co2 = sum(co2),
    ch4 = sum(ch4),
    n2o = sum(n2o),
    pm10_pri = sum(pm10_pri),
    pm25_pri = sum(pm25_pri),
    so2 = sum(so2),
    co = sum(co),
    no = sum(no),
    nox = sum(nox),
    voc = sum(voc),
    .groups = "keep"
  )

# summarize by vehicle type, county, data_source
epa_emissions_summary_vehicle <- epa_emissions_combine %>%
  group_by(emissions_year, geoid, county_name, data_source, vehicle_type) %>%
  summarize(
    emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e),
    co2 = sum(co2),
    ch4 = sum(ch4),
    n2o = sum(n2o),
    pm10_pri = sum(pm10_pri),
    pm25_pri = sum(pm25_pri),
    so2 = sum(so2),
    co = sum(co),
    no = sum(no),
    nox = sum(nox),
    voc = sum(voc),
    .groups = "keep"
  )

# summarize by county, alternate vehicle types/modes
epa_emissions_summary_alt_mode_truck <- epa_emissions_combine %>%
  group_by(emissions_year, geoid, county_name, data_source, alt_mode_truck) %>%
  summarize(
    emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e),
    co2 = sum(co2),
    ch4 = sum(ch4),
    n2o = sum(n2o),
    pm10_pri = sum(pm10_pri),
    pm25_pri = sum(pm25_pri),
    so2 = sum(so2),
    co = sum(co),
    no = sum(no),
    nox = sum(nox),
    voc = sum(voc),
    .groups = "keep"
  )

# basic plots ----
epa_emissions_summary %>%
  group_by(county_name) %>%
  plot_ly(
    x = ~emissions_year,
    y = ~emissions_metric_tons_co2e,
    color = ~county_name,
    legendgroup = ~county_name
  ) %>%
  add_markers(
    marker = list(size = 10),
    legendgroup = ~county_name
    
  ) %>%
  add_lines(
    marker = list(
      size = 3
    ),
    legendgroup = ~county_name,
    showlegend = FALSE
  ) %>%
  plotly_layout(
    main_title = "County transportation emissions, all EPA sources",
    legend_title = "County",
    x_title = "Year",
    y_title = "Metric tons CO<sub>2</sub>e"
  )


epa_emissions_summary_alt_mode_truck %>%
  filter(county_name == "Ramsey") %>%
  group_by(county_name, alt_mode_truck) %>%
  plot_ly(
    x = ~emissions_year,
    y = ~emissions_metric_tons_co2e,
    color = ~alt_mode_truck
  ) %>%
  add_markers(
    marker = list(
      size = 10,
      opacity = 0.7
    ),
    legendgroup = ~alt_mode_truck
  ) %>%
  add_lines(
    marker = list(
      size = 3,
      opacity = 0.7
    ),
    legendgroup = ~alt_mode_truck,
    showlegend = FALSE
  ) %>%
  plotly_layout(
    main_title = "Ramsey",
    legend_title = "Vehicle type",
    x_title = "Year",
    y_title = "Metric tons CO<sub>2</sub>e"
  )




epa_emissions_summary_alt_mode_truck %>%
  filter(county_name == "Hennepin") %>%
  group_by(county_name, alt_mode_truck) %>%
  plot_ly(
    x = ~emissions_year,
    y = ~emissions_metric_tons_co2e,
    color = ~alt_mode_truck
  ) %>%
  add_markers(
    marker = list(
      size = 10,
      opacity = 0.7
    ),
    legendgroup = ~alt_mode_truck
  ) %>%
  add_lines(
    marker = list(
      size = 3,
      opacity = 0.7
    ),
    legendgroup = ~alt_mode_truck,
    showlegend = FALSE
  ) %>%
  plotly_layout(
    main_title = "Hennepin",
    legend_title = "Vehicle type",
    x_title = "Year",
    y_title = "Metric tons CO<sub>2</sub>e"
  )


# create indices of known data available by year -----

epa_onroad_emissions_year_scc_index <- epa_emissions_combine %>%
  select(
    data_source,
    scc6, scc6_desc, fuel_type, vehicle_type,
    alt_mode, alt_mode_truck, vehicle_weight_label,
    fuel_type_label
  ) %>%
  unique()

epa_onroad_emissions_year_pollutant_index <- epa_emissions_combine %>%
  select(
    emissions_year, data_source, interpolation,
    co2, ch4, n2o,
    emissions_metric_tons_co2e, co,
    no, nox, pm10_pri, pm25_pri, nh3, so2, voc
  ) %>%
  group_by(
    emissions_year,
    data_source, interpolation
  ) %>%
  pivot_longer(
    cols = c(
      co2, ch4, n2o, emissions_metric_tons_co2e, co,
      no, nox, pm10_pri, pm25_pri, nh3, so2
    ),
    names_to = "pollutant",
    values_drop_na = TRUE
  ) %>%
  select(-value) %>%
  unique() %>%
  left_join(pollutant_key)

# export -----
# pack everything up for future use

epa_onroad_emissions_compile <- epa_emissions_combine %>%
  select(
    emissions_year,
    data_source, interpolation, geoid, county_name,
    scc6, scc6_desc, fuel_type, vehicle_type, alt_mode,
    alt_mode_truck,
    emissions_metric_tons_co2e, co2, ch4, n2o,
    co, no, nox, so2, nh3, pm10_pri, pm25_pri
  ) %>%
  group_by(
    emissions_year,
    data_source, interpolation, geoid, county_name,
    scc6, scc6_desc, fuel_type, vehicle_type, alt_mode,
    alt_mode_truck
  ) %>%
  pivot_longer(
    cols = c(
      emissions_metric_tons_co2e, co2, ch4, n2o,
      co, no, nox, so2, nh3, pm10_pri, pm25_pri
    ),
    names_to = "pollutant",
    values_to = "emissions"
  ) %>%
  unique() %>%
  left_join(pollutant_key) %>%
  ungroup()


names(epa_onroad_emissions_compile)
epa_onroad_emissions_compile_meta <- tibble::tribble(
  ~"Column", ~"Class", ~"Description",
  "emissions_year", class(epa_onroad_emissions_compile$emissions_year), "Emissions year",
  "data_source", class(epa_onroad_emissions_compile$data_source), "EPA data source",
  "interpolation", class(epa_onroad_emissions_compile$interpolation), "Original or interpolated data",
  "emissions", class(epa_onroad_emissions_compile$emissions), "Annual emissions"
) %>%
  bind_rows(cprg_county_meta) %>%
  bind_rows(scc_combine_meta) %>%
  bind_rows(pollutant_key_meta) %>%
  filter(Column %in% names(epa_onroad_emissions_compile))

saveRDS(epa_onroad_emissions_compile, "_transportation/data/epa_onroad_emissions_compile.RDS",
        compress = "xz")
saveRDS(epa_onroad_emissions_compile_meta, "_transportation/data/epa_onroad_emissions_compile_meta.RDS")



# what is the complete start-to-finish pipeline
# for each portion of this dataset?
epa_onroad_source_set <-
  bind_rows(
    epa_nei_onroad %>%
      select(file_location, calc_year, metadata_info) %>%
      mutate(
        data_source = "National Emissions Inventory",
        dataset = "epa_nei_smoke_ff.RDS",
        process_source = "_transportation/data-raw/epa_nei_smoke_ff.R"
      ),
    epa_equates %>%
      select(file_location, calc_year, metadata_info) %>%
      mutate(
        data_source = "EQUATES",
        dataset = "equates_cmas_mn_wi.RDS",
        process_source = "_transportation/data-raw/epa_equates_read.R"
      ),
    epa_emismod %>%
      select(file_location, calc_year, metadata_info) %>%
      mutate(
        data_source = "Air Emissions Modeling",
        dataset = "onroad_mn_wi.RDS",
        process_source = "_transportation/data-raw/epa_air_emissions_modeling_onroad.R"
      )
  ) %>%
  unique() %>%
  mutate(
    # pull MOVES edition information from metadata
    moves_2014 = stringr::str_extract(metadata_info, "MOVES20[:digit:][:digit:][:alpha:]"),
    moves4 = stringr::str_extract(metadata_info, "MOVES[:digit:]"),
    moves3 = stringr::str_extract(file_location, "MOVES[:digit:]"),
    moves_edition =
      case_when(
        is.na(moves_2014) & is.na(moves4) & is.na(moves3) & data_source == "National Emissions Inventory" ~ "MOVES3",
        is.na(moves_2014) & is.na(moves4) ~ moves3,
        is.na(moves_2014) & is.na(moves3) ~ moves4,
        is.na(moves_2014) & moves4 == "MOVES2" ~ moves3,
        is.na(moves_2014) ~ moves4,
        TRUE ~ moves_2014
      )
  ) %>%
  select(-moves3, -moves4, -moves_2014, -metadata_info) %>%
  mutate(
    compiled_to = "epa_onroad_emissions_compile",
    calc_year = as.character(calc_year)
  ) %>%
  unique() %>%
  select(data_source, dataset, moves_edition,
    emissions_year = calc_year,
    process_source,
    file_location
  )

saveRDS(epa_onroad_source_set, "_transportation/data/epa_onroad_source_set.RDS")
