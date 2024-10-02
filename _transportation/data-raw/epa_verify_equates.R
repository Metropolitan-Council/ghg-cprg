# Verify EQUATES data downloaded from different sources
# CMAS comes from the CMAS Warehouse Google Drive
# EQUATES comes from EPA site
# Also, how close were our interpolations?

source("R/_load_pkgs.R")
source("R/global_warming_potential.R")
source("_meta/data-raw/county_geography.R")

scc_combine <- readRDS("_transportation/data/scc_combine.RDS")
scc_combine_meta <- readRDS("_transportation/data/scc_combine_meta.RDS")

pollutant_key <- readRDS("_transportation/data/pollutant_key.RDS")
pollutant_key_meta <- readRDS("_transportation/data/pollutant_key_meta.RDS")

cprg_county_meta <- read_rds("_meta/data/cprg_county_meta.RDS")


scc6_remove <- c(
  "220352", # single unit short haul
  "220343", # school buses
  "220361", # combination short haul
  "220354" # motor homes
)


epa_equates <- readRDS("_transportation/data-raw/epa/air_emissions_modeling/EQUATES/equates_mn_wi.RDS") %>%
  mutate(geoid = region_cd) %>%
  left_join(counties_light, by = join_by(geoid)) %>%
  filter(
    cprg_area == TRUE,
    emis_type %in% c("RPD", "")
  ) %>%
  left_join(scc_combine, by = join_by(scc6)) %>%
  filter(!scc6 %in% scc6_remove) %>%
  bind_rows()

equates_cmas <- readRDS("_transportation/data-raw/epa/air_emissions_modeling/EQUATES/equates_cmas_mn_wi.RDS") %>%
  mutate(geoid = region_cd) %>%
  left_join(counties_light, by = join_by(geoid)) %>%
  filter(
    cprg_area == TRUE,
    emis_type %in% c("RPD", "")
  ) %>%
  left_join(scc_combine) %>%
  filter(!scc6 %in% scc6_remove)



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
      sum(co2, (ch4 * gwp$ch4), na.rm = T),
    emissions_metric_tons_co2e = co2_co2_equivalent / 1000000
  ) %>%
  select(
    calc_year, geoid, county_name,
    emissions_metric_tons_co2e, everything()
  ) %>%
  left_join(scc_combine)



equates_cmas_summary <- equates_cmas %>%
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
      sum(co2, (ch4 * gwp$ch4), na.rm = T),
    emissions_metric_tons_co2e = co2_co2_equivalent / 1000000
  ) %>%
  select(
    calc_year, geoid, county_name,
    emissions_metric_tons_co2e, everything()
  ) %>%
  left_join(scc_combine) %>%
  mutate(data_source = "EQUATES") %>%
  ungroup() %>%
  mutate(emissions_year = as.numeric(calc_year)) %>%
  mutate(download_source = "CMAS")



equates_cmas_summary_vehicle <- equates_cmas_summary %>%
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
  ) %>%
  mutate(download_source = "CMAS")


# interpolate intermediary years
epa_equates_summary_interp <- epa_equates_summary %>%
  filter(
    # some of the vehicle/fuel type combinations only have PM2.5, PM10, and VOC
    # not any other pollutant types
    # remove these from the dataset
    !is.na(emissions_metric_tons_co2e),
    !is.na(co2)
  ) %>%
  mutate(data_source = "EQUATES") %>%
  group_by(
    geoid, county_name,
    scc6,
    scc6_desc,
    fuel_type,
    vehicle_type
  ) %>%
  # convert character to numeric
  mutate(emissions_year = as.numeric(calc_year)) %>%
  # complete the time series by filling in missing years with NA values
  complete(emissions_year = 2002:2019) %>%
  mutate(
    # use Kalman interpolation for all pollutants
    emissions_metric_tons_co2e = na_kalman(emissions_metric_tons_co2e,
      smooth = TRUE,
      type = "trend"
    ),
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

    # so2 = na_kalman(so2, smooth = TRUE, type = "trend"),
    # nh4 = na_kalman(nh4, smooth = TRUE, type = "trend"),

    # we got NAs in the data_source column when we ran complete()
    # if it is NA, then it means that row was interpolated!
    interpolation = ifelse(is.na(data_source), "Interpolated",
      "Original"
    ),
    data_source = "EQUATES"
  ) %>%
  # replace negative values with 0
  mutate(across(where(is.numeric), ~ ifelse(. < 0, 0, .))) %>%
  # re-calculate CO2e using the interpolated individual pollutants
  mutate(
    co2_co2_equivalent_recalc =
      sum(co2, (ch4 * gwp$ch4), na.rm = T),
    emissions_metric_tons_co2e_recalc = co2_co2_equivalent / 1000000
  ) %>%
  select(
    1:7, data_source, interpolation, emissions_metric_tons_co2e,
    everything()
  ) %>%
  mutate(download_source = "EPA")


epa_equates_summary_vehicle <- epa_equates_summary_interp %>%
  group_by(
    emissions_year, geoid, county_name,
    data_source, interpolation, vehicle_type
  ) %>%
  summarize(
    emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e),
    co2 = sum(co2),
    ch4 = sum(ch4),
    # n2o = sum(n2o),
    pm10_pri = sum(pm10_pri),
    pm25_pri = sum(pm25_pri),
    so2 = sum(so2),
    co = sum(co),
    no = sum(no),
    nox = sum(nox),
    voc = sum(voc),
    .groups = "keep"
  ) %>%
  mutate(download_source = "EPA")

# combine and compare -----

equates_diff <- equates_cmas_summary %>%
  select(
    emissions_year, geoid, county_name, data_source, scc6_desc, scc6, fuel_type, vehicle_type,
    co2,
    emissions_metric_tons_co2e
  ) %>%
  full_join(
    epa_equates_summary_interp %>%
      select(
        emissions_year, geoid, county_name, data_source, scc6_desc,
        scc6, fuel_type, vehicle_type,
        interpolation,
        co2,
        emissions_metric_tons_co2e
      ),
    by = join_by(
      emissions_year, geoid, county_name, data_source,
      scc6_desc,
      scc6, fuel_type, vehicle_type
    ),
    suffix = c(".cmas", ".metc")
  ) %>%
  mutate(diff = (emissions_metric_tons_co2e.cmas - emissions_metric_tons_co2e.metc))


veh_type_comp <- bind_rows(
  equates_cmas_summary_vehicle,
  epa_equates_summary_vehicle
) %>%
  unique() %>%
  group_by(
    emissions_year, geoid, county_name,
    data_source, download_source,
    vehicle_type
  ) %>%
  summarize(
    emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e, na.rm = T),
    .groups = "keep"
  ) %>%
  pivot_wider(
    names_from = download_source,
    values_from = emissions_metric_tons_co2e,
    values_fill = 0
  ) %>%
  mutate(diff = EPA - CMAS)

# interestingly, the biggest difference in
# interpolation vs actual is in combination-long haul trucks,
# followed by combination short-haul.
# Also big variation in passenger trucks, passenger cars
veh_type_comp %>%
  filter(
    diff != 0,
    county_name == "Ramsey"
  ) %>%
  plot_ly(
    x = ~diff,
    y = ~vehicle_type,
    color = ~vehicle_type,
    type = "box",
    boxpoints = "all",
    jitter = 0.3,
    opacity = 0.7,
    hoverinfo = "text",
    hovertext = ~ paste0(
      county_name, " County ", emissions_year, "<br>",
      "Difference = ", scales::comma(round(diff))
    )
  )


bind_rows(
  equates_cmas_summary_vehicle,
  epa_equates_summary_vehicle
) %>%
  filter(county_name == "Hennepin") %>%
  View()
group_by(county_name, vehicle_type, download_source) %>%
  plot_ly(
    x = ~emissions_year,
    y = ~emissions_metric_tons_co2e,
    color = ~vehicle_type,
    symbol = ~download_source
  ) %>%
  add_markers(
    marker = list(
      size = 10,
      opacity = 0.7
    )
  ) %>%
  add_lines(
    marker = list(
      size = 3,
      opacity = 0.7
    )
  ) %>%
  plotly_layout(
    main_title = "Hennepin",
    legend_title = "Vehicle type"
  )





# verify identical for FTP and Google Drive -----
purrr::map2(
  .x = c(
    # diesel
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/cmas_data_warehouse/diesel_MYR_2002_SMOKE_MOVES_MOVES3_AQstyle_06jan2021_v0.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/cmas_data_warehouse/diesel_MYR_2005_SMOKE_MOVES_MOVES3_AQstyle_13jan2021_v0.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/cmas_data_warehouse/diesel_MYR_2008_SMOKE_MOVES_MOVES3_AQstyle_30oct2020_v0.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/cmas_data_warehouse/diesel_MYR_2011_SMOKE_MOVES_MOVES3_AQstyle_20apr2021_nf_v1.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/cmas_data_warehouse/diesel_MYR_2014_SMOKE_MOVES_MOVES3_AQstyle_23nov2020_v0.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/cmas_data_warehouse/diesel_MYR_2017_SMOKE_MOVES_MOVES3_AQstyle_15dec2020_v0.RDS",

    # gasoline
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/cmas_data_warehouse/gas_MYR_2002_SMOKE_MOVES_MOVES3_AQstyle_28may2021_nf_v1.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/cmas_data_warehouse/gas_MYR_2005_SMOKE_MOVES_MOVES3_AQstyle_13jan2021_v0.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/cmas_data_warehouse/gas_MYR_2008_SMOKE_MOVES_MOVES3_AQstyle_30oct2020_v0.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/cmas_data_warehouse/gas_MYR_2011_SMOKE_MOVES_MOVES3_AQstyle_20apr2021_nf_v1.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/cmas_data_warehouse/gas_MYR_2014_SMOKE_MOVES_MOVES3_AQstyle_23nov2020_v0.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/cmas_data_warehouse/gas_MYR_2017_SMOKE_MOVES_MOVES3_AQstyle_15dec2020_v0.RDS"
  ),
  .y = c(
    # diesel
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/diesel_MYR_2002_SMOKE_MOVES_MOVES3_AQstyle_06jan2021_v0.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/diesel_MYR_2005_SMOKE_MOVES_MOVES3_AQstyle_13jan2021_v0.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/diesel_MYR_2008_SMOKE_MOVES_MOVES3_AQstyle_30oct2020_v0.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/diesel_MYR_2011_SMOKE_MOVES_MOVES3_AQstyle_20apr2021_nf_v1.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/diesel_MYR_2014_SMOKE_MOVES_MOVES3_AQstyle_23nov2020_v0.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/diesel_MYR_2017_SMOKE_MOVES_MOVES3_AQstyle_15dec2020_v0.RDS",

    # gas
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/gas_MYR_2002_SMOKE_MOVES_MOVES3_AQstyle_28may2021_nf_v1.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/gas_MYR_2005_SMOKE_MOVES_MOVES3_AQstyle_13jan2021_v0.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/gas_MYR_2008_SMOKE_MOVES_MOVES3_AQstyle_30oct2020_v0.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/gas_MYR_2011_SMOKE_MOVES_MOVES3_AQstyle_20apr2021_nf_v1.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/gas_MYR_2014_SMOKE_MOVES_MOVES3_AQstyle_23nov2020_v0.RDS",
    "_transportation/data-raw/epa/air_emissions_modeling/EQUATES/EQUATES_MN_WI/gas_MYR_2017_SMOKE_MOVES_MOVES3_AQstyle_15dec2020_v0.RDS"
  ),
  function(file1, file2) {
    waldo::compare(
      readRDS(file1) %>%
        select(-file_location),
      readRDS(file2) %>%
        select(-file_location)
    )
  }
)
