# compile EPA emissions datasets and interpolate between them

source("R/_load_pkgs.R")
source("R/global_warming_potential.R")
source("_meta/data-raw/county_geography.R")
library(imputeTS)

scc_combine <- readRDS("_transportation/data/scc_combine.RDS")
scc_combine_meta <- readRDS("_transportation/data/scc_combine_meta.RDS")

pollutant_key <- readRDS("_transportation/data/pollutant_key.RDS")
pollutant_key_meta <- readRDS("_transportation/data/pollutant_key_meta.RDS")

cprg_county_meta <- read_rds("_meta/data/cprg_county_meta.RDS")

if(Sys.info()["user"][[1]] == "rotenle"){
  cli::cli_alert_warning("All plotly's will launch in Firefox DE")
  options(browser = "/usr/bin/open -a 'Firefox Developer Edition'",
          viewer = NULL)
}

# read in base datasets -----
# first, NEI inventory from 
epa_nei_onroad <- readRDS("_transportation/data-raw/epa/nei/epa_nei_smoke_ff.RDS") %>% 
  mutate(geoid = region_cd,
         pollutant_code = poll) %>%
  left_join(counties_light) %>%
  filter(cprg_area == TRUE,
         emis_type %in% c("RPD", "")) %>% 
  left_join(scc_combine)

epa_equates <- readRDS("_transportation/data-raw/epa/air_emissions_modeling/EQUATES/equates_mn_wi.RDS") %>% 
  mutate(geoid = region_cd) %>%
  left_join(counties_light) %>%
  filter(cprg_area == TRUE,
         emis_type %in% c("RPD", "")) %>% 
  left_join(scc_combine)

epa_emismod <- read_rds("_transportation/data-raw/epa/air_emissions_modeling/onroad_mn_wi.RDS") %>% 
  mutate(geoid = region_cd) %>%
  left_join(counties_light) %>%
  filter(cprg_area == TRUE,
         emis_type %in% c("RPD", "")) %>% 
  left_join(scc_combine)

# summarize datasets -----
# aggregate each dataset up to scc6, pollutant_code
epa_nei_onroad_summary <- epa_nei_onroad %>% 
  group_by(geoid, county_name, calc_year, poll, scc6, scc6_desc) %>%
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
    emissions_metric_tons_co2e = co2_co2_equivalent / 1000000,
    
    co2_co2_n2o_equivalent =
      sum(co2, (ch4 * gwp$ch4), (n2o * gwp$n2o), na.rm = T),
    emissions_metric_tons_co2e_n2o = co2_co2_n2o_equivalent / 1000000
  ) %>%
  select(calc_year, geoid, county_name,
         emissions_metric_tons_co2e, emissions_metric_tons_co2e_n2o,
         everything())

epa_emismod_summary <- epa_emismod %>% 
  group_by(geoid, county_name, calc_year, poll, scc6, scc6_desc) %>%
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
    emissions_metric_tons_co2e = co2_co2_equivalent / 1000000,
    
    co2_co2_n2o_equivalent =
      sum(co2, (ch4 * gwp$ch4), (n2o * gwp$n2o), na.rm = T),
    emissions_metric_tons_co2e_n2o = co2_co2_n2o_equivalent / 1000000
  ) %>%
  select(calc_year, geoid, county_name,
         emissions_metric_tons_co2e, emissions_metric_tons_co2e_n2o,
         everything())

# the difference with and without n2o is at MOST 3%
# and mostly effects trucks, buses, motor homes, larger vehicles
# which makes sense, because these emit more n2o than passenger 
# cars and smaller vehicles, both because more fuel usage, but 
# also because these larger vehicles are more likely to run on diesel
# and diesel emits more n2o than gasoline 
epa_emismod_summary %>% 
  group_by(calc_year, geoid, county_name, scc6_desc) %>% 
  summarize(emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e),
            emissions_metric_tons_co2e_n2o = sum(emissions_metric_tons_co2e_n2o)) %>% 
  mutate(pct_diff = (emissions_metric_tons_co2e_n2o -  emissions_metric_tons_co2e)/emissions_metric_tons_co2e)

# compile EQUATES data  
# note that we don't have n2o from EQUATES
# I'm waiting on a reply on the CMAS forum
# https://forum.cmascenter.org/t/nitrous-oxide-n2o-availability-in-equates-county-level/5199
epa_equates_summary <- epa_equates %>% 
  group_by(geoid, county_name, calc_year, poll, scc6) %>%
  summarize(emissions_short_tons = sum(emissions_short_tons),
            .groups = "keep") %>%
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
  select(calc_year, geoid, county_name,
         emissions_metric_tons_co2e, everything()) %>% 
  left_join(scc_combine)

# interpolate intermediary years 
epa_equates_summary_interp <- epa_equates_summary %>% 
  filter(
    # some of the vehicle/fuel type combinations only have PM2.5, PM10, and VOC
    # not any other pollutant types
    # remove these from the dataset
    !is.na(emissions_metric_tons_co2e),
    !is.na(co2),
    # CNG school buses, motor homes, and short haul trucks 
    # have very low data availability (fewer than 3 observations)
    # and so won't interpolate. 
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
  # convert character to numeric
  mutate(emissions_year = as.numeric(calc_year)) %>% 
  # complete the time series by filling in missing years with NA values
  complete(emissions_year = 2002:2019) %>% 
  mutate(
    # use Kalman interpolation for all pollutants
    emissions_metric_tons_co2e = na_kalman(emissions_metric_tons_co2e, smooth = TRUE, 
                                           type = "trend"),
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
                           "Original"),
    data_source  = "EQUATES"
  ) %>%
  # replace negative values with 0
  mutate(across(where(is.numeric), ~ifelse(. < 0, 0, .))) %>% 
  # re-calculate CO2e using the interpolated individual pollutants
  mutate(co2_co2_equivalent_recalc =
           sum(co2, (ch4 * gwp$ch4), na.rm = T),
         emissions_metric_tons_co2e_recalc = co2_co2_equivalent / 1000000) %>% 
  select(1:7, data_source, interpolation, emissions_metric_tons_co2e, 
         everything())

# check
# do the re-calculated total emissions equal (within an acceptable tolerance)
# equal the interpolated total emissions?
epa_equates_summary_interp %>% 
  select(geoid, county_name, emissions_year, 
         interpolation, scc6, emissions_metric_tons_co2e, 
         emissions_metric_tons_co2e_recalc) %>% 
  unique() %>% 
  mutate(
    value_compare = equals(round(emissions_metric_tons_co2e, digits = 0), 
                           round(emissions_metric_tons_co2e_recalc, digits = 0))) %>% 
  filter(value_compare == FALSE)


# combine specific years to get a full time series -----
epa_emissions_combine <- bind_rows(
  epa_nei_onroad_summary %>% 
    # only use NEI for 2020
    filter(calc_year == "2020") %>% 
    # add identifying columns
    mutate(data_source = "National Emissions Inventory",
           emissions_year = as.numeric(calc_year),
           interpolation = "Original"),
  # get air emissions model for 2021 and 2022
  epa_emismod_summary %>% 
    filter(calc_year %in% c("2021", "2022")) %>% 
    mutate(data_source = "Air Emissions Modeling",
           emissions_year = as.numeric(calc_year),
           interpolation = "Original"),
  # use EQUATES for all other years
  epa_equates_summary_interp) %>% 
  # remove CNG school buses, motor homes, and short haul trucks 
  # from all datasets
  filter(! scc6 %in% c("220352",
                       "220343",
                       "220361",
                       "220354")
  ) %>% 
  ungroup() %>% 
  select(emissions_year, data_source, interpolation, 
         geoid, county_name, scc6, 
         co2, n2o, ch4, emissions_metric_tons_co2e, 
         co, no, nox, pm10_pri, pm25_pri, so2, nh3,
         voc) %>%
  unique() %>% 
  left_join(scc_combine) %>% 
  arrange(emissions_year)

# summarize combined datasets -----
epa_emissions_summary <- epa_emissions_combine %>% 
  group_by(emissions_year, geoid, county_name, data_source) %>% 
  summarize(emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e),
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
            .groups = "keep")

# summarize by vehicle type, county, data_source
epa_emissions_summary_vehicle <- epa_emissions_combine %>% 
  group_by(emissions_year, geoid, county_name, data_source, vehicle_type) %>% 
  summarize(emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e),
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
            .groups = "keep")

# summarize by county, alternate vehicle types/modes
epa_emissions_summary_alt_mode_truck <- epa_emissions_combine %>% 
  group_by(emissions_year, geoid, county_name, data_source, alt_mode_truck) %>% 
  summarize(emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e),
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
  filter(county_name == "Ramsey") %>%
  group_by(county_name, alt_mode_truck) %>%
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
    marker = list(
      size = 3,
      opacity = 0.7
    )) %>% 
  plotly_layout(
    legend_title = "Vehicle type"
  )




epa_emissions_summary_alt_mode_truck %>% 
  filter(county_name == "Hennepin") %>%
  group_by(county_name, alt_mode_truck) %>%
  plot_ly(
    x = ~emissions_year,
    y = ~emissions_metric_tons_co2e,
    color = ~alt_mode_truck
    # symbol = ~data_source
  ) %>% 
  add_markers(
    marker = list(size = 10,
                  opacity = 0.7)
  ) %>% 
  add_lines(
    marker = list(
      size = 3,
      opacity = 0.7
    )) %>% 
  plotly_layout(
    legend_title = "Vehicle type"
  )


# create indices of known data available by year -----

epa_onroad_emissions_year_scc_index <- epa_emissions_combine %>% 
  select(scc6, scc6_desc, fuel_type, vehicle_type,
         alt_mode, alt_mode_truck) %>% 
  unique()

epa_onroad_emissions_year_pollutant_index <- epa_emissions_combine %>% 
  select(emissions_year, data_source, interpolation,
         co2, ch4, n2o, 
         emissions_metric_tons_co2e, co,
         no, nox, pm10_pri, pm25_pri, nh3, so2) %>%
  group_by(emissions_year,
           data_source, interpolation) %>% 
  pivot_longer(cols = c(co2, ch4, n2o, emissions_metric_tons_co2e, co,
                        no, nox, pm10_pri, pm25_pri, nh3, so2),
               names_to = "pollutant",
               values_drop_na = TRUE) %>% 
  select(-value) %>% 
  unique() %>% 
  left_join(pollutant_key)

# export -----
# pack everything up for future use

epa_onroad_emissions_compile <- epa_emissions_combine %>% 
  select(emissions_year,
         data_source, interpolation, geoid, county_name,
         scc6, scc6_desc, fuel_type, vehicle_type, alt_mode, 
         alt_mode_truck, 
         emissions_metric_tons_co2e, co2, ch4, n2o, 
         co, no, nox, so2, nh3, pm10_pri, pm25_pri) %>% 
  group_by(emissions_year,
           data_source, interpolation, geoid, county_name,
           scc6, scc6_desc, fuel_type, vehicle_type, alt_mode, 
           alt_mode_truck) %>% 
  pivot_longer(cols = c(emissions_metric_tons_co2e, co2, ch4, n2o, 
                        co, no, nox, so2, nh3, pm10_pri, pm25_pri),
               names_to = "pollutant",
               values_to = "emissions") %>% 
  left_join(pollutant_key)


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

saveRDS(epa_onroad_emissions_compile, "_transportation/data/epa_onroad_emissions_compile.RDS")
saveRDS(epa_onroad_emissions_compile_meta, "_transportation/data/epa_onroad_emissions_compile_meta.RDS")



epa_onroad_source_set <- bind_rows(
  epa_nei_onroad %>% 
    select(file_location, calc_year) %>%
    mutate(data_source = "National Emissions Inventory",
           dataset = "epa_nei_smoke_ff.RDS",
           process_source = "_transportation/data-raw/epa_nei_smoke_ff.R"),
  epa_equates %>% 
    select(file_location, calc_year) %>% 
    mutate(data_source = "EQUATES",
           dataset = "equates_mn_wi.RDS",
           process_source = "_transportation/data-raw/epa_equates_read.R"),
  epa_emismod %>% 
    select(file_location, calc_year) %>%
    mutate(data_source = "Air Emissions Modeling",
           dataset = "onroad_mn_wi.RDS",
           process_source = "_transportation/data-raw/epa_air_emissions_modeling_onroad.R")
) %>% 
  unique() %>% 
  mutate(compiled_to = "epa_onroad_emissions_compile") %>% 
  select(data_source, dataset , emissions_year = calc_year, 
         process_source, 
         file_location)

saveRDS(epa_onroad_source_set, "_transportation/data-raw/epa/epa_onroad_source_set.RDS")
