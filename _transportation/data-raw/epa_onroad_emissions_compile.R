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
epa_nei_onroad <- readRDS("_transportation/data-raw/epa/epa_nei_onroad_emissions.RDS") %>% 
  filter(cprg_area == TRUE) %>% 
  left_join(scc_complete_road %>% 
              select(scc, scc6) %>% 
              unique())

epa_equates <- readRDS("_transportation/data-raw/epa/air_emissions_modeling/EQUATES/equates_cprg.RDS") %>% 
  filter(cprg_area == TRUE,
         emis_type == "RPD")

epa_emismod <- read_rds("_transportation/data-raw/epa/air_emissions_modeling/onroad_mn_wi.RDS") %>% 
  mutate(geoid = region_cd) %>%
  left_join(counties_light) %>%
  filter(cprg_area == TRUE,
         emis_type == "RPD") %>% 
  left_join(scc_combine)

# start summarizing datasets -----
# aggregate each dataset up to scc6, pollutant_code
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
    emissions_metric_tons_co2e = co2_co2_equivalent / 1000000,
    co2_co2_n2o_equivalent =
      sum(co2, (ch4 * gwp$ch4), (n2o * gwp$n2o), na.rm = T),
    emissions_metric_tons_co2e_n2o = co2_co2_n2o_equivalent / 1000000
  ) %>%
  select(nei_inventory_year, geoid, county_name, 
         emissions_metric_tons_co2e, emissions_metric_tons_co2e_n2o,
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
    emissions_metric_tons_co2e = co2_co2_equivalent / 1000000,
    
    co2_co2_n2o_equivalent =
      sum(co2, (ch4 * gwp$ch4), (n2o * gwp$n2o), na.rm = T),
    emissions_metric_tons_co2e_n2o = co2_co2_n2o_equivalent / 1000000
  ) %>%
  select(calc_year, geoid, county_name,
         emissions_metric_tons_co2e, emissions_metric_tons_co2e_n2o,
         everything())

# the difference with and without n2o is at most 3%
# and mostly effects trucks, buses, motor homes, larger vehicles
# which makes sense, because these emit more n2o than passenger 
# cars and smaller vehicles, both because more fuel usage, but 
# also diesel emits more n2o than gasoline vehicles
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
    # so2 = na_kalman(so2, smooth = TRUE, type = "trend"),
    # nh4 = na_kalman(nh4, smooth = TRUE, type = "trend"),
    
    # we got NAs in the data_source column when we ran complete()
    # if it is NA, then it means that row was interpolated!
    interpolation = ifelse(is.na(data_source), "Interpolated",
                           "Original"),
    data_source  = "EQUATES"
  ) %>%
  mutate(across(where(is.numeric), ~ifelse(. < 0, 0, .))) %>% 
  # re-calculate CO2e using the interpolated individual pollutants
  mutate(co2_co2_equivalent_recalc =
           sum(co2, (ch4 * gwp$ch4), na.rm = T),
         emissions_metric_tons_co2e_recalc = co2_co2_equivalent / 1000000) %>% 
  select(1:7, data_source, interpolation, emissions_metric_tons_co2e, 
         everything())

# check
# do the re-calculated total emissions equal (within a given tolerance)
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
    filter(nei_inventory_year == "2020") %>% 
    # add identifying columns
    mutate(data_source = "National Emissions Inventory",
           emissions_year = as.numeric(nei_inventory_year),
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
         co, no, nox, pm10_pri, pm25_pri, so2, nh3) %>%
  unique() %>% 
  left_join(scc_combine) %>% 
  arrange(emissions_year)


epa_emissions_summary <- epa_emissions_combine %>% 
  group_by(emissions_year, geoid, county_name, data_source) %>% 
  summarize(emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e),
            co2 = sum(co2),
            ch4 = sum(ch4),
            n2o = sum(n2o),
            pm10_pri = sum(pm10_pri),
            pm25_pri = sum(pm25_pri),
            co = sum(co),
            no = sum(no),
            nox = sum(nox),
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
            co = sum(co),
            no = sum(no),
            nox = sum(nox),
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
            co = sum(co),
            no = sum(no),
            nox = sum(nox),
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
    legend_title = "Vehicle type"
  )

# create indices of known data available by year -----

pollutant_key <- epa_nei_onroad %>% 
  select(pollutant_code, pollutant_desc) %>% 
  unique() %>% 
  mutate(pollutant = make_clean_names(pollutant_code),
         unit_of_measurement = "grams") %>% 
  bind_rows(
    tibble(
      pollutant = "emissions_metric_tons_co2e",
      pollutant_code = "emissions_metric_tons_co2e",
      pollutant_desc = "Carbon dioxide equivalence, excluding nitrous oxide",
      unit_of_measurement = "metric tons")
  ) %>% 
  bind_rows(
    tibble(
      pollutant = "no",
      pollutant_code = "NO",
      pollutant_desc = "Nitric Oxide",
      unit_of_measurement = "grams")
  ) %>% 
  mutate(
    # fill out HTML formatting for future use
    pollutant_format = case_when(
      pollutant_code == "CH4" ~ "CH<sub>4</sub>",
      pollutant_code == "CO" ~ "CO",
      pollutant_code == "CO2" ~ "CO<sub>2</sub>",
      pollutant_code == "N2O" ~ "N<sub>2</sub>",
      pollutant_code == "NH3" ~ "NH<sub>3</sub>",
      pollutant_code == "SO2" ~ "SO<sub>2</sub>",
      pollutant_code == "NOX" ~ "NOx",
      pollutant_code == "NO" ~ "NO",
      pollutant_code == "PM10-PRI" ~ "PM<sub>2.5</sub>",
      pollutant_code == "PM25-PRI" ~ "PM<sub>10</sub>",
      pollutant_code == "VOC" ~ "VOC",
      pollutant_code == "emissions_metric_tons_co2e" ~ "CO<sub>2</sub>e" 
    ))

emissions_year_scc_index <- epa_emissions_combine %>% 
  select(emissions_year, scc6, scc6_desc, fuel_type, vehicle_type,
         alt_mode, alt_mode_truck) %>% 
  unique()

emissions_year_pollutant_index <- epa_emissions_combine %>% 
  select(emissions_year,data_source, interpolation, co2, ch4, n2o, 
         emissions_metric_tons_co2e, co,
         no, nox, pm10_pri, pm25_pri, nh3, so2) %>%
  pivot_longer(cols = c(co2, ch4, n2o, emissions_metric_tons_co2e, co,
                        no, nox, pm10_pri, pm25_pri, nh3, so2),
               names_to = "pollutant",
               values_drop_na = TRUE) %>% 
  select(-value) %>% 
  unique() %>% 
  left_join(pollutant_key)

# export -----
# pack everything up for future use



epa_onroad_emissions_combine <- epa_emissions_combine %>% 
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
