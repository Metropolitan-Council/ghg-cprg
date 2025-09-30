##### read in collar county forecasts for employment, households, population

source("R/_load_pkgs.R")
cprg_county <- read_rds("_meta/data/cprg_county.rds")

### read in taz data

### trunk path
taz_path <- here::here("_meta", "data-raw", "forecast_taz")
### list emp files
taz_emp_list <- list.files(taz_path, pattern = "xlsx")

# function to read in and format files
taz_emp_format <- function(taz_file) {
  files_in_folder <- file.path(taz_path, taz_file) # List files in folder

  taz_data <- read_xlsx(files_in_folder) %>%
    clean_names() %>%
    pivot_longer(
      cols = 4:13, # Adjust column selection as needed
      names_to = "variable"
    ) %>%
    group_by(year, county, variable) %>%
    summarize(value = sum(value), .groups="keep") %>%
    mutate(
      state_id = stringr::str_sub(county, 1, 2),
      county_id = stringr::str_sub(county, 3, 5),
      inventory_year = as.numeric(year)
    ) %>%
    ungroup() %>%
    select(-year)
}

# read in, format, bind
taz_emp <- lapply(taz_emp_list, taz_emp_format) %>%
  bind_rows()

taz_emp %>%
  distinct(inventory_year, county) %>%
  group_by(inventory_year) %>%
  summarise(n_counties = n(), .groups = "keep")

# just keeping total employment for now

taz_emp_use <- taz_emp %>%
  filter(variable == "totemp")

# inventory_year n_counties
# <dbl>      <int>
#   1           2020          7
# 2           2022         19
# 3           2030         19
# 4           2040         19
# 5           2050         19

### 2020 only has seven core counties

### repeat process for socio data

### list socio files - note these are csv format not xlsx
taz_socio_list <- list.files(taz_path, pattern = "csv")

# function to read in and format files
taz_socio_format <- function(taz_file) {
  files_in_folder <- file.path(taz_path, taz_file) # List files in folder

  taz_data <- read_csv(files_in_folder) %>%
    clean_names() %>%
    mutate(total_hh = rowSums(across(contains("hhsize")), na.rm = TRUE)) %>%
    pivot_longer(
      cols = 8:42, # Adjust column selection as needed
      names_to = "variable"
    ) %>%
    group_by(year, state, co_code, variable) %>%
    summarize(value = sum(value), .groups="keep") %>%
    ungroup() %>%
    rename(
      inventory_year = year,
      state_id = state,
      county_id = co_code
    ) %>%
    mutate(state_id = as.character(state_id))
}

# read in, format, bind
taz_socio <- lapply(taz_socio_list, taz_socio_format) %>%
  bind_rows()

# taz_socio %>%
#   distinct(inventory_year, county_id) %>%
#   group_by(inventory_year) %>%
#   summarise(n_counties = n(), .groups = "drop")
# # 2030 and 2040 only have core counties


### pare down to key variables

# taz_socio %>%
#   pull(variable) %>%
#   unique()

taz_socio_use <- taz_socio %>%
  filter(variable %in% c("total_hh", "totpop"))


### bind data and bring in county names

forecast_out <- bind_rows(
  taz_emp_use,
  taz_socio_use
) %>%
  mutate(geoid = paste0(state_id, county_id)) %>%
  right_join(cprg_county %>% st_drop_geometry(), by = join_by(geoid)) %>%
  mutate(variable = recode(variable,
    "totemp" = "total_jobs",
    "total_hh" = "total_households",
    "totpop" = "total_pop"
  ))

### compare to urbansim data

urbansim <- read_rds("_meta/data/urbansim_data.RDS")

us_county <- urbansim %>%
  mutate(county_id = substr(coctu_id_gnis, 1, 3)) %>%
  group_by(county_id, inventory_year, variable) %>%
  summarize(value = sum(value), .groups = "keep")

### compare taz to urbansim

taz_us <- forecast_out %>%
  left_join(us_county,
    by = c(
      "variable",
      "inventory_year",
      "county_id"
    )
  )

# ggplot(
#   taz_us,
#   aes(
#     x = value.x,
#     y = value.y,
#     col = county_name,
#     shape = as.character(inventory_year)
#   )
# ) +
#   geom_point() +
#   geom_abline(intercept = 0, slope = 1) +
#   facet_wrap(. ~ variable)
# 
# # looks good, need to interpolate the missing TAZ years

forecast_interpolated <- forecast_out %>%
  right_join(
    expand.grid(
      inventory_year = seq(2020, 2050, by = 1),
      variable = unique(forecast_out$variable),
      county_name = unique(forecast_out$county_name)
    ),
    by = c("inventory_year", "variable", "county_name")
  ) %>%
  group_by(variable, county_name) %>%
  arrange(inventory_year, .by_group = TRUE) %>%
  mutate(value = imputeTS::na_kalman(value)) %>%
  ungroup() %>%
  # bring back to decadal and most recent inventory year
  filter(inventory_year %in% c(2020, 2022, 2030, 2040, 2050)) %>%
  mutate(data_source = if_else(is.na(geoid),
    "Interpolated",
    "TAZ forecast"
  )) %>%
  select(variable, value, inventory_year, geoid, county_name) %>%
  # fill in missing geoid
  group_by(county_name) %>%
  tidyr::fill(geoid, .direction = "downup") %>%
  ungroup()

## compare to urbansim again, expecting some differences

forecast_us <- forecast_interpolated %>%
  mutate(county_id = substr(geoid, 3, 5)) %>%
  left_join(us_county,
    by = c(
      "variable",
      "inventory_year",
      "county_id"
    )
  )

# ggplot(
#   forecast_us,
#   aes(
#     x = value.x,
#     y = value.y,
#     col = county_name,
#     shape = as.character(inventory_year)
#   )
# ) +
#   geom_point() +
#   geom_abline(intercept = 0, slope = 1) +
#   facet_wrap(. ~ variable)
# 
# # again looks good

message("Saving demographic forecast data to: \n\t _meta/data/demographic_forecast_11_county.RDS")
write_rds(
  forecast_interpolated,
  "_meta/data/demographic_forecast_11_county.RDS"
)


message("Finished demographic forecast")



