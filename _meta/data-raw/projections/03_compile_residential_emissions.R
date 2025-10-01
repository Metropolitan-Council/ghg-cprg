### develop regional residential emissions targets based on state GCAM models

## restart and rerun when making updates to ghg.ccap@ccap-graphics
# remotes::install_github("Metropolitan-Council/ghg.ccap@ccap-graphics")
# enter 3 ('none') if prompted to update other packages

source("R/_load_pkgs.R")
source("R/cprg_colors.R")


## load state gcam modeling

gcam <- read_rds("_meta/data/gcam/mpca_subsector_gcam.RDS")
unique(gcam$subsector_mc)

res_scenarios <- gcam %>%
  filter(
    subsector_mc == "Residential natural gas",
    scenario %in% c(
      "Net-Zero Pathway",
      "PPP after Fed RB",
      "CP after Fed RB"
    )
  )

res_targets <- res_scenarios %>%
  filter(emissions_year %in% c(2030, 2050))

# load needed objects
regional_housing_forecast <- read_rds("_meta/data/regional_housing_forecast.RDS") %>%
  mutate(geog_id = "1")

# regional density_output (seven county, but only importance is no change in density here)
density_output <- run_scenario_land_use()


# bau
bau_results <- run_scenario_building(
  res_tb = regional_housing_forecast,
  res_tb_bau = regional_housing_forecast,
  .baseline_year = 2022,
  .selected_ctu = "CCAP Region",
  .density_output = density_output
)

### accelerated policy pathway ####

## how can we reverse engineer to meet targets?
### try basic optimization

## create targets

# Create a grid of parameter combinations, starting high to avoid too many permutations
param_grid <- expand.grid(
  new_homes_leed_gold_pct = seq(0.4, 1, 0.2),
  existing_retrofit_pct = seq(0.4, 1, 0.2),
  heat_pump_pct = seq(0.4, 1, 0.2)
)

### create emission target for 2050

target_emissions_2050 <- (bau_results %>%
  filter(
    scenario == "bau",
    inventory_year == 2005
  ) %>%
  pull(natural_gas_emissions)) *
  res_targets %>%
    filter(
      emissions_year == 2050,
      scenario == "PPP after Fed RB"
    ) %>%
    pull(proportion_of_2005)

evaluate_scenario <- function(params, target_emissions_2050) {
  result <- run_scenario_building(
    res_tb = regional_housing_forecast,
    res_tb_bau = regional_housing_forecast,
    .baseline_year = 2022,
    .selected_ctu = "CCAP Region",
    .scenario = "ppp",
    .density_output = density_output,
    .new_sf_homes_leed_gold_pct = params$new_homes_leed_gold_pct,
    .existing_sf_retrofit_pct = params$existing_retrofit_pct,
    .sf_heat_pump_pct = params$heat_pump_pct,
    .new_mf_homes_leed_gold_pct = params$new_homes_leed_gold_pct,
    .existing_mf_retrofit_pct = params$existing_retrofit_pct,
    .mf_heat_pump_pct = params$heat_pump_pct
  )

  # Calculate total emissions for target years
  # emissions_2030 <- result %>% filter(inventory_year == 2030,
  #                                     scenario == "ppp") %>% pull(natural_gas_emissions)
  emissions_2050 <- result %>%
    filter(
      inventory_year == 2050,
      scenario == "ppp"
    ) %>%
    pull(natural_gas_emissions)

  # Calculate distance from targets (you want to minimize this)
  distance <- abs(emissions_2050 - target_emissions_2050)

  return(distance)
}

cat("Running", nrow(param_grid), "parameter combinations...\n")

param_grid$distance <- NA

for (i in 1:nrow(param_grid)) {
  # Show progress every 100 iterations
  if (i %% 100 == 0) {
    cat("Progress:", i, "/", nrow(param_grid), "\n")
  }

  # Get current parameter combination
  current_params <- param_grid[i, ]

  # Evaluate distance using your function
  param_grid$distance[i] <- evaluate_scenario(current_params, target_emissions_2050)
}

param_grid_sorted <- param_grid %>%
  arrange(distance)

#### rerun at finer scale

param_grid_finer <- expand.grid(
  new_homes_leed_gold_pct = seq(0.35, 0.45, 0.02),
  existing_retrofit_pct = seq(0.75, 0.85, 0.02),
  heat_pump_pct = seq(0.55, 0.65, 0.02)
)

cat("Running", nrow(param_grid_finer), "parameter combinations...\n")

param_grid_finer$distance <- NA

for (i in 1:nrow(param_grid_finer)) {
  # Show progress every 100 iterations
  if (i %% 100 == 0) {
    cat("Progress:", i, "/", nrow(param_grid_finer), "\n")
  }

  # Get current parameter combination
  current_params <- param_grid_finer[i, ]

  # Evaluate distance using your function
  param_grid_finer$distance[i] <- evaluate_scenario(current_params, target_emissions_2050)
}

ppp_residential_vars <- param_grid_finer %>%
  arrange(distance) %>%
  slice(1)

ppp_results <- run_scenario_building(
  res_tb = regional_housing_forecast,
  res_tb_bau = regional_housing_forecast,
  .baseline_year = 2022,
  .selected_ctu = "CCAP Region",
  .density_output = density_output,
  .scenario = "ppp",
  .new_sf_homes_leed_gold_pct = ppp_residential_vars$new_homes_leed_gold_pct,
  .existing_sf_retrofit_pct = ppp_residential_vars$existing_retrofit_pct,
  .sf_heat_pump_pct = ppp_residential_vars$heat_pump_pct,
  .new_mf_homes_leed_gold_pct = ppp_residential_vars$new_homes_leed_gold_pct,
  .existing_mf_retrofit_pct = ppp_residential_vars$existing_retrofit_pct,
  .mf_heat_pump_pct = ppp_residential_vars$heat_pump_pct
) %>%
  filter(scenario == "ppp")



### net-zero pathway ####


## create targets

# Create a grid of parameter combinations, starting high to avoid too many permutations
param_grid <- expand.grid(
  new_homes_leed_gold_pct = seq(0.5, 1, 0.1),
  existing_retrofit_pct = seq(0.5, 1, 0.1),
  heat_pump_pct = seq(0.5, 1, 0.1)
)

### create emission target for 2050

target_emissions_2050_nz <- (bau_results %>%
  filter(
    scenario == "bau",
    inventory_year == 2005
  ) %>%
  pull(natural_gas_emissions)) *
  res_targets %>%
    filter(
      emissions_year == 2050,
      scenario == "Net-Zero Pathway"
    ) %>%
    pull(proportion_of_2005)

evaluate_scenario_nz <- function(params, target_emissions_2050_nz) {
  result <- run_scenario_building(
    res_tb = regional_housing_forecast,
    res_tb_bau = regional_housing_forecast,
    .baseline_year = 2022,
    .selected_ctu = "CCAP Region",
    .scenario = "nz",
    .density_output = density_output,
    .new_sf_homes_leed_gold_pct = params$new_homes_leed_gold_pct,
    .existing_sf_retrofit_pct = params$existing_retrofit_pct,
    .sf_heat_pump_pct = params$heat_pump_pct,
    .new_mf_homes_leed_gold_pct = params$new_homes_leed_gold_pct,
    .existing_mf_retrofit_pct = params$existing_retrofit_pct,
    .mf_heat_pump_pct = params$heat_pump_pct
  )

  # Calculate total emissions for target years
  # emissions_2030 <- result %>% filter(inventory_year == 2030,
  #                                     scenario == "ppp") %>% pull(natural_gas_emissions)
  emissions_2050 <- result %>%
    filter(
      inventory_year == 2050,
      scenario == "nz"
    ) %>%
    pull(natural_gas_emissions)

  # Calculate distance from targets (you want to minimize this)
  distance <- abs(emissions_2050 - target_emissions_2050_nz)

  return(distance)
}

cat("Running", nrow(param_grid), "parameter combinations...\n")

param_grid$distance_nz <- NA

for (i in 1:nrow(param_grid)) {
  # Show progress every 100 iterations
  if (i %% 100 == 0) {
    cat("Progress:", i, "/", nrow(param_grid), "\n")
  }

  # Get current parameter combination
  current_params <- param_grid[i, ]

  # Evaluate distance using your function
  param_grid$distance_nz[i] <- evaluate_scenario_nz(current_params, target_emissions_2050_nz)
}

param_grid_sorted_nz <- param_grid %>%
  arrange(distance_nz) %>%
  slice(1:5)

#### rerun at finer scale

param_grid_finer_nz <- expand.grid(
  new_homes_leed_gold_pct = seq(0.5, 0.6, 0.02),
  existing_retrofit_pct = seq(0.5, 0.6, 0.02),
  heat_pump_pct = seq(0.65, 0.75, 0.02)
)

cat("Running", nrow(param_grid_finer_nz), "parameter combinations...\n")

param_grid_finer_nz$distance <- NA

for (i in 1:nrow(param_grid_finer_nz)) {
  # Show progress every 100 iterations
  if (i %% 100 == 0) {
    cat("Progress:", i, "/", nrow(param_grid_finer_nz), "\n")
  }

  # Get current parameter combination
  current_params <- param_grid_finer_nz[i, ]

  # Evaluate distance using your function
  param_grid_finer_nz$distance[i] <- evaluate_scenario_nz(current_params, target_emissions_2050_nz)
}

residential_vars_nz <- param_grid_finer_nz %>%
  arrange(distance) %>%
  slice(1)

nz_results <- run_scenario_building(
  res_tb = regional_housing_forecast,
  res_tb_bau = regional_housing_forecast,
  .baseline_year = 2022,
  .selected_ctu = "CCAP Region",
  .density_output = density_output,
  .scenario = "nz",
  .new_sf_homes_leed_gold_pct = residential_vars_nz$new_homes_leed_gold_pct,
  .existing_sf_retrofit_pct = residential_vars_nz$existing_retrofit_pct,
  .sf_heat_pump_pct = residential_vars_nz$heat_pump_pct,
  .new_mf_homes_leed_gold_pct = residential_vars_nz$new_homes_leed_gold_pct,
  .existing_mf_retrofit_pct = residential_vars_nz$existing_retrofit_pct,
  .mf_heat_pump_pct = residential_vars_nz$heat_pump_pct
) %>%
  filter(scenario == "nz")

tail(nz_results)


residential_pathways <- bind_rows(
  bau_results %>%
    filter(scenario == "bau"),
  ppp_results,
  nz_results
)
# waldo::compare(residential_pathways, readRDS("_meta/data/residential_pathways.RDS"))

message("Saving residential projections data to: \n\t _meta/data/residential_pathways.RDS")
saveRDS(residential_pathways, "_meta/data/residential_pathways.RDS")


### numbers for CCAP document
# sector wide 2030/2050 scenario to BAU comparisons

residential_pathways_2030 <- residential_pathways %>%
  filter(inventory_year == 2030) %>%
  mutate(total_emissions = electricity_emissions + natural_gas_emissions)

bau2030 <- residential_pathways_2030 %>%
  filter(scenario == "bau") %>%
  pull(total_emissions)

ppp2030 <- residential_pathways_2030 %>%
  filter(scenario == "ppp") %>%
  pull(total_emissions) -
  bau2030

nz2030 <- residential_pathways_2030 %>%
  filter(scenario == "nz") %>%
  pull(total_emissions) -
  bau2030

ppp2030 / bau2030
nz2030 / bau2030

# 2050

residential_pathways_2050 <- residential_pathways %>%
  filter(inventory_year == 2050) %>%
  mutate(total_emissions = electricity_emissions + natural_gas_emissions)

bau2050 <- residential_pathways_2050 %>%
  filter(scenario == "bau") %>%
  pull(total_emissions)

ppp2050 <- residential_pathways_2050 %>%
  filter(scenario == "ppp") %>%
  pull(total_emissions) -
  bau2050

nz2050 <- residential_pathways_2050 %>%
  filter(scenario == "nz") %>%
  pull(total_emissions) -
  bau2050

ppp2050
nz2050
ppp2050 / bau2050
nz2050 / bau2050

message("Finished residential projections")

