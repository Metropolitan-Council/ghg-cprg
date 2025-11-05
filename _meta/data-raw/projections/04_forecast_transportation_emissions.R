### develop transportation emissions targets based on state GCAM models
### TODO: Liz replace with ghg.ccap runs

source("R/_load_pkgs.R")
source("R/cprg_colors.R")
source("_meta/data-raw/projections/interpolate_emissions.R")
source("_meta/data-raw/projections/01_projections_plotter.R")

source("_meta/data-raw/ctu_coctu_index.R")


library(ghg.ccap)
ctu_list <- ghg.ccap::building_data$non_residential %>%
  ungroup() %>%
  # filter(
  #   sp_categories == "commercial_jobs",
  #   inventory_year == 2021,
  #   value != 0,
  #   !geog_name %in% c(
  #     "Blakeley Twp.",
  #     "Coates",
  #     "Douglas Twp.",
  #     "Fort Snelling",
  #     "Grey Cloud Island Twp.",
  #     "Hilltop",
  #     "Laketown Twp.", # no PLDV in 2040
  #     "Landfall", # no single-family homes
  #     "Lilydale", # ditto
  #     "Rogers"
  #   )
  # ) %>%
  select(geog_name) %>%
  mutate(ctu_desc = as.character(geog_name)) %>%
  unique() %>% 
  filter(stringr::str_detect(geog_name, "County", negate = TRUE))

all_bau <- purrr::map(
  ctu_list$geog_name,
  (function(x) {
    cli::cli_alert_info(x)
    suppressMessages(
      run_all_modules(
        .scenario = "bau",
        .selected_ctu = x,
        pass_tb = ghg.ccap::transportation_data$passenger,
        freight_tb = ghg.ccap::transportation_data$freight,
        .factor_values = ghg.ccap::factor_values,
        .enviro_factors = ghg.ccap::enviro_factors,
        .elast = ghg.ccap::elast,
        .elast_5d = ghg.ccap::elast_5d,
        .fuel_economy = ghg.ccap::fuel_economy,
        tb = ghg.ccap::land_use_data,
        non_res_tb = ghg.ccap::building_data$non_residential,
        res_tb = ghg.ccap::building_data$residential,
        res_tb_bau = ghg.ccap::building_data$residential,
        non_res_tb_bau = ghg.ccap::building_data$non_residential,
        run_land_use = FALSE,
        run_non_residential = FALSE
      )
    )
  })
)

# essential functions

summarize_emiss <-   function(x){
  bau_mode_year <- x$transp$passenger_all %>%
    dplyr::bind_rows(x$transp$freight_all) %>%
    dplyr::filter(!mode %in% c(
      "MM", "RI", 
      "RU", "FR",
      "WAT", "AIR"
    )) %>%
    dplyr::left_join(
      ghg.ccap::transportation_index$modes %>%
        dplyr::select(mode_abbrev, mode_description_1, sector, category),
      by = c("mode" = "mode_abbrev")
    ) %>%
    dplyr::mutate(emissions_year = as.numeric(year)) %>%
    dplyr::group_by(emissions_year, type,scenario, geog_name, geog_id, category, sector) %>%
    dplyr::summarize(
      dir_ghg = sum(dir_ghg, na.rm = T),
      vmt = sum(vmt, na.rm = T),
      .groups = "keep"
    ) %>% 
    left_join(ctu_coctu_index, by  = c("geog_id" = "gnis",
                                       "geog_name" = "ctu_name")
    )
  
}

summarize_county <- function(x){
  x %>% 
  group_by(county_name, emissions_year,scenario, type, sector, category) %>% 
    dplyr::summarize(
      dir_ghg = sum(dir_ghg, na.rm = T),
      vmt = sum(vmt, na.rm = T),
      n_ctus = length(unique(geog_id)),
      .groups = "keep"
    )
}

summarize_region <- function(x){
  x %>% 
    group_by(emissions_year, scenario, type, sector, category) %>% 
    dplyr::summarize(
      dir_ghg = sum(dir_ghg, na.rm = T),
      vmt = sum(vmt, na.rm = T),
      n_ctus = length(unique(geog_id)),
      .groups = "keep"
    )
}


bau_mode_year <- purrr::map_dfr(
  all_bau,
  summarize_emiss
)

bau_mode_year %>% 
  summarize_region()

bau_mode_year %>% 
  summarize_county()



# start modeling reductions

ev <- purrr::map(
  ctu_list$geog_name,
  (function(x) {
    cli::cli_alert_info(x)
    suppressMessages(
      run_all_modules(
        .scenario = "bau",
        .selected_ctu = x,
        pass_tb = ghg.ccap::transportation_data$passenger,
        freight_tb = ghg.ccap::transportation_data$freight,
        .factor_values = ghg.ccap::factor_values,
        .enviro_factors = ghg.ccap::enviro_factors,
        .elast = ghg.ccap::elast,
        .elast_5d = ghg.ccap::elast_5d,
        .fuel_economy = ghg.ccap::fuel_economy,
        tb = ghg.ccap::land_use_data,
        non_res_tb = ghg.ccap::building_data$non_residential,
        res_tb = ghg.ccap::building_data$residential,
        res_tb_bau = ghg.ccap::building_data$residential,
        non_res_tb_bau = ghg.ccap::building_data$non_residential,
        run_land_use = FALSE,
        run_non_residential = FALSE,
        .bev_pct_sales = 0.75,
        
      )
    )
  })
) 

ev_mode_year <- purrr::map_dfr(ev,
              summarize_emiss)





## load state gcam modeling

gcam <- read_rds("_meta/data/gcam/mpca_subsector_gcam.RDS")
unique(gcam$subsector_mc)

### set net-zero by regional analysis

county_emissions <- readRDS(file.path(here::here(), "_meta/data/cprg_county_emissions.RDS"))

## net-zero sequestration
seq_target <- readRDS(file.path(here::here(), "_meta/data/regional_net_zero_target.RDS")) %>%
  pull(net_zero_target)


### transportation target
tr_target <- county_emissions %>%
  filter(
    emissions_year == 2022,
    sector == "Transportation",
    category != "Aviation"
  ) %>%
  pull(value_emissions) %>%
  sum() / # residential natural gas emissions
  county_emissions %>%
  filter(
    emissions_year == 2022,
    category != "Electricity"
  ) %>%
  pull(value_emissions) %>%
  sum() * # regional wide emissions minus electricity
  seq_target * -1 # emissions goal

# load in transportation bau for seven county

tr_bau <- read_csv(paste0(here::here(), "/_meta/data-raw/projections/transportation_county_emissions_time_series.csv")) %>%
  group_by(emissions_year) %>%
  summarize(value_emissions = sum(emissions_metric_tons_co2e), .groups = "keep") %>%
  ungroup()

# load in passenger car ppp for region

tr_pathways <- read_rds(paste0(here::here(), "/_meta/data-raw/projections/ppp_baseline_diff.RDS"))

bau_percentage <- tr_bau %>%
  mutate(perc_2022 = value_emissions / value_emissions[emissions_year == 2022])

tr_emissions_collar <- county_emissions %>%
  filter(
    sector == "Transportation",
    category != "Aviation",
    county_name %in% c(
      "St. Croix",
      "Pierce",
      "Sherburne",
      "Chisago"
    )
  ) %>%
  filter(emissions_year == 2022) %>%
  summarize(value_emissions = sum(value_emissions)) %>%
  ungroup() %>%
  cross_join(bau_percentage) %>%
  mutate(value_emissions_collar = value_emissions.x * perc_2022) %>%
  filter(emissions_year >= 2023) %>%
  select(emissions_year, value_emissions_collar)

# join seven county with 4 collar counties
tr_emissions_bau <- bind_rows(
  county_emissions %>%
    filter(
      sector == "Transportation",
      category != "Aviation"
    ) %>%
    group_by(emissions_year) %>%
    summarize(value_emissions = sum(value_emissions)) %>%
    mutate(scenario = "bau"),
  tr_bau %>%
    left_join(tr_emissions_collar, by = join_by(emissions_year)) %>%
    mutate(
      value_emissions = value_emissions + value_emissions_collar,
      scenario = "bau"
    ) %>%
    filter(emissions_year >= 2023) %>%
    select(emissions_year, value_emissions, scenario)
)

### insert passenger vehicle reductions from ghg.ccap analysis - LR

passenger_reductions <- tr_pathways %>% 
  filter(category == "Passenger vehicles") %>% 
  ungroup() %>% 
  mutate(proportion_2020 = dir_ghg.scen / dir_ghg.scen[emissions_year == 2020]) %>% 
  select(emissions_year, subsector_mc = category, proportion_2020)

# update gcam with above proportions
gcam_updated <- gcam %>%
  # focus on relevant subsectors
  filter(
    sector == "Transportation",
    scenario %in% c("PPP after Fed RB"),
    subsector_mc %in% c("Buses", "Passenger vehicles", "Trucks")
  ) %>%
  mutate(emissions_year = as.numeric(emissions_year)) %>% 
  # join in the replacement proportions for passenger vehicles
  left_join(
    passenger_reductions %>% 
      rename(new_proportion_2020 = proportion_2020),
    by = c("emissions_year", "subsector_mc")
  ) %>%
  # update the proportion_of_2020 only for passenger vehicles
  mutate(
    proportion_of_2020 = if_else(
      subsector_mc == "Passenger vehicles" & !is.na(new_proportion_2020),
      new_proportion_2020,
      proportion_of_2020
    ),
    # recalculate emissions based on updated proportion_of_2020
    value_emissions = if_else(
      subsector_mc == "Passenger vehicles",
      value_2020 * proportion_of_2020,
      value_emissions
    )
  ) %>%
  select(-new_proportion_2020)

tr_scenarios <- gcam_updated %>%
  filter(
    sector == "Transportation",
    scenario %in% c(
      "PPP after Fed RB"
    )
  ) %>% # create new on-road category
  filter(subsector_mc %in% c(
    "Buses",
    "Passenger vehicles",
    "Trucks"
  )) %>%
  group_by(emissions_year, sector, scenario) %>%
  summarize(value_emissions = sum(value_emissions), .groups = "keep") %>% 
  ungroup()%>%
  mutate(value_2020 = value_emissions / value_emissions[emissions_year == 2020]) 



tr_emissions_proj <- tr_emissions_bau %>%
  filter(emissions_year == 2020) %>%
  mutate(
    baseline_emissions = value_emissions,
    sector = "Transportation"
  ) %>%
  select(-scenario) %>%
  left_join(
    tr_scenarios %>%
      filter(emissions_year >= 2025),
    by = c("sector")
  ) %>%
  mutate(
    value_emissions = baseline_emissions * value_2020,
    scenario = case_when(
      scenario == "CP after Fed RB" ~ "bau",
      scenario == "PPP after Fed RB" ~ "ppp"
    ),
    emissions_year = as.numeric(emissions_year.y)
  ) %>%
  filter(!is.na(scenario)) %>%
  group_by(
    emissions_year,
    scenario
  ) %>%
  summarize(value_emissions = sum(value_emissions), .groups = "keep") %>%
  ungroup()


tr_emissions_pathways <- interpolate_emissions(bind_rows(
  tr_emissions_bau,
  tr_emissions_proj %>%
    filter(scenario == "ppp")
))




### problems with state's PPP, shifting up
# 
# # create a new alternative PPP scenario
# ppp_2025 <-
#   tr_emissions_pathways %>%
#   filter(scenario == "bau", emissions_year == "2025") %>%
#   pull(value_emissions) -
#   tr_emissions_pathways %>%
#   filter(scenario == "ppp", emissions_year == "2025") %>%
#   pull(value_emissions)
# 
# # create new scenario
# tr_emissions_pathways <- tr_emissions_pathways %>%
#   mutate(value_emissions = if_else(scenario == "ppp" & emissions_year >= 2025,
#     value_emissions + ppp_2025,
#     value_emissions
#   ))

# waldo::compare(tr_emissions_pathways, readRDS("_meta/data-raw/projections/tr_pathways.rds"))


### graph it!!####

#  base data (2005-2025, identical across scenarios)
base_data <- tr_emissions_pathways %>%
  filter(emissions_year <= 2025,
         scenario == "bau")


#  diverging scenarios (2026+)
diverging_data <- tr_emissions_pathways %>%
  filter(emissions_year >= 2025) %>%
  mutate(segment = "diverging")

# net_zero_data <- diverging_data %>% filter(scenario == "nz")

bau_data <- diverging_data %>% filter(scenario == "bau")

#sharp drop in PPP in 2025 due to 2020 weirdness, smoothing while anchoring to 2030 target

smooth_ppp <- tibble(
  emissions_year = 2025:2030
) %>%
  mutate(
    # Use a cubic interpolation between 2025 and 2030
    value_emissions = approx(
      x = c(2025, 2030),
      y = c( bau_data %>% filter(emissions_year == 2025) %>% pull(value_emissions),
             diverging_data %>% filter(emissions_year == 2030, scenario == "ppp") %>% pull(value_emissions)),
      xout = emissions_year,
      method = "linear" # or "spline" for smoother curve
    )$y
  ) %>% 
  mutate(
    scenario = "ppp",
    segment = "diverging"
  )

diverging_data <- bind_rows(
  diverging_data %>% filter(scenario == "bau"),
  smooth_ppp,
  diverging_data %>% filter(scenario == "ppp",emissions_year >= 2031)
)

tr_pathways_out <- bind_rows(
  base_data,
  diverging_data %>%
    filter(!(emissions_year == 2025 &
           scenario == "bau")) %>%
    select(-segment)
)

message("Saving transportation projections data to: \n\t _meta/data-raw/projections/tr_pathways.rds")
saveRDS(
  tr_emissions_pathways,
  "_meta/data-raw/projections/tr_pathways.rds"
)

# Create the plot
tr_plot <- plot_emissions_pathways(
  base_data = base_data,
  diverging_data = diverging_data,
  target_value = tr_target,
  target_year = 2050,
  base_cutoff_year = 2022,
  ppp_bau_color = "#60C8E9",
  y_max = 20e6,  # Optional: set max y value
  title = "On-road Transportation Emissions \n(Millions of CO2-equivalency)"
)

print(tr_plot)

message("Saving transportation projections plot to: \n\t ~/imgs/transportation_decarbonization_pathways.png")
ggplot2::ggsave(
  plot = tr_plot,
  filename = paste0(here::here(), "/imgs/transportation_decarbonization_pathways.png"), # add your file path here
  width = 14,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)

### numbers for CCAP document
# sector wide 2030/2050 scenario to BAU comparisons

tr_2030 <- tr_pathways_out %>%
  filter(emissions_year == 2030)

bau2030 <- tr_2030 %>%
  filter(scenario == "bau") %>%
  pull(value_emissions)

ppp2030 <- tr_2030 %>%
  filter(scenario == "ppp") %>%
  pull(value_emissions) -
  bau2030

nz2030 <- tr_2030 %>%
  filter(scenario == "nz") %>%
  pull(value_emissions) -
  bau2030

ppp2030
ppp2030 / bau2030
nz2030 / bau2030

# 2050

tr_2050 <- tr_pathways_out %>%
  filter(emissions_year == 2050)

bau2050 <- tr_2050 %>%
  filter(scenario == "bau") %>%
  pull(value_emissions)

ppp2050 <- tr_2050 %>%
  filter(scenario == "ppp") %>%
  pull(value_emissions) -
  bau2050

nz2050 <- tr_target - bau2050

ppp2050
nz2050
ppp2050 / bau2050
nz2050 / bau2050


message("Finished transportation projections")
