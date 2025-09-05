### develop regional residential emissions targets based on state GCAM models

source("R/_load_pkgs.R")
source("R/cprg_colors.R")

interpolate_emissions <- function(df) {
  df %>%
    mutate(emissions_year = as.numeric(emissions_year)) %>%
    group_by(sector, category) %>%
    # complete sequence of years from min to max
    complete(emissions_year = seq(min(emissions_year), max(emissions_year), by = 1)) %>%
    # interpolate missing values linearly
    mutate(value_emissions = approx(emissions_year, value_emissions, 
                                    xout = emissions_year, rule = 1)$y) %>%
    ungroup()
}


regional_housing_forecast <- read_rds("_meta/data/regional_housing_forecast.RDS") %>% 
  mutate(geog_name = "all") # to avoid check_input error

# regional density_output (seven county, but only importance is no change in density here)
density_output <- run_scenario_land_use()

# work around check_input issues:

orig_check_inputs <- ghg.ccap:::check_inputs

assignInNamespace(
  "check_inputs",
  function(name, value) {
    if (name == "selected_ctu") {
      if (value %in% c("all", "CCAP Region")) return()
      else if (!value %in% unique(ghg.ccap::transportation_data$passenger$geog_name)) {
        cli::cli_abort("Enter a valid geog_name name")
      }
    } else {
      # call the saved original, not the patched one
      orig_check_inputs(name, value)
    }
  },
  ns = "ghg.ccap"
)

# bau
bau_results <- run_scenario_building(
  res_tb = regional_housing_forecast,
  res_tb_bau = regional_housing_forecast,
    .baseline_year = 2022,
    .selected_ctu = "CCAP Region",  # iterates across all ctus
    .density_output = density_output
  )


) %>%
  group_by(inventory_year, scenario) %>%
  summarize(mwh = sum(residential_mwh),
            mcf = sum(residential_mcf),
            electricity_emissions = sum(electricity_emissions),
            natgas_emissions = sum(natural_gas_emissions)) %>%
  ungroup() %>%
  filter(scenario == "bau")

#### read in and create business as usual projections from different sectors #

county_emissions <- readRDS("_meta/data/cprg_county_emissions.RDS") 

tr_bau <- read_csv("./_meta/data-raw/bau_projections/transportation_county_emissions_time_series.csv")
wd_ns_bau <- read_csv("./_meta/data-raw/bau_projections/bau_region_ww_sw_ns.csv")
res_bau <- read_csv("./_meta/data-raw/bau_projections/residential_bau.csv")
comm_bau <- read_csv("./_meta/data-raw/bau_projections/comm_nonres_bau_2005_2050.csv")

ind_mpca <- read_rds("_meta/data/gcam/mpca_subsector_gcam.RDS") %>% 
  filter(sector == "Industry", scenario == "Current policies",
         emissions_year >= 2025) %>% 
  select(emissions_year, subsector_mc, proportion_of_2020)

aviation_mpca <- read_rds("_meta/data/gcam/mpca_subsector_gcam.RDS") %>% 
  filter(subsector_mc == "Aviation", scenario == "Current policies",
         emissions_year >= 2025) %>% 
  select(emissions_year, subsector_mc, proportion_of_2020)

ind_bau <- county_emissions %>% 
  filter(category %in% unique(ind_mpca$subsector_mc),
         emissions_year == 2020,
         !county_name %in% c("St. Croix",
                             "Pierce",
                             "Chisago",
                             "Sherburne")) %>% 
  group_by(sector, category) %>% 
  summarise(value_emissions_2020 = sum(value_emissions)) %>% 
  ungroup() %>% 
  left_join(ind_mpca, by = c("category" = "subsector_mc")) %>% 
  mutate(value_emissions = value_emissions_2020 * proportion_of_2020,
         emissions_year = as.numeric(emissions_year)) %>% 
  select(sector, category, emissions_year, value_emissions) %>% 
  #bring back in inventory data
  bind_rows(
    county_emissions %>% 
      filter(category %in% unique(ind_mpca$subsector_mc),
             emissions_year >= 2005,
             !county_name %in% c("St. Croix",
                                 "Pierce",
                                 "Chisago",
                                 "Sherburne")) %>% 
      group_by(emissions_year,sector, category) %>% 
      summarise(value_emissions = sum(value_emissions)) %>% 
      ungroup()
  )



# Apply the interpolation function to your data
ind_bau_interpolated <- interpolate_emissions(ind_bau)

aviation_bau <- county_emissions %>% 
  filter(category == "Aviation",
         emissions_year == 2021,
         !county_name %in% c("St. Croix",
                             "Pierce",
                             "Chisago",
                             "Sherburne")) %>% 
  group_by(sector, category) %>% 
  summarise(value_emissions_2021 = sum(value_emissions)) %>% 
  ungroup() %>% 
  left_join(aviation_mpca, by = c("category" = "subsector_mc")) %>% 
  mutate(value_emissions = value_emissions_2021 * proportion_of_2020,
         emissions_year = as.numeric(emissions_year)) %>% 
  select(sector, category, emissions_year, value_emissions) %>% 
  bind_rows(
    county_emissions %>% 
      filter(emissions_year >= 2005,
             category == "Aviation",
             !county_name %in% c("St. Croix",
                                 "Pierce",
                                 "Chisago",
                                 "Sherburne")) %>% 
      group_by(emissions_year, sector, category) %>% 
      summarise(value_emissions = sum(value_emissions)) %>% 
      ungroup()
  )

aviation_bau_interpolated <- interpolate_emissions(aviation_bau)


# complete ag by holding steady

ag_bau <- county_emissions %>% 
  filter(emissions_year == 2021,
         sector == "Agriculture",
         !county_name %in% c("St. Croix",
                             "Pierce",
                             "Chisago",
                             "Sherburne")) %>% 
  group_by(sector, category) %>% 
  summarize(value_emissions = sum(value_emissions)) %>% 
  ungroup() %>% 
  cross_join(data.frame(emissions_year = seq(2025, 2050, by = 1))) %>% 
  bind_rows(county_emissions %>% 
              filter(sector == "Agriculture",
                     !county_name %in% c("St. Croix",
                                         "Pierce",
                                         "Chisago",
                                         "Sherburne")) %>% 
              group_by(emissions_year,sector, category) %>% 
              summarize(value_emissions = sum(value_emissions, na.rm = TRUE)) %>% 
              ungroup()
  )

ag_bau_interpolated <- interpolate_emissions(ag_bau)


### calculate commercial natural gas emissions

comm_bau_out <- comm_bau %>% 
  group_by(inventory_year, sector) %>% 
  summarize(mwh = sum(mwh, na.rm = TRUE),
            mcf = sum(mcf, na.rm = TRUE)) %>% 
  # grab EFs from res data
  left_join(res_bau %>% 
              mutate(mwh_ef = electricity_emissions / mwh,
                     mcf_ef = natgas_emissions / mcf) %>% 
              select(inventory_year, mwh_ef, mcf_ef)
  ) %>% 
  mutate(electricity_emissions = mwh * mwh_ef,
         natgas_emissions = mcf * mcf_ef)

#process and bind_rows into one bau object

bau <- bind_rows(wd_ns_bau %>% 
                   group_by(inventory_year, sector) %>% 
                   summarise(value_emissions = sum(value_emissions)) %>% 
                   ungroup() %>% 
                   mutate(sector = str_to_title(sector)) %>% 
                   rename(emissions_year = inventory_year),
                 tr_bau %>% 
                   group_by(emissions_year) %>% 
                   summarise(value_emissions = sum(emissions_metric_tons_co2e)) %>% 
                   ungroup() %>% 
                   mutate(sector = "Transportation"),
                 aviation_bau_interpolated %>% 
                   select(-category),
                 res_bau %>% 
                   select(emissions_year = inventory_year, 
                          value_emissions = electricity_emissions) %>% 
                   mutate(sector = "Electricity"),
                 res_bau %>% 
                   select(emissions_year = inventory_year, 
                          value_emissions = natgas_emissions) %>% 
                   mutate(sector = "Building Fuel"),
                 comm_bau_out %>% 
                   select(emissions_year = inventory_year, 
                          value_emissions = electricity_emissions) %>% 
                   mutate(sector = "Electricity"),
                 comm_bau_out %>% 
                   select(emissions_year = inventory_year, 
                          value_emissions = natgas_emissions) %>% 
                   mutate(sector = "Building Fuel"),
                 ind_bau_interpolated %>% 
                   group_by(sector, emissions_year) %>% 
                   summarize(value_emissions = sum(value_emissions)) %>% 
                   ungroup()%>% 
                   mutate(sector = "Industrial Processes"),
                 ag_bau %>% group_by(sector, emissions_year) %>% 
                   summarize(value_emissions = sum(value_emissions)) %>% 
                   ungroup()
) %>% 
  filter(emissions_year >= 2005) %>% 
  group_by(emissions_year, sector) %>% 
  summarize(value_emissions = sum(value_emissions)) %>% 
  ungroup() 

bau

bau %>%
  filter(emissions_year %in% c(2005,2022, 2030, 2050)) %>%
  group_by(emissions_year) %>%
  summarize(total_emissions = sum(value_emissions)) %>%
  mutate(proportion_of_2005 = total_emissions / total_emissions[emissions_year == 2005])

bau %>%
  filter(emissions_year %in% c(2005, 2022, 2030, 2050)) %>%
  group_by(emissions_year, sector)%>%
  summarize(total_emissions = sum(value_emissions)) %>% 
  print(n = 100)

sector_colors <- unlist(sector_colors_alt)

bau_positive <- bau %>%
  filter(value_emissions >= 0) %>% 
  mutate(sector = factor(sector,
                         levels = c("Electricity",
                                    "Transportation",
                                    "Building Fuel",
                                    "Industrial Processes",
                                    "Waste",
                                    "Agriculture")))

bau_negative <- bau %>%
  filter(value_emissions < 0)

bau_projection_plot <- ggplot() +
  # Add positive emissions as stacked areas above zero
  geom_area(data = bau_positive, 
            aes(x = emissions_year, y = value_emissions, fill = sector),
            position = "stack") +
  # Add negative emissions (natural systems) below zero
  geom_area(data = bau_negative,
            aes(x = emissions_year, y = value_emissions, fill = sector)) +
  # Apply the custom color palette
  scale_fill_manual(values = sector_colors, name = "Sector") +
  # Add labels and formatting
  labs(
    title = "Business as usual projections by sector",
    subtitle = expression(paste("(Million metric tons of ", CO[2], "e)")),
    x = "Year",
    y = ""
  ) +
  # Add a horizontal line at y = 0 for reference
  geom_hline(yintercept = 0, color = "black", linetype = "solid", alpha = 0.7) +
  # Clean theme
  theme_minimal() +
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    plot.subtitle = element_text(size =18),
    legend.position = "right",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16)
  ) +
  # Format y-axis to show values in scientific notation or scaled
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, suffix = "M"))

bau_projection_plot

ggsave(plot = bau_projection_plot,
       filename = paste0(wd,"/seven_county_bau_projections.png"),  # add your file path here
       width = 12,          
       height = 6,          
       units = "in",
       dpi = 300, 
       bg = "white")

