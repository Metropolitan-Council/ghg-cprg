### function for interpolation

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

#### read in and create business as usual projections from different sectors #

county_emissions <- readRDS("_meta/data/cprg_county_emissions.RDS") 

tr_bau <- readRDS("./_meta/data-raw/projections/tr_pathways.rds")%>% 
  filter(scenario == "bau")
sw_bau <- readRDS("./_meta/data-raw/projections/sw_bau.rds")
res_bau <- readRDS("_meta/data/residential_pathways.RDS") %>% 
  filter(scenario == "bau")
nonres_ng_bau <- readRDS("./_meta/data-raw/projections/nonres_ng_pathways.rds")%>% 
  filter(scenario == "bau")
nonres_elec_bau <- readRDS("./_meta/data-raw/projections/nonres_elec_bau.rds")
ind_bau <- readRDS("./_meta/data-raw/projections/ind_bau.rds")%>% 
  filter(scenario == "bau")
ag_bau <- readRDS("./_meta/data-raw/projections/ag_bau.rds")%>% 
  filter(scenario == "bau")
ns_bau <- readRDS("_meta/data/regional_ns_forecast.RDS") %>% 
  filter(scenario == "bau")


aviation_mpca <- read_rds("_meta/data/gcam/mpca_subsector_gcam.RDS") %>% 
  filter(subsector_mc == "Aviation", scenario == "CP after Fed RB",
         emissions_year >= 2025) %>% 
  select(emissions_year, subsector_mc, proportion_of_2020)


aviation_bau <- county_emissions %>% 
  filter(category == "Aviation",
         emissions_year == 2021) %>% 
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
             category == "Aviation") %>% 
      group_by(emissions_year, sector, category) %>% 
      summarise(value_emissions = sum(value_emissions)) %>% 
      ungroup()
  ) %>% 
  arrange(emissions_year)

aviation_bau_interpolated <- interpolate_emissions(aviation_bau)


bau <- bind_rows(sw_bau %>% 
                   mutate(sector = "Waste"),
                 tr_bau %>% 
                   mutate(sector = "Transportation"),
                 aviation_bau_interpolated %>% 
                   select(-category),
                 res_bau %>% 
                   select(emissions_year = inventory_year, 
                          value_emissions = electricity_emissions) %>% 
                   mutate(sector = "Electricity"),
                 res_bau %>% 
                   select(emissions_year = inventory_year, 
                          value_emissions = natural_gas_emissions) %>% 
                   mutate(sector = "Building Fuel"),
                 nonres_elec_bau %>% 
                   mutate(sector = "Electricity"),
                 nonres_ng_bau %>% 
                   mutate(sector = "Building Fuel"),
                 ind_bau %>% 
                   mutate(sector = "Industrial Processes"),
                 ag_bau %>% 
                   mutate(sector = "Agriculture"),
                 ns_bau %>% 
                   select(emissions_year = inventory_year,
                          sector,
                          value_emissions = total_emissions)
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
       filename = paste0(wd,"/eleven_county_bau_projections.png"),  # add your file path here
       width = 12,          
       height = 6,          
       units = "in",
       dpi = 300, 
       bg = "white")

