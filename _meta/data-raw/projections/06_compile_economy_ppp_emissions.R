### function for interpolation

source("R/_load_pkgs.R")
source("R/cprg_colors.R")
source("_meta/data-raw/projections/interpolate_emissions.R")


#### read in and create business as usual projections from different sectors #

county_emissions <- readRDS("_meta/data/cprg_county_emissions.RDS")

tr_bau <- readRDS("./_meta/data-raw/projections/tr_pathways.rds")
sw_bau <- readRDS("./_meta/data-raw/projections/waste_pathways.rds")
res_bau <- readRDS("_meta/data/residential_pathways.RDS")
nonres_ng_bau <- readRDS("./_meta/data-raw/projections/nonres_ng_pathways.rds")
nonres_elec_bau <- readRDS("./_meta/data-raw/projections/nonres_elec_bau.rds")
ind_bau <- readRDS("./_meta/data-raw/projections/ind_pathways.rds")
ag_bau <- readRDS("./_meta/data-raw/projections/ag_pathways.rds")
ns_bau <- readRDS("_meta/data/regional_ns_forecast.RDS")

nonres_elec_bau <- bind_rows(
  nonres_elec_bau,
  nonres_elec_bau %>%
    filter(emissions_year >= 2025) %>%
    mutate(scenario = "ppp")
)

aviation_mpca <- read_rds("_meta/data/gcam/mpca_subsector_gcam.RDS") %>%
  filter(
    subsector_mc == "Aviation", scenario %in% c(
      "CP after Fed RB",
      "PPP after Fed RB"
    ),
    emissions_year >= 2025
  ) %>%
  select(emissions_year, scenario, subsector_mc, proportion_of_2020)

aviation <- county_emissions %>%
  filter(
    category == "Aviation",
    emissions_year == 2021
  ) %>%
  group_by(sector, category) %>%
  summarise(value_emissions_2021 = sum(value_emissions), .groups="keep") %>%
  ungroup() %>%
  left_join(aviation_mpca, by = c("category" = "subsector_mc")) %>%
  mutate(
    value_emissions = value_emissions_2021 * proportion_of_2020,
    emissions_year = as.numeric(emissions_year),
    scenario = case_when(
      scenario == "CP after Fed RB" ~ "bau",
      scenario == "PPP after Fed RB" ~ "ppp"
    )
  ) %>%
  select(sector, scenario, category, emissions_year, value_emissions) %>%
  bind_rows(
    county_emissions %>%
      filter(
        emissions_year >= 2005,
        category == "Aviation"
      ) %>%
      group_by(emissions_year, sector, category) %>%
      summarise(value_emissions = sum(value_emissions), .groups="keep") %>%
      ungroup() %>%
      mutate(scenario = "bau")
  ) %>%
  arrange(emissions_year)

aviation_bau_interpolated <- interpolate_emissions(aviation) %>%
  mutate(
    sector = "Transportation",
    category = "Aviation"
  ) %>%
  arrange(emissions_year)

net_zero <- readRDS(file.path(here::here(), "_meta/data/regional_net_zero_target.RDS")) %>%
  pull(net_zero_target) * -1

pathways <- bind_rows(
  sw_bau %>%
    mutate(sector = "Waste"),
  tr_bau %>%
    mutate(sector = "Transportation"),
  aviation_bau_interpolated %>%
    select(-category),
  res_bau %>%
    select(
      emissions_year = inventory_year,
      value_emissions = electricity_emissions,
      scenario
    ) %>%
    mutate(sector = "Electricity"),
  res_bau %>%
    select(
      emissions_year = inventory_year,
      value_emissions = natural_gas_emissions,
      scenario
    ) %>%
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
    select(
      emissions_year = inventory_year,
      sector,
      value_emissions = total_emissions,
      scenario
    )
) %>%
  filter(
    emissions_year >= 2005,
    scenario %in% c("bau", "ppp")
  ) %>%
  group_by(emissions_year, sector, scenario) %>%
  summarize(value_emissions = sum(value_emissions) %>% round(digits = 2), .groups="keep") %>%
  ungroup() %>%
  filter(!(emissions_year <= 2025 & scenario == "ppp"))

saveRDS(pathways, "_meta/data-raw/projections/economy_ppp_emissions.RDS")
write.csv(pathways, "_meta/data-raw/projections/economy_ppp_emissions.csv",row.names = FALSE)

bau_line <- pathways %>%
  filter(
    scenario == "bau",
    value_emissions > 0
  ) %>%
  group_by(emissions_year) %>%
  summarize(value_emissions = sum(value_emissions), .groups="keep") %>%
  ungroup()

pathways_pos <- pathways %>%
  filter(
    emissions_year <= 2025 | scenario == "ppp",
    value_emissions >= 0
  ) %>%
  mutate(sector = factor(sector,
    levels = c(
      "Electricity",
      "Transportation",
      "Building Fuel",
      "Industrial Processes",
      "Waste",
      "Agriculture"
    )
  ))

pathways_neg <- pathways %>%
  filter(
    emissions_year <= 2025 | scenario == "ppp",
    value_emissions < 0
  )

sector_colors <- unlist(sector_colors_alt)


ppp_projection_plot <- ggplot() +
  # Add positive emissions as stacked areas above zero
  geom_area(
    data = pathways_pos,
    aes(x = emissions_year, y = value_emissions, fill = sector),
    position = "stack"
  ) +
  # Add negative emissions (natural systems) below zero
  geom_area(
    data = pathways_neg,
    aes(x = emissions_year, y = value_emissions, fill = sector)
  ) +
  # Apply the custom color palette
  scale_fill_manual(values = sector_colors, name = "Sector") +
  geom_line(
    data = bau_line,
    aes(x = emissions_year, y = value_emissions),
    linetype = "dashed", color = "black", linewidth = 1
  ) +
  geom_point(
    data = data.frame(emissions_year = 2050, value_emissions = net_zero),
    aes(x = emissions_year, y = value_emissions),
    shape = "*", # asterisk
    size = 12, # make larger or smaller
    stroke = 1.5, # line thickness of the asterisk
    color = "black"
  ) +
  # Add labels and formatting
  labs(
    title = "Potential policy pathways by sector",
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
    plot.subtitle = element_text(size = 18),
    legend.position = "right",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16)
  ) +
  # Format y-axis to show values in scientific notation or scaled
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, suffix = "M"))

print(ppp_projection_plot)

message("Saving economy-wide PPP projections plot to: \n\t ~/imgs/eleven_county_ppp_projections.png")
ggsave(
  plot = ppp_projection_plot,
  filename = paste0(here::here(), "/imgs/eleven_county_ppp_projections.png"), # add your file path here
  width = 12,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)


message("Finished economy-wide PPP projections")
