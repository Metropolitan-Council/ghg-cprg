### develop industrial emissions targets based on state GCAM models

source("R/_load_pkgs.R")
source("R/cprg_colors.R")
source("_meta/data-raw/projections/interpolate_emissions.R")
source("_meta/data-raw/projections/01_projections_plotter.R")

## load state gcam modeling

gcam <- read_rds("_meta/data/gcam/mpca_subsector_gcam.RDS")
unique(gcam$subsector_mc)


### set net-zero by regional analysis

county_emissions <- readRDS(file.path(here::here(), "_meta/data/cprg_county_emissions.RDS"))

## to be updated once we have better sequestration growth potential
seq_target <- readRDS(file.path(here::here(), "_meta/data/regional_net_zero_target.RDS")) %>%
  pull(net_zero_target)

industrial_emissions <- county_emissions %>%
  filter(sector == "Industrial") %>%
  mutate(category = if_else(
    category == "Industrial fuel combustion",
    "Industrial processes",
    category
  ))


### industrial target
ind_target <- industrial_emissions %>%
  filter(
    emissions_year == 2022,
    category != "Electricity"
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

industrial_scenarios <- gcam %>%
  filter(
    subsector_mc %in% c(
      "Industrial fuel combustion",
      "Industrial natural gas",
      "Industrial processes",
      "Refinery processes"
    ),
    scenario %in% c(
      "Net-Zero Pathway",
      "PPP after Fed RB",
      "CP after Fed RB"
    )
  )

industrial_emissions_proj <- industrial_emissions %>%
  filter(emissions_year == 2020) %>%
  group_by(category) %>%
  summarize(baseline_emissions = sum(value_emissions), .groups = "keep") %>%
  left_join(
    industrial_scenarios %>%
      filter(emissions_year >= 2025),
    by = c("category" = "subsector_mc")
  ) %>%
  mutate(
    value_emissions = baseline_emissions * proportion_of_2020,
    scenario = case_when(
      scenario == "CP after Fed RB" ~ "bau",
      scenario == "PPP after Fed RB" ~ "ppp",
      scenario == "Net-Zero Pathway" ~ "nz"
    )
  ) %>%
  filter(!is.na(value_emissions)) %>%
  group_by(
    emissions_year,
    scenario
  ) %>%
  summarize(value_emissions = sum(value_emissions), .groups = "keep") %>%
  ungroup()

industrial_emissions_proj <- interpolate_emissions(industrial_emissions_proj)


#  base data (2005-2025, identical across scenarios)
base_data <- industrial_emissions %>%
  filter(emissions_year <= 2025) %>% # Use any scenario since they're identical
  mutate(segment = "base", scenario = "bau") %>%
  filter(!category %in% c("Electricity", "Building Fuel")) %>%
  group_by(emissions_year) %>%
  summarize(value_emissions = sum(value_emissions), .groups = "keep") %>%
  ungroup()

bau_2025 <- industrial_emissions_proj %>%
  filter(scenario == "bau", emissions_year == 2025) %>%
  select(emissions_year, value_emissions)

# Append to base_data
extended <- base_data %>%
  bind_rows(bau_2025) %>%
  arrange(emissions_year)

# Fill in missing years by linear interpolation
base_data <- extended %>%
  complete(emissions_year = full_seq(emissions_year, 1)) %>%
  arrange(emissions_year) %>%
  mutate(value_emissions = zoo::na.approx(value_emissions, emissions_year, na.rm = FALSE))

## save bau data

ind_bau <- bind_rows(
  base_data %>%
    mutate(scenario = "bau"),
  industrial_emissions_proj %>%
    filter(
      emissions_year >= 2026,
      scenario != "nz"
    )
)
# waldo::compare(ind_bau, readRDS("_meta/data-raw/projections/ind_pathways.rds"))

message("Saving industrial projections data to: \n\t _meta/data-raw/projections/ind_pathways.rds")
saveRDS(
  ind_bau,
  "_meta/data-raw/projections/ind_pathways.rds"
)

#  diverging scenarios (2026+)
diverging_data <- industrial_emissions_proj %>%
  filter(emissions_year >= 2025) %>%
  mutate(segment = "diverging")

net_zero_data <- diverging_data %>% filter(scenario == "nz")

bau_data <- diverging_data %>% filter(scenario == "bau")

# PPP data - need to merge with net_zero for the lower bound
ppp_data <- diverging_data %>%
  filter(scenario == "ppp") %>%
  select(emissions_year, value_emissions) %>%
  rename(ppp_emissions = value_emissions)

# net_zero_for_ppp <- diverging_data %>%
#   filter(scenario == "nz") %>%
#   select(emissions_year, value_emissions) %>%
#   rename(net_zero_emissions = value_emissions)
#
# ppp_ribbon_data <- ppp_data %>%
#   left_join(net_zero_for_ppp, by = "emissions_year")
### graph it####

# Create the plot
ind_plot <- plot_emissions_pathways(
  base_data = base_data,
  diverging_data = diverging_data,
  target_value = ind_target,
  target_year = 2050,
  base_cutoff_year = 2022,
  ppp_bau_color = "#A42264",
  y_max = 6e6,  # Optional: set max y value
  title = "Industrial Emissions \n(Millions of CO2-equivalency)"
)

ind_plot


message("Saving industrial projections plot to: \n\t ~/imgs/industrial_decarbonization_pathways.png")
ggplot2::ggsave(
  plot = ind_plot,
  filename = paste0(here::here(), "/imgs/industrial_decarbonization_pathways.png"),
  width = 14,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)


### numbers for CCAP document
# sector wide 2030/2050 scenario to BAU comparisons

ind_2030 <- industrial_emissions_proj %>%
  filter(emissions_year == 2030)

bau2030 <- ind_2030 %>%
  filter(scenario == "bau") %>%
  pull(value_emissions)

ppp2030 <- ind_2030 %>%
  filter(scenario == "ppp") %>%
  pull(value_emissions) -
  bau2030

nz2030 <- ind_2030 %>%
  filter(scenario == "nz") %>%
  pull(value_emissions) -
  bau2030

ppp2030
ppp2030 / bau2030
nz2030 / bau2030

# 2050

ind_2050 <- industrial_emissions_proj %>%
  filter(emissions_year == 2050)

bau2050 <- ind_2050 %>%
  filter(scenario == "bau") %>%
  pull(value_emissions)

ppp2050 <- ind_2050 %>%
  filter(scenario == "ppp") %>%
  pull(value_emissions) -
  bau2050

nz2050 <- ind_target - bau2050

ppp2050
nz2050
ppp2050 / bau2050
nz2050 / bau2050


message("Finished industrial projections")
