### develop transportation emissions targets based on state GCAM models
### TODO: Liz replace with ghg.ccap runs

source("R/_load_pkgs.R")
source("R/cprg_colors.R")
source("_meta/data-raw/projections/interpolate_emissions.R")
source("_meta/data-raw/projections/01_projections_plotter.R")

## load state gcam modeling

gcam <- read_rds("_meta/data/gcam/mpca_subsector_gcam.RDS")
unique(gcam$subsector_mc)

### set net-zero by regional analysis

county_emissions <- readRDS(file.path(here::here(), "_meta/data/ppp_baseline_diff.RDS"))

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

# load in transportation bau and ppp for region

tr_pathways <- read_rds(paste0(here::here(), "/_meta/data-raw/projections/ppp_baseline_diff.RDS"))
county_emissions %>% 
  filter(emissions_year == 2015, sector == "Transportation", source == "On-road",
         !county_name %in% c("Chisago", "St. Croix", "Pierce", "Sherburne")) %>% 
  group_by(category) %>% 
  summarize(sum(value_emissions))


tr_scenarios <- gcam %>%
  filter(
    sector == "Transportation",
    scenario %in% c(
      "Net-Zero Pathway",
      "PPP after Fed RB",
      "CP after Fed RB"
    )
  ) %>% # create new on-road category
  filter(subsector_mc %in% c(
    "Buses",
    "Passenger vehicles",
    "Trucks"
  )) %>%
  group_by(emissions_year, sector, scenario) %>%
  summarize(value_emissions = sum(value_emissions), .groups = "keep") %>%
  ungroup() %>%
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

# create a new alternative PPP scenario
ppp_2025 <-
  tr_emissions_pathways %>%
  filter(scenario == "bau", emissions_year == "2025") %>%
  pull(value_emissions) -
  tr_emissions_pathways %>%
  filter(scenario == "ppp", emissions_year == "2025") %>%
  pull(value_emissions)

# create new scenario
tr_emissions_pathways <- tr_emissions_pathways %>%
  mutate(value_emissions = if_else(scenario == "ppp" & emissions_year >= 2025,
    value_emissions + ppp_2025,
    value_emissions
  ))

# waldo::compare(tr_emissions_pathways, readRDS("_meta/data-raw/projections/tr_pathways.rds"))
message("Saving transportation projections data to: \n\t _meta/data-raw/projections/tr_pathways.rds")
saveRDS(
  tr_emissions_pathways,
  "_meta/data-raw/projections/tr_pathways.rds"
)

### graph it!!####

#  base data (2005-2025, identical across scenarios)
base_data <- tr_emissions_pathways %>%
  filter(emissions_year <= 2025)


#  diverging scenarios (2026+)
diverging_data <- tr_emissions_pathways %>%
  filter(emissions_year >= 2025) %>%
  mutate(segment = "diverging")

# net_zero_data <- diverging_data %>% filter(scenario == "nz")

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
  width = 12,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)

### numbers for CCAP document
# sector wide 2030/2050 scenario to BAU comparisons

tr_2030 <- tr_emissions_pathways %>%
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

tr_2050 <- tr_emissions_pathways %>%
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
