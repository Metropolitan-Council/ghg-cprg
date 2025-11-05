#### plot residential pathways graphics

source("R/_load_pkgs.R")
source("_meta/data-raw/projections/01_projections_plotter.R")

residential_pathways <- readRDS("_meta/data-raw/projections/residential_pathways.RDS")

### set net-zero by regional analysis

county_emissions <- readRDS(file.path(here::here(), "_meta/data/cprg_county_emissions.RDS"))

# pull in net zero sequestration estimate
seq_target <- readRDS(file.path(here::here(), "_meta/data/regional_net_zero_target.RDS")) %>%
  pull(net_zero_target)

### residential target will be linked to proportion natural gas in 2022
### minus total electricity emissions (as they go to zero)
res_target <- county_emissions %>%
  filter(
    emissions_year == 2022,
    sector == "Residential",
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


total_emissions <- residential_pathways %>%
  mutate(total_emissions = electricity_emissions + natural_gas_emissions) %>%
  mutate(scenario = factor(scenario,
    levels = c("bau", "ppp", "nz")
  ))

#  base data (2005-2025, identical across scenarios)
base_data <- total_emissions %>%
  filter(inventory_year <= 2025, scenario == "bau") %>% # Use any scenario since they're identical
  mutate(segment = "base")

#  diverging scenarios (2026+)
diverging_data <- total_emissions %>%
  filter(inventory_year >= 2025) %>%
  mutate(segment = "diverging")

# net_zero_data <- diverging_data %>% filter(scenario == "nz")

# PPP data - need to merge with net_zero for the lower bound
ppp_data <- diverging_data %>%
  filter(scenario == "ppp") %>%
  select(inventory_year, total_emissions) %>%
  rename(ppp_emissions = total_emissions)

# net_zero_for_ppp <- diverging_data %>%
#   filter(scenario == "nz") %>%
#   select(inventory_year, total_emissions) %>%
#   rename(net_zero_emissions = total_emissions)
#
# ppp_ribbon_data <- ppp_data %>%
#   left_join(net_zero_for_ppp, by = "inventory_year")

# graph it ####

res_plot <- plot_emissions_pathways(
  base_data = base_data,
  diverging_data = diverging_data,
  target_value = res_target,
  target_year = 2050,
  base_cutoff_year = 2022,
  ppp_bau_color = "#8856A7",
  y_max = 20e6, # Optional: set max y value
  title = "Residential Building Emissions \n(Millions of CO2-equivalency)"
)

print(res_plot)

message("Saving residential projections plot to: \n\t ~/imgs/residential_decarbonization_pathways.png")
ggplot2::ggsave(
  plot = res_plot,
  filename = paste0(here::here(), "/imgs/residential_decarbonization_pathways.png"), # add your file path here
  width = 14,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)

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

# nz2030 <- residential_pathways_2030 %>% filter(scenario == "nz") %>% pull(total_emissions) -
#   bau2030

ppp2030
ppp2030 / bau2030
# nz2030 / bau2030

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

nz2050 <- res_target - bau2050

# nz2050 <- residential_pathways_2050 %>% filter(scenario == "nz") %>% pull(total_emissions) -
#   bau2050

ppp2050
nz2050
ppp2050 / bau2050
nz2050 / bau2050



message("Finished residential projections")
