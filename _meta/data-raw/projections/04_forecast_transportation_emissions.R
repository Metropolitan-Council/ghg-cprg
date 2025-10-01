### develop transportation emissions targets based on state GCAM models
### TODO: Liz replace with ghg.ccap runs

source("R/_load_pkgs.R")
source("R/cprg_colors.R")
source("_meta/data-raw/projections/interpolate_emissions.R")

## load state gcam modeling

gcam <- read_rds("_meta/data/gcam/mpca_subsector_gcam.RDS")
unique(gcam$subsector_mc)

### set net-zero by regional analysis

county_emissions <- readRDS(file.path(here::here(), "_meta/data/cprg_county_emissions.RDS"))

## to be updated once we have better sequestration growth potential
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

tr_bau <- read_csv(paste0(here::here(), "/_meta/data-raw/bau_projections/transportation_county_emissions_time_series.csv")) %>%
  group_by(emissions_year) %>%
  summarize(value_emissions = sum(emissions_metric_tons_co2e), .groups = "keep") %>%
  ungroup()

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
emissions_gg <- ggplot() +
  # Base fill (2005-2025, gray)
  geom_ribbon(
    data = base_data,
    aes(x = emissions_year, ymin = 0, ymax = value_emissions),
    fill = "gray80", alpha = 0.7
  ) +

  # Net zero fill (#36454F)
  # geom_ribbon(data = net_zero_data,
  #             aes(x = emissions_year, ymin = 0, ymax = value_emissions),
  #             fill = "#36454F", alpha = 0.3) +

  # PPP fill (from net_zero to ppp)
  geom_ribbon(
    data = ppp_data,
    aes(x = emissions_year, ymin = 0, ymax = ppp_emissions),
    fill = "#191970", alpha = 0.5
  ) +

  # Base line (2005-2025)
  geom_line(
    data = base_data,
    aes(x = emissions_year, y = value_emissions),
    color = "black", linewidth = 1
  ) +

  # Diverging scenario lines
  geom_line(
    data = diverging_data %>% filter(scenario == "bau"),
    aes(x = emissions_year, y = value_emissions, color = "Business as usual"),
    linetype = "dashed", linewidth = 1
  ) +
  geom_line(
    data = diverging_data %>% filter(scenario == "ppp"),
    aes(x = emissions_year, y = value_emissions, color = "Potential policy pathways"),
    linewidth = 1
  ) +
  geom_point(
    data = data.frame(emissions_year = 2050, value_emissions = tr_target),
    aes(x = emissions_year, y = value_emissions),
    shape = "*", # asterisk
    size = 12, # make larger or smaller
    stroke = 1.5, # line thickness of the asterisk
    color = "black"
  ) +
  geom_segment(aes(x = 2025, xend = 2025, y = 0, yend = base_data %>% filter(emissions_year == 2025) %>% pull(value_emissions)),
    color = "black", linetype = "solid", linewidth = 0.8
  ) +
  # annotate("text", x = 2025, y = max(your_data$value_emissions) * 0.9,
  #          label = "Historical | Projected", angle = 90, hjust = 1, size = 3.5) +

  # Manual color scale with correct order
  scale_color_manual(
    values = c(
      "Business as usual" = "black",
      # "Net zero" = "#36454F",
      "Potential policy pathways" = "#191970"
    ),
    breaks = c("Business as usual", "Potential policy pathways") # Force legend order
  ) +

  # Manual legend guide to show line types
  guides(
    color = guide_legend(
      title = "Scenarios",
      override.aes = list(
        linetype = c("dashed", "solid"),
        color = c("black", "#191970")
      )
    )
  ) +
  labs(
    x = "Year",
    y = "",
    title = "On-road Transportation Emissions \n(Millions of CO2-equivalency)"
  ) +
  scale_y_continuous(labels = label_number(scale = 1e-6)) + # convert to millions
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 18),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 18),
    legend.key.width = unit(1.2, "cm")
  ) +
  xlim(2005, 2050)

print(emissions_gg)

message("Saving transportation projections plot to: \n\t ~/imgs/transportation_decarbonization_pathways.png")
ggplot2::ggsave(
  plot = emissions_gg,
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
