### develop agricultural emissions targets based on state GCAM models

source("R/_load_pkgs.R")
source("R/cprg_colors.R")


## load state gcam modeling

gcam <- read_rds("_meta/data/gcam/mpca_subsector_gcam.RDS")
unique(gcam$subsector_mc)


### set net-zero by regional analysis

county_emissions <- readRDS(file.path(here::here(), "_meta/data/cprg_county_emissions.RDS"))

ag_land <- read_rds("_nature/data/nlcd_county_landcover_allyrs.rds") %>%
  filter(land_cover_type == "Cropland") %>% 
  group_by(inventory_year) %>% 
  summarize(ag_area_sq_km = sum(area)) %>% 
  ungroup() %>% 
  mutate(
    pct_of_prev_year = (ag_area_sq_km / lag(ag_area_sq_km) - 1)  # calculate percentage agriculture lost compared to previous year
  )

# take average loss over last 10 years
avg_loss <- ag_land %>%
  filter(inventory_year >= max(inventory_year) - 9) %>%  # last 10 years
  pull(pct_of_prev_year) %>% 
  mean()
# this will be expected loss of emissions per year
# create vector for 2023–2050
years <- 2022:2050
n_years <- length(years)

# cumulative decay vector
loss_df <- data.frame(avg_decay = avg_loss * seq_len(n_years),
                      emissions_year = years)

## to be updated once we have better sequestration growth potential
seq_target <- readRDS(file.path(here::here(), "_meta/data/regional_net_zero_target.RDS")) %>%
  pull(net_zero_target) 

### agricultural target
ag_target <- county_emissions %>%
  filter(
    emissions_year == 2022,
    sector == "Agriculture"
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

agriculture_emissions <- county_emissions %>%
  filter(sector == "Agriculture")

agriculture_scenarios <- gcam %>%
  filter(
    sector == "Agriculture",
    scenario %in% c(
      "Current policies without federal support",
      "CAF Pathway without federal support"
    )
  )

agriculture_emissions_proj <- agriculture_emissions %>%
  filter(emissions_year == 2022) %>%
  mutate(category = case_when(
    grepl("manure", ignore.case = TRUE, source) ~ "Manure management",
    grepl("Enteric fermentation", ignore.case = TRUE, source) ~ "Enteric fermentation",
    TRUE ~ "Cropland soil"
  )) %>%
  group_by(category) %>%
  summarize(baseline_emissions = sum(value_emissions), .groups = "keep") %>%
  cross_join(loss_df) %>% 
  mutate(value_emissions = baseline_emissions * (1 + avg_decay),
         scenario = "bau")

ppp_estimates <- agriculture_scenarios %>% 
  filter(emissions_year >= 2022) %>% 
  mutate(scenario = case_when(
    scenario == "Current policies without federal support" ~ "cp",
    scenario == "CAF Pathway without federal support" ~ "ppp"
  )) %>% 
  select(emissions_year, scenario, subsector_mc,value_emissions) %>% 
  pivot_wider(
    names_from = scenario,
    values_from = value_emissions
  ) %>%
  # Compute ratio CAF Pathway / Current Policies
  mutate(
    ppp_ratio = ppp / cp,
    emissions_year = as.numeric(emissions_year)
  )

# calculate ppp values based on reduction compared to cp (bau) and 
# restructure data frame for graphing
ag_scenarios <- agriculture_emissions_proj %>% 
  left_join(ppp_estimates,
            join_by(emissions_year,
                    category == subsector_mc))%>%
  mutate(ppp_value_emissions = value_emissions * ppp_ratio) %>%
  group_by(emissions_year) %>%
  summarize(
    value_emissions = sum(value_emissions, na.rm = TRUE),
    ppp_value_emissions = sum(ppp_value_emissions, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(value_emissions, ppp_value_emissions),
    names_to = "scenario",
    values_to = "value_emissions"
  ) %>%
  mutate(
    scenario = recode(scenario,
                      value_emissions = "bau",
                      ppp_value_emissions = "ppp"
    )
  )
  
## save bau data

ag_bau <- bind_rows(
  county_emissions %>%
    filter(
      sector %in% c("Agriculture"),
      emissions_year <= 2021
    ) %>% # Use any scenario since they're identical
    mutate(scenario = "bau") %>%
    group_by(emissions_year, scenario) %>%
    summarize(value_emissions = sum(value_emissions, na.rm = TRUE), .groups = "keep") %>%
    ungroup(),
  ag_scenarios
)



message("Saving agriculture projections data to: \n\t _meta/data-raw/projections/ag_pathways.rds")
saveRDS(
  ag_bau,
  "_meta/data-raw/projections/ag_pathways.rds"
)


#  base data (2005-2025, identical across scenarios)
base_data <- agriculture_emissions %>%
  filter(emissions_year <= 2022) %>% # Use any scenario since they're identical
  mutate(segment = "base", scenario = "bau") %>%
  # filter(!category %in% c("Electricity", "Building Fuel")) %>%
  group_by(emissions_year) %>%
  summarize(value_emissions = sum(value_emissions)) %>%
  ungroup()

#  diverging scenarios (2026+)
diverging_data <- ag_scenarios %>%
  filter(emissions_year >= 2022) %>%
  mutate(segment = "diverging")

# net_zero_data <- diverging_data %>% filter(scenario == "nz")

bau_data <- diverging_data %>% filter(scenario == "bau")

# PPP data - need to merge with net_zero for the lower bound
ppp_data <- diverging_data %>%
  filter(scenario == "ppp") %>%
  select(emissions_year, value_emissions) %>%
  rename(ppp_emissions = value_emissions)

ag_plot <- plot_emissions_pathways(
  base_data = base_data,
  diverging_data = diverging_data,
  target_value = ag_target,
  target_year = 2050,
  base_cutoff_year = 2022,
  ppp_bau_color = "#A5CF4C",
  y_max = 1.5e6,  # Optional: set max y value
  title = "Agricultural Emissions \n(Millions of CO2-equivalency)"
)

ag_plot

# Create the plot
emissions_gg <- ggplot() +
  # Base fill (2005-2025, gray)
  geom_ribbon(
    data = base_data,
    aes(x = emissions_year, ymin = 0, ymax = value_emissions),
    fill = "gray80", alpha = 0.7
  ) +

  # # Net zero fill (#36454F)
  # geom_ribbon(data = net_zero_data,
  #             aes(x = emissions_year, ymin = 0, ymax = value_emissions),
  #             fill = "#36454F", alpha = 0.3) +

  # PPP fill (from net_zero to ppp)
  geom_ribbon(
    data = ppp_data,
    aes(x = emissions_year, ymin = 0, ymax = ppp_emissions),
    fill = "#8fb910", alpha = 0.5
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
    data = data.frame(emissions_year = 2050, value_emissions = ag_target),
    aes(x = emissions_year, y = value_emissions),
    shape = "*", # asterisk
    size = 12, # make larger or smaller
    stroke = 1.5, # line thickness of the asterisk
    color = "black"
  ) +
  geom_segment(aes(x = 2022, xend = 2022, y = 0, yend = base_data %>% filter(emissions_year == 2022) %>% pull(value_emissions)),
    color = "black", linetype = "solid", linewidth = 0.8
  ) +
  # annotate("text", x = 2025, y = max(your_data$value_emissions) * 0.9,
  #          label = "Historical | Projected", angle = 90, hjust = 1, size = 3.5) +

  # Manual color scale with correct order
  scale_color_manual(
    values = c(
      "Business as usual" = "black",
      # "Net zero" = "#36454F",
      "Potential policy pathways" = "#8fb910"
    ),
    breaks = c("Business as usual", "Potential policy pathways") # Force legend order
  ) +

  # Manual legend guide to show line types
  guides(
    color = guide_legend(
      title = "Scenarios",
      override.aes = list(
        linetype = c("dashed", "solid"),
        color = c("black", "#8fb910")
      )
    )
  ) +
  labs(
    x = "Year",
    y = "",
    title = "Agricultural Emissions \n(Millions of CO2-equivalency)"
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



message("Saving agriculture projections plot to: \n\t ~/imgs/agriculture_decarbonization_pathways.png")
ggplot2::ggsave(
  plot = emissions_gg,
  filename = paste0(here::here(), "/imgs/agriculture_decarbonization_pathways.png"),
  width = 12,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)


### numbers for CCAP document
# sector wide 2030/2050 scenario to BAU comparisons

ag_2030 <- agriculture_emissions_proj %>%
  filter(emissions_year == 2030)

bau2030 <- ag_2030 %>%
  filter(scenario == "bau") %>%
  pull(value_emissions)

ppp2030 <- ag_2030 %>%
  filter(scenario == "ppp") %>%
  pull(value_emissions) -
  bau2030

# nz2030 <- ind_2030 %>% filter(scenario == "nz") %>% pull(value_emissions) -
bau2030

ppp2030
ppp2030 / bau2030
# nz2030 / bau2030

# 2050

ag_2050 <- agriculture_emissions_proj %>%
  filter(emissions_year == 2050)

bau2050 <- ag_2050 %>%
  filter(scenario == "bau") %>%
  pull(value_emissions)

ppp2050 <- ag_2050 %>%
  filter(scenario == "ppp") %>%
  pull(value_emissions) -
  bau2050

nz2050 <- ag_target - bau2050

ppp2050
nz2050
ppp2050 / bau2050
nz2050 / bau2050


message("Finished agriculture projections")
