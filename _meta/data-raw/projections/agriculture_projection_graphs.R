### develop agricultural emissions targets based on state GCAM models

source("R/_load_pkgs.R")
source("R/cprg_colors.R")


## load state gcam modeling

gcam <- read_rds("_meta/data/gcam/mpca_subsector_gcam.RDS")
unique(gcam$subsector_mc)

agriculture_emissions <- readRDS(file.path(here::here(), "_meta/data/cprg_county_emissions.RDS")) %>% 
  filter(sector == "Agriculture")

agriculture_scenarios <- gcam %>% 
  filter(sector == "Agriculture",
         scenario %in% c("Current policies without federal support",
                         "CAF Pathway without federal support")
  )

agriculture_emissions_proj <- agriculture_emissions %>% 
  filter(emissions_year == 2020) %>% 
  group_by(source) %>% 
  summarize(baseline_emissions = sum(value_emissions)) %>% 
  mutate(category = case_when(
    grepl("manure", ignore.case = TRUE, source) ~ "Manure management",
    grepl("Enteric fermentation", ignore.case = TRUE, source) ~ "Enteric fermentation",
    TRUE ~ "Cropland soil"
  )) %>% 
  left_join(
    agriculture_scenarios %>% 
      filter(emissions_year >= 2023),
    by = c("category" = "subsector_mc")
  ) %>% 
  mutate(value_emissions = baseline_emissions * proportion_of_2020,
         scenario = case_when(
           scenario == "Current policies without federal support" ~ "bau",
           scenario == "CAF Pathway without federal support" ~ "ppp"
         )) %>% 
  filter(!is.na(value_emissions)) %>% 
  group_by(emissions_year,
           scenario) %>% 
  summarize(value_emissions = sum(value_emissions)) %>% 
  ungroup()


#  base data (2005-2025, identical across scenarios)
base_data <- agriculture_emissions %>%
  filter(emissions_year <= 2025) %>%  # Use any scenario since they're identical
  mutate(segment = "base", scenario = "bau") %>% 
  #filter(!category %in% c("Electricity", "Building Fuel")) %>% 
  group_by(emissions_year) %>% 
  summarize(value_emissions = sum(value_emissions)) %>% 
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

net_zero_for_ppp <- diverging_data %>%
  filter(scenario == "nz") %>%
  select(emissions_year, value_emissions) %>%
  rename(net_zero_emissions = value_emissions)

ppp_ribbon_data <- ppp_data %>%
  left_join(net_zero_for_ppp, by = "emissions_year")

# Create the plot
emissions_gg <- ggplot() +
  # Base fill (2005-2025, gray)
  geom_ribbon(data = base_data,
              aes(x = emissions_year, ymin = 0, ymax = value_emissions),
              fill = "gray80", alpha = 0.7) +
  
  # Net zero fill (#36454F)
  geom_ribbon(data = net_zero_data,
              aes(x = emissions_year, ymin = 0, ymax = value_emissions),
              fill = "#36454F", alpha = 0.3) +
  
  # PPP fill (from net_zero to ppp)
  geom_ribbon(data = ppp_ribbon_data,
              aes(x = emissions_year, ymin = net_zero_emissions, ymax = ppp_emissions),
              fill = "#708090", alpha = 0.5) +
  
  # Base line (2005-2025)
  geom_line(data = base_data,
            aes(x = emissions_year, y = value_emissions),
            color = "black", size = 1) +
  
  # Diverging scenario lines
  geom_line(data = diverging_data %>% filter(scenario == "bau"),
            aes(x = emissions_year, y = value_emissions, color = "Business as usual"),
            linetype = "dashed", size = 1) +
  
  geom_line(data = diverging_data %>% filter(scenario == "ppp"),
            aes(x = emissions_year, y = value_emissions, color = "Accelerated policy pathways"),
            size = 1) +
  
  geom_line(data = diverging_data %>% filter(scenario == "nz"),
            aes(x = emissions_year, y = value_emissions, color = "Net zero"),
            size = 1) +
  
  geom_segment(aes(x = 2025, xend = 2025, y = 0, yend = base_data %>% filter(emissions_year == 2025) %>% pull(value_emissions)),
               color = "black", linetype = "solid", size = 0.8) +
  # annotate("text", x = 2025, y = max(your_data$value_emissions) * 0.9,
  #          label = "Historical | Projected", angle = 90, hjust = 1, size = 3.5) +
  
  # Manual color scale with correct order
  scale_color_manual(
    values = c(
      "Business as usual" = "black",
      "Net zero" = "#36454F",
      "Accelerated policy pathways" = "#708090"
    ),
    breaks = c("Business as usual", "Accelerated policy pathways", "Net zero")  # Force legend order
  ) +
  
  # Manual legend guide to show line types
  guides(
    color = guide_legend(
      title = "Scenarios",
      override.aes = list(
        linetype = c("dashed", "solid", "solid"),
        color = c("black", "#708090", "#36454F")
      )
    )
  ) +
  labs(
    x = "Year",
    y = "",
    title = "Industrial Process Emissions \n(Millions of CO2-equivalency)"
  ) +
  scale_y_continuous(labels = label_number(scale = 1e-6)) +  # convert to millions
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 18),
    axis.text =  element_text(size = 14),
    legend.text = element_text(size = 18),
    legend.key.width = unit(1.2, "cm")
  ) +
  xlim(2005, 2050)

print(emissions_gg)

ggplot2::ggsave(plot = emissions_gg,
                filename = paste0(wd,"/industrial_decarbonization_pathways.png"),  # add your file path here
                width = 12,
                height = 6,
                units = "in",
                dpi = 300,
                bg = "white")

### numbers for CCAP document
# sector wide 2030/2050 scenario to BAU comparisons

ind_2030 <- industrial_emissions_proj %>% 
  filter(emissions_year == 2030) 

bau2030 <- ind_2030 %>% filter(scenario == "bau") %>% pull(value_emissions)

ppp2030 <- ind_2030 %>% filter(scenario == "ppp") %>% pull(value_emissions) -
  bau2030

nz2030 <- ind_2030 %>% filter(scenario == "nz") %>% pull(value_emissions) -
  bau2030

ppp2030 / bau2030
nz2030 / bau2030

#2050

ind_2050 <- industrial_emissions_proj %>% 
  filter(emissions_year == 2050) 

bau2050 <- ind_2050 %>% filter(scenario == "bau") %>% pull(value_emissions)

ppp2050 <- ind_2050 %>% filter(scenario == "ppp") %>% pull(value_emissions) -
  bau2050

nz2050 <- ind_2050 %>% filter(scenario == "nz") %>% pull(value_emissions) -
  bau2050

ppp2050
nz2050
ppp2050 / bau2050
nz2050 / bau2050
