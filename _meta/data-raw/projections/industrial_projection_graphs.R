### develop industrial emissions targets based on state GCAM models

source("R/_load_pkgs.R")
source("R/cprg_colors.R")


## load state gcam modeling

gcam <- read_rds("_meta/data/gcam/mpca_subsector_gcam.RDS")
unique(gcam$subsector_mc)

industrial_emissions <- readRDS(file.path(here::here(), "_meta/data/cprg_county_emissions.RDS")) %>% 
  filter(sector == "Industrial")

industrial_scenarios <- gcam %>% 
  filter(subsector_mc %in% c("Industrial fuel combustion",
                             "Industrial processes",
                             "Refinery processes"),
         scenario %in% c("Net-Zero Pathway",
                         "PPP after Fed RB",
                         "CP after Fed RB")
  )

industrial_emissions_proj <- industrial_emissions %>% 
  filter(emissions_year == 2020) %>% 
  group_by(category) %>% 
  summarize(baseline_emissions = sum(value_emissions)) %>% 
  left_join(
    industrial_scenarios %>% 
      filter(emissions_year >= 2025),
    by = c("category" = "subsector_mc")
  ) %>% 
  mutate(value_emissions = baseline_emissions * proportion_of_2020,
         scenario = case_when(
           scenario == "CP after Fed RB" ~ "bau",
           scenario == "PPP after Fed RB" ~ "ppp",
           scenario == "Net-Zero Pathway" ~ "nz"
         ))
  
  


#  base data (2005-2025, identical across scenarios)
base_data <- industrial_emissions %>%
  filter(emissions_year <= 2025) %>%  # Use any scenario since they're identical
  mutate(segment = "base", scenario = "bau")

#  diverging scenarios (2026+)
diverging_data <- industrial_emissions_proj %>%
  filter(emissions_year >= 2025) %>%
  mutate(segment = "diverging")

net_zero_data <- diverging_data %>% filter(scenario == "nz")


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
  
  # Net zero fill (maroon)
  geom_ribbon(data = net_zero_data,
              aes(x = emissions_year, ymin = 0, ymax = value_emissions),
              fill = "maroon", alpha = 0.3) +
  
  # PPP fill (from net_zero to ppp)
  geom_ribbon(data = ppp_ribbon_data,
              aes(x = emissions_year, ymin = net_zero_emissions, ymax = ppp_emissions),
              fill = "rosybrown1", alpha = 0.5) +
  
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
      "Net zero" = "maroon",
      "Accelerated policy pathways" = "rosybrown3"
    ),
    breaks = c("Business as usual", "Accelerated policy pathways", "Net zero")  # Force legend order
  ) +
  
  # Manual legend guide to show line types
  guides(
    color = guide_legend(
      title = "Scenarios",
      override.aes = list(
        linetype = c("dashed", "solid", "solid"),
        color = c("black", "rosybrown3", "maroon")
      )
    )
  ) +
  labs(
    x = "Year",
    y = "",
    title = "Residential Building Emissions \n(Millions of CO2-equivalency)"
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
                filename = paste0(wd,"/residential_decarbonization_pathways.png"),  # add your file path here
                width = 12,
                height = 6,
                units = "in",
                dpi = 300,
                bg = "white")
