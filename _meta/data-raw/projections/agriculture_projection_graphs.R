### develop agricultural emissions targets based on state GCAM models

source("R/_load_pkgs.R")
source("R/cprg_colors.R")


## load state gcam modeling

gcam <- read_rds("_meta/data/gcam/mpca_subsector_gcam.RDS")
unique(gcam$subsector_mc)


### set net-zero by regional analysis

county_emissions <- readRDS(file.path(here::here(), "_meta/data/cprg_county_emissions.RDS"))

## to be updated once we have better sequestration growth potential
seq_target <- readRDS(file.path(here::here(), "_meta/data/regional_net_zero_target.RDS")) %>% 
  pull(net_zero_target)

### agricultural target
ag_target <- county_emissions %>% 
  filter(emissions_year == 2022,
         sector == "Agriculture") %>%
  pull(value_emissions) %>% sum() /  #residential natural gas emissions
  county_emissions %>% 
  filter(emissions_year == 2022,
         category != "Electricity") %>%
  pull(value_emissions) %>% sum() * #regional wide emissions minus electricity
  seq_target * -1 # emissions goal

agriculture_emissions <- county_emissions %>% 
  filter(sector == "Agriculture")

agriculture_scenarios <- gcam %>% 
  filter(sector == "Agriculture",
         scenario %in% c("Current policies without federal support",
                         "CAF Pathway without federal support")
  )

agriculture_emissions_proj <- agriculture_emissions %>% 
  filter(emissions_year == 2022) %>% 
  mutate(category = case_when(
    grepl("manure", ignore.case = TRUE, source) ~ "Manure management",
    grepl("Enteric fermentation", ignore.case = TRUE, source) ~ "Enteric fermentation",
    TRUE ~ "Cropland soil"
  )) %>% 
  group_by(category) %>% 
  summarize(baseline_emissions = sum(value_emissions)) %>% 
  left_join(
    agriculture_scenarios %>% 
      filter(emissions_year >= 2022),
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
  ungroup() %>% 
  mutate(emissions_year = as.numeric(emissions_year))


#  base data (2005-2025, identical across scenarios)
base_data <- agriculture_emissions %>%
  filter(emissions_year <= 2022) %>%  # Use any scenario since they're identical
  mutate(segment = "base", scenario = "bau") %>% 
  #filter(!category %in% c("Electricity", "Building Fuel")) %>% 
  group_by(emissions_year) %>% 
  summarize(value_emissions = sum(value_emissions)) %>% 
  ungroup()

#  diverging scenarios (2026+)
diverging_data <- agriculture_emissions_proj %>%
  filter(emissions_year >= 2022) %>%
  mutate(segment = "diverging")

#net_zero_data <- diverging_data %>% filter(scenario == "nz")

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
  geom_ribbon(data = base_data,
              aes(x = emissions_year, ymin = 0, ymax = value_emissions),
              fill = "gray80", alpha = 0.7) +
  
  # # Net zero fill (#36454F)
  # geom_ribbon(data = net_zero_data,
  #             aes(x = emissions_year, ymin = 0, ymax = value_emissions),
  #             fill = "#36454F", alpha = 0.3) +
  
  # PPP fill (from net_zero to ppp)
  geom_ribbon(data = ppp_data,
              aes(x = emissions_year, ymin = 0, ymax = ppp_emissions),
              fill = "#8fb910", alpha = 0.5) +
  
  # Base line (2005-2025)
  geom_line(data = base_data,
            aes(x = emissions_year, y = value_emissions),
            color = "black", size = 1) +
  
  # Diverging scenario lines
  geom_line(data = diverging_data %>% filter(scenario == "bau"),
            aes(x = emissions_year, y = value_emissions, color = "Business as usual"),
            linetype = "dashed", size = 1) +
  
  geom_line(data = diverging_data %>% filter(scenario == "ppp"),
            aes(x = emissions_year, y = value_emissions, color = "Potential policy pathways"),
            size = 1) +
  
  geom_point(
    data = data.frame(emissions_year = 2050, value_emissions = ag_target),
    aes(x = emissions_year, y = value_emissions),
    shape = "*",    # asterisk
    size = 12,     # make larger or smaller
    stroke = 1.5, # line thickness of the asterisk
    color = "black"
  ) +
  
  geom_segment(aes(x = 2022, xend = 2022, y = 0, yend = base_data %>% filter(emissions_year == 2022) %>% pull(value_emissions)),
               color = "black", linetype = "solid", size = 0.8) +
  # annotate("text", x = 2025, y = max(your_data$value_emissions) * 0.9,
  #          label = "Historical | Projected", angle = 90, hjust = 1, size = 3.5) +
  
  # Manual color scale with correct order
  scale_color_manual(
    values = c(
      "Business as usual" = "black",
      # "Net zero" = "#36454F",
      "Potential policy pathways" ="#8fb910"
    ),
    breaks = c("Business as usual", "Potential policy pathways")  # Force legend order
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
                filename = paste0(wd,"/agriculture_decarbonization_pathways.png"),  # add your file path here
                width = 12,
                height = 6,
                units = "in",
                dpi = 300,
                bg = "white")

### numbers for CCAP document
# sector wide 2030/2050 scenario to BAU comparisons

ag_2030 <- agriculture_emissions_proj %>% 
  filter(emissions_year == 2030) 

bau2030 <- ag_2030 %>% filter(scenario == "bau") %>% pull(value_emissions)

ppp2030 <- ag_2030 %>% filter(scenario == "ppp") %>% pull(value_emissions) -
  bau2030

# nz2030 <- ind_2030 %>% filter(scenario == "nz") %>% pull(value_emissions) -
  bau2030

  ppp2030
ppp2030 / bau2030
# nz2030 / bau2030

#2050

ag_2050 <- agriculture_emissions_proj %>% 
  filter(emissions_year == 2050) 

bau2050 <- ag_2050 %>% filter(scenario == "bau") %>% pull(value_emissions)

ppp2050 <- ag_2050 %>% filter(scenario == "ppp") %>% pull(value_emissions) -
  bau2050

nz2050 <- ag_target - bau2050

ppp2050
nz2050
ppp2050 / bau2050
nz2050 / bau2050
