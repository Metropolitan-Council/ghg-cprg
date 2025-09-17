### develop commercial emissions targets based on forecasted job growth state GCAM models


## restart and rerun when making updates to ghg.ccap@ccap-graphics
# remotes::install_github("Metropolitan-Council/ghg.ccap@ccap-graphics")
# enter 3 ('none') if prompted to update other packages

source("R/_load_pkgs.R")
source("R/cprg_colors.R")

interpolate_emissions <- function(df) {
  df %>%
    mutate(emissions_year = as.numeric(emissions_year)) %>%
    group_by(scenario) %>%
    # complete sequence of years from min to max
    complete(emissions_year = seq(min(emissions_year), max(emissions_year), by = 1)) %>%
    # interpolate missing values linearly
    mutate(value_emissions = approx(emissions_year, value_emissions, 
                                    xout = emissions_year, rule = 1)$y) %>%
    ungroup()
}

## load state gcam modeling

gcam <- read_rds("_meta/data/gcam/mpca_subsector_gcam.RDS")
unique(gcam$subsector_mc)

nonres_scenarios <- gcam %>% 
  filter(subsector_mc == "Commercial natural gas",
         scenario %in% c("PPP after Fed RB",
                         "CP after Fed RB")
  )

# elec_scenarios <- gcam %>% 
#   filter(sector == "Electricity",
#          scenario == "PPP after Fed RB")

nonres_targets <- nonres_scenarios %>% 
  filter(emissions_year %in% c(2030, 2050),
)

### set net-zero by regional analysis

county_emissions <- readRDS(file.path(here::here(), "_meta/data/cprg_county_emissions.RDS"))

seq_target <- readRDS(file.path(here::here(), "_meta/data/regional_net_zero_target.RDS")) %>% 
  pull(net_zero_target)


nonres_emissions <- county_emissions %>% 
  filter((sector == "Commercial" & category != "Electricity") |
         (sector == "Industrial" & category == "Building Fuel")
  )

### nonres target - based on natural gas emissions, not electricity
nonres_target <- (nonres_emissions %>% 
  filter(emissions_year == 2022) %>%
  pull(value_emissions) %>% sum() /  #residential natural gas emissions
  county_emissions %>% 
  filter(emissions_year == 2022,
         category != "Electricity") %>%
  pull(value_emissions) %>% sum()) * #regional wide emissions minus electricity
  seq_target * -1 # emissions goal

### project commercial growth based on job shares

total_jobs <- read_rds("_meta/data/demographic_forecast_11_county.RDS") %>% 
  filter(variable == "total_jobs")

emissions_per_job <- total_jobs %>% 
  filter(inventory_year == 2022) %>% 
  left_join(nonres_emissions %>% 
              filter(emissions_year == 2022) %>% 
              group_by(county_name) %>% 
              summarise(value_emissions = sum(value_emissions)) %>% 
              ungroup(),
            by = "county_name") %>% 
  mutate(emissions_per_job = value_emissions / value)

nonres_emissions_bau <- total_jobs %>% 
  filter(inventory_year >= 2022) %>% 
  left_join(emissions_per_job %>% 
              select(county_name, emissions_per_job),
            by = "county_name") %>% 
  mutate(value_emissions = value * emissions_per_job,
         scenario = "bau") %>% 
  rename(emissions_year = inventory_year) %>% 
  group_by(emissions_year, scenario) %>% 
  summarize(value_emissions = sum(value_emissions))

#nonres_emissions_forecast <- interpolate_emissions(nonres_emissions_forecast)

nonres_scenarios <- gcam %>%
  filter(subsector_mc %in% c("Commercial natural gas",
                             "Industrial natural gas",
                             "Commercial fuel combustion"),
         scenario %in% c("PPP after Fed RB",
                         "CP after Fed RB")
  ) %>% 
  mutate(subsector_mc = if_else(grepl("natural", subsector_mc),
                                "Building Fuel",
                                subsector_mc))

nonres_emissions_proj <- nonres_emissions %>% 
  filter(emissions_year == 2020) %>% 
  group_by(sector, category) %>% 
  summarize(baseline_emissions = sum(value_emissions, na.rm = "TRUE")) %>% 
  left_join(
    nonres_scenarios %>% 
      filter(emissions_year >= 2025),
    by = c("sector",
      "category" = "subsector_mc")
  ) %>% 
  mutate(value_emissions = baseline_emissions * proportion_of_2020,
         scenario = case_when(
           scenario == "CP after Fed RB" ~ "bau",
           scenario == "PPP after Fed RB" ~ "ppp"
         ),
         emissions_year = as.numeric(emissions_year)) %>% 
  filter(!is.na(value_emissions)) %>% 
  group_by(emissions_year,
           scenario) %>% 
  summarize(value_emissions = sum(value_emissions)) %>% 
  ungroup()

nonres_emissions_proj <- nonres_emissions %>% 
  filter(emissions_year == 2020) %>% 
  group_by(sector, category) %>% 
  summarize(baseline_emissions = sum(value_emissions, na.rm = "TRUE")) %>% 
  left_join(
    nonres_scenarios %>% 
      filter(emissions_year >= 2025),
    by = c("sector",
           "category" = "subsector_mc")
  ) %>% 
  mutate(value_emissions = baseline_emissions * proportion_of_2020,
         scenario = case_when(
           scenario == "CP after Fed RB" ~ "bau",
           scenario == "PPP after Fed RB" ~ "ppp"
         ),
         emissions_year = as.numeric(emissions_year)) %>% 
  filter(!is.na(value_emissions)) %>% 
  group_by(emissions_year,
           scenario) %>% 
  summarize(value_emissions = sum(value_emissions)) %>% 
  ungroup()


nonres_emissions_pathways_natgas <- interpolate_emissions(bind_rows(
  nonres_emissions_bau,
  nonres_emissions_proj %>% 
    filter(scenario == "ppp")
))

## save bau data

nonres_ng_pathways <- bind_rows(
  county_emissions %>%
    filter(sector %in% c("Commercial",
                         "Industrial"),
           category %in% c("Building Fuel"),
           emissions_year <= 2021) %>%  # Use any scenario since they're identical
    mutate(scenario = "bau") %>% 
    group_by(emissions_year, scenario) %>% 
    summarize(value_emissions = sum(value_emissions, na.rm = TRUE)) %>% 
    ungroup(),
  nonres_emissions_pathways_natgas
)

# saveRDS(nonres_ng_pathways,
#   "_meta/data-raw/projections/nonres_ng_pathways.rds")

### electricity emissions ####


nonres_elec_emissions <- county_emissions %>% 
  filter((sector == "Commercial" & category == "Electricity") |
           (sector == "Industrial" & category == "Electricity")
  ) %>% 
  left_join(ghg.ccap::grid_emissions,
            by = c("emissions_year" = "inventory_year")) %>% 
  mutate(mwh = value_emissions / mt_co2e_per_mwh)



# base this on 2020 to match to state elec decarb values
mwh_per_job <- total_jobs %>% 
  filter(inventory_year == 2022) %>% 
  left_join(nonres_elec_emissions %>% 
              filter(emissions_year == 2022) %>% 
              group_by(county_name) %>% 
              summarise(mwh = sum(mwh)) %>% 
              ungroup(),
            by = "county_name") %>% 
  mutate(mwh_per_job = mwh / value)

nonres_elec_emissions_bau <- total_jobs %>% 
  filter(inventory_year >= 2022) %>% 
  left_join(mwh_per_job %>% 
              select(county_name, mwh_per_job),
            by = "county_name") %>%
  left_join(ghg.ccap::grid_emissions,
            by = "inventory_year") %>% 
  mutate(value_emissions = value * mwh_per_job * mt_co2e_per_mwh,
         scenario = "bau") %>% 
  rename(emissions_year = inventory_year) %>% 
  group_by(emissions_year, scenario) %>% 
  summarize(value_emissions = sum(value_emissions))

nonres_elec_bau <- bind_rows(
  county_emissions %>%
    filter(sector %in% c("Commercial",
                         "Industrial"),
           category %in% c("Electricity"),
           emissions_year <= 2021) %>%  # Use any scenario since they're identical
    mutate(scenario = "bau") %>% 
    group_by(emissions_year, scenario) %>% 
    summarize(value_emissions = sum(value_emissions, na.rm = TRUE)) %>% 
    ungroup(),
  interpolate_emissions(nonres_elec_emissions_bau)
)

# saveRDS(nonres_elec_bau,
#   "_meta/data-raw/projections/nonres_elec_bau.rds")

nonres_emissions_pathways <- left_join(
  nonres_emissions_pathways_natgas,
  interpolate_emissions(nonres_elec_emissions_bau) %>% 
    select(emissions_year, elec_emissions = value_emissions),
  by = "emissions_year"
) %>% 
  mutate(value_emissions = value_emissions + elec_emissions) %>% 
  select(-elec_emissions)


### graph it!!####

#  base data (2005-2025, identical across scenarios)
base_data <- county_emissions %>%
  filter(sector %in% c("Commercial",
                       "Industrial"),
         category %in% c("Electricity",
                         "Building Fuel"),
    emissions_year <= 2022) %>%  # Use any scenario since they're identical
  mutate(segment = "base", scenario = "bau") %>% 
  group_by(emissions_year) %>% 
  summarize(value_emissions = sum(value_emissions, na.rm = TRUE)) %>% 
  ungroup()

bau_2025 <- nonres_emissions_pathways %>%
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
diverging_data <- nonres_emissions_pathways %>%
  filter(emissions_year >= 2022) %>%
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
  geom_ribbon(data = base_data,
              aes(x = emissions_year, ymin = 0, ymax = value_emissions),
              fill = "gray80", alpha = 0.7) +
  
  # Net zero fill (#36454F)
  # geom_ribbon(data = net_zero_data,
  #             aes(x = emissions_year, ymin = 0, ymax = value_emissions),
  #             fill = "#36454F", alpha = 0.3) +
  
  # PPP fill (from net_zero to ppp)
  geom_ribbon(data = ppp_data,
              aes(x = emissions_year, ymin = 0, ymax = ppp_emissions),
              fill = "#DB5755", alpha = 0.5) +
  
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
    data = data.frame(emissions_year = 2050, value_emissions = nonres_target),
    aes(x = emissions_year, y = value_emissions),
    shape = "*",    # asterisk
    size = 12,     # make larger or smaller
    stroke = 1.5, # line thickness of the asterisk
    color = "black"
  ) +
  
  geom_segment(aes(x = 2025, xend = 2025, y = 0, yend = base_data %>% filter(emissions_year == 2025) %>% pull(value_emissions)),
               color = "black", linetype = "solid", size = 0.8) +
  # annotate("text", x = 2025, y = max(your_data$value_emissions) * 0.9,
  #          label = "Historical | Projected", angle = 90, hjust = 1, size = 3.5) +
  
  # Manual color scale with correct order
  scale_color_manual(
    values = c(
      "Business as usual" = "black",
      # "Net zero" = "#36454F",
      "Potential policy pathways" = "#DB5755"
    ),
    breaks = c("Business as usual", "Potential policy pathways")  # Force legend order
  ) +
  
  # Manual legend guide to show line types
  guides(
    color = guide_legend(
      title = "Scenarios",
      override.aes = list(
        linetype = c("dashed", "solid"),
        color = c("black", "#DB5755")
      )
    )
  ) +
  labs(
    x = "Year",
    y = "",
    title = "Non-residential Building Emissions \n(Millions of CO2-equivalency)"
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
                filename = paste0(wd,"/nonres_decarbonization_pathways.png"),  # add your file path here
                width = 12,
                height = 6,
                units = "in",
                dpi = 300,
                bg = "white")

### numbers for CCAP document
# sector wide 2030/2050 scenario to BAU comparisons

nonres_2030 <- nonres_emissions_pathways %>% 
  filter(emissions_year == 2030) 

bau2030 <- nonres_2030 %>% filter(scenario == "bau") %>% pull(value_emissions)

ppp2030 <- nonres_2030 %>% filter(scenario == "ppp") %>% pull(value_emissions) -
  bau2030

nz2030 <- nonres_2030 %>% filter(scenario == "nz") %>% pull(value_emissions) -
  bau2030

ppp2030
ppp2030 / bau2030
nz2030 / bau2030

#2050

nonres_2050 <- nonres_emissions_pathways %>% 
  filter(emissions_year == 2050) 

bau2050 <- nonres_2050 %>% filter(scenario == "bau") %>% pull(value_emissions)

ppp2050 <- nonres_2050 %>% filter(scenario == "ppp") %>% pull(value_emissions) -
  bau2050

nz2050 <- nonres_target - bau2050

ppp2050
nz2050
ppp2050 / bau2050
nz2050 / bau2050
