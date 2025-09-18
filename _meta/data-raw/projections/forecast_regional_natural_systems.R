rm(list = ls())



remotes::install_github("Metropolitan-Council/ghg.ccap", ref = "ccap-graphics")
source("R/_load_pkgs.R")

# library(ghg.ccap)

# Load data
natural_systems_data <- c()

natural_systems_data$land_cover_carbon <- readr::read_rds(paste0(here::here(), "/_nature/data/", "land_cover_carbon.rds"))


lc_county <- readr::read_rds(paste0(here::here(), "/_nature/data/", "nlcd_county_landcover_allyrs.rds")) %>%
  mutate(
    geog_name = paste(county_name, "County"),
    geog_id = county_id,
    ctu_class = "COUNTY"
  ) %>%
  ungroup() %>%
  group_by(geog_name, ctu_class, geog_id, inventory_year, land_cover_type) %>%
  dplyr::summarize(area = sum(area), .groups = "keep") %>%
  ungroup()


inventory_start_year <- 2005
inventory_end_year <- 2022
future_years <- 2023:2050




natural_systems_data$county$inventory <- lc_county %>%
  filter(inventory_year %in% seq(inventory_start_year, inventory_end_year, by = 1)) %>%
  # dplyr::select(geog_name, ctu_class, geog_id, inventory_year, land_cover_type, area) %>%
  pivot_wider(names_from = land_cover_type, values_from = area) %>%
  rowwise() %>%
  mutate(TOTAL = rowSums(across(c(
    Bare, Developed_Low, Developed_Med, Developed_High,
    Urban_Grassland, Urban_Tree,
    Cropland, Grassland, Tree, Water,
    Wetland
  )), na.rm = T)) %>%
  ungroup() %>%
  # replace NAs with 0
  mutate(across(everything(), ~ tidyr::replace_na(., 0)))


county_projections_2022 <- natural_systems_data$county$inventory %>%
  filter(inventory_year == inventory_end_year) %>%
  select(-c(inventory_year, TOTAL)) %>%
  pivot_longer(cols = -c(geog_name, geog_id, ctu_class), names_to = "land_cover_type", values_to = "area")


county_projections_null <- county_projections_2022 %>%
  tidyr::crossing(inventory_year = future_years) %>%
  pivot_wider(names_from = "land_cover_type", values_from = "area") %>%
  rowwise() %>%
  mutate(TOTAL = sum(dplyr::c_across(c(
    Bare, Developed_Low, Developed_Med, Developed_High,
    Urban_Grassland, Urban_Tree,
    Cropland, Grassland, Tree, Water,
    Wetland
  )), na.rm = T)) %>%
  ungroup() %>%
  # replace NAs with 0
  mutate(across(everything(), ~ tidyr::replace_na(., 0)))










# Regional inventory by summing county inventories
natural_systems_data$regional$inventory <- natural_systems_data$county$inventory %>%
  group_by(inventory_year) %>%
  summarize(
    Bare = sum(Bare),
    Developed_Low = sum(Developed_Low),
    Developed_Med = sum(Developed_Med),
    Developed_High = sum(Developed_High),
    Urban_Grassland = sum(Urban_Grassland),
    Urban_Tree = sum(Urban_Tree),
    Cropland = sum(Cropland),
    Grassland = sum(Grassland),
    Tree = sum(Tree),
    Water = sum(Water),
    Wetland = sum(Wetland),
    TOTAL = sum(TOTAL)
  ) %>%
  mutate(
    geog_name = "Regional",
    ctu_class = "REGION",
    geog_id = "00000000"
  ) %>%
  dplyr::select(
    geog_name, ctu_class, geog_id, inventory_year,
    Bare, Cropland, Developed_High, Developed_Low, Developed_Med,
    Grassland, Tree, Urban_Grassland, Urban_Tree, Water, Wetland, TOTAL
  )


regional_projections_2022 <- natural_systems_data$regional$inventory %>%
  filter(inventory_year == inventory_end_year) %>%
  select(-c(inventory_year, TOTAL)) %>%
  pivot_longer(cols = -c(geog_name, geog_id, ctu_class), names_to = "land_cover_type", values_to = "area")



natural_systems_data$regional$null_projections <- regional_projections_2022 %>%
  tidyr::crossing(inventory_year = future_years) %>%
  pivot_wider(names_from = "land_cover_type", values_from = "area") %>%
  rowwise() %>%
  mutate(TOTAL = sum(dplyr::c_across(c(
    Bare, Developed_Low, Developed_Med, Developed_High,
    Urban_Grassland, Urban_Tree,
    Cropland, Grassland, Tree, Water,
    Wetland
  )), na.rm = T)) %>%
  ungroup() %>%
  # replace NAs with 0
  mutate(across(everything(), ~ tidyr::replace_na(., 0)))





mod_bau <- ghg.ccap::run_scenario_natural_systems(
  .selected_ctu = "Regional",
  tb_inv = natural_systems_data$regional$inventory,
  tb_future = natural_systems_data$regional$null_projections,
  tb_seq = natural_systems_data$land_cover_carbon,
  .enviro_factors = ghg.ccap::enviro_factors,
)

# Scenario 1: Reforest all barren land, 5% of cropland, and 10% of grassland
scen1_urbanTree_2050 <- 10
scen1_cropland_2050 <- 5
scen1_bare_2050 <- 100
scen1_grassland_2050 <- 10

mod_scen1 <- ghg.ccap::run_scenario_natural_systems(
  .selected_ctu = "Regional",
  tb_inv = natural_systems_data$regional$inventory,
  tb_future = natural_systems_data$regional$null_projections,
  tb_seq = natural_systems_data$land_cover_carbon,
  .enviro_factors = ghg.ccap::enviro_factors,

  # user inputs here!
  .urban_tree_start = 2025,
  .urban_tree_time = 25,
  .urban_tree_area_perc = scen1_urbanTree_2050,
  # .l2l_start = input$lawnsToLegumes_start_yr,
  # .l2l_time = input$lawnsToLegumes_comp_time,
  # .l2l_area_perc = input$lawnsToLegumes_area_pct,
  .grassland_area_perc = scen1_grassland_2050,
  .cropland_area_perc = scen1_cropland_2050,
  .bare_area_perc = scen1_bare_2050,
  .restoration_start = 2025,
  .restoration_time = 25,
)


# Scenario 2: Reforest all barren land, 10% of cropland, and 20% of grassland
scen2_urbanTree_2050 <- 10
scen2_cropland_2050 <- 10
scen2_bare_2050 <- 100
scen2_grassland_2050 <- 20

mod_scen2 <- ghg.ccap::run_scenario_natural_systems(
  .selected_ctu = "Regional",
  tb_inv = natural_systems_data$regional$inventory,
  tb_future = natural_systems_data$regional$null_projections,
  tb_seq = natural_systems_data$land_cover_carbon,
  .enviro_factors = ghg.ccap::enviro_factors,

  # user inputs here!
  .urban_tree_start = 2025,
  .urban_tree_time = 25,
  .urban_tree_area_perc = scen2_urbanTree_2050,
  # .l2l_start = input$lawnsToLegumes_start_yr,
  # .l2l_time = input$lawnsToLegumes_comp_time,
  # .l2l_area_perc = input$lawnsToLegumes_area_pct,
  .grassland_area_perc = scen2_grassland_2050,
  .cropland_area_perc = scen2_cropland_2050,
  .bare_area_perc = scen2_bare_2050,
  .restoration_start = 2025,
  .restoration_time = 25,
)


df_netZero <- mod_scen2 %>%
  filter(inventory_year == 2050) %>%
  group_by(geog_name, ctu_class, geog_id, inventory_year) %>%
  summarize(
    net_zero_target = sum(value_emissions, na.rm = T),
    .groups = "keep"
  )


target_seq_for_netZero <- df_netZero %>% pull(net_zero_target)



#
# write_rds(df_netZero,
#           "_meta/data/regional_net_zero_target.RDS")



# Plotting function -------------------------------------------------------
plot_emissions <- function(bau, scenario, target) {
  # Aggregate helper
  agg <- function(df) {
    df %>%
      filter(!is.na(value_emissions)) %>%
      group_by(inventory_year) %>%
      summarize(total_emissions = sum(value_emissions, na.rm = TRUE), .groups = "drop")
  }

  scenario_agg <- agg(scenario)
  bau_agg <- agg(bau)

  ggplot() +
    # Base fill (2005–2025, gray)
    geom_ribbon(
      data = scenario_agg %>% filter(inventory_year <= 2025),
      aes(x = inventory_year, ymin = 0, ymax = total_emissions),
      fill = "gray80", alpha = 0.7
    ) +

    # Scenario fill (lightgreen, 2025+)
    geom_ribbon(
      data = scenario_agg %>% filter(inventory_year >= 2025),
      aes(x = inventory_year, ymin = 0, ymax = total_emissions),
      fill = "lightgreen", alpha = 0.5
    ) +

    # Base line (2005–2025)
    geom_line(
      data = scenario_agg %>% filter(inventory_year <= 2025),
      aes(x = inventory_year, y = total_emissions),
      color = "black", linewidth = 1
    ) +

    # Diverging scenario lines
    geom_line(
      data = bau_agg %>% filter(inventory_year >= 2025),
      aes(x = inventory_year, y = total_emissions, color = "Business as usual"),
      linetype = "dashed", linewidth = 1
    ) +
    geom_line(
      data = scenario_agg %>% filter(inventory_year >= 2025),
      aes(x = inventory_year, y = total_emissions, color = "Potential policy pathways"),
      linewidth = 1
    ) +

    # 2050 target point
    geom_point(
      data = data.frame(emissions_year = 2050, value_emissions = target),
      aes(x = emissions_year, y = value_emissions),
      shape = "*", size = 12, stroke = 1.5, color = "black"
    ) +

    # Divider at 2025
    geom_segment(
      aes(
        x = 2025, xend = 2025, y = 0,
        yend = scenario_agg %>% filter(inventory_year == 2025) %>% pull(total_emissions)
      ),
      color = "black", linetype = "solid", linewidth = 0.8
    ) +

    # Manual color scale
    scale_color_manual(
      values = c(
        "Business as usual" = "black",
        "Potential policy pathways" = "lightgreen"
      ),
      breaks = c("Business as usual", "Potential policy pathways", "Net zero")
    ) +
    guides(
      color = guide_legend(
        title = "Scenarios",
        override.aes = list(
          linetype = c("dashed", "solid"),
          color = c("black", "lightgreen")
        )
      )
    ) +
    labs(
      x = "Year",
      y = "",
      title = "Sequestration by Natural Systems \n(Millions of CO2-equivalency)"
    ) +
    scale_y_continuous(labels = label_number(scale = 1e-6)) +
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
}












scen1_gg <- plot_emissions(
  bau = mod_bau,
  scenario = mod_scen1,
  target = target_seq_for_netZero
)

print(scen1_gg)



scen2_gg <- plot_emissions(
  bau = mod_bau,
  scenario = mod_scen2,
  target = target_seq_for_netZero
)

print(scen2_gg)


ggplot2::ggsave(
  plot = scen1_gg,
  filename = paste0(here::here(), "/imgs/ns_decarbonization_pathways.png"), # add your file path here
  width = 12,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)






# Final output and summary ------------------------------------------------



regional_ns_forecast <- rbind(
  # business as usual
  mod_bau %>%
    filter(!is.na(value_emissions)) %>%
    group_by(inventory_year) %>%
    summarize(total_emissions = sum(value_emissions, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      scenario = "bau",
      geog_name = "Regional",
      geog_class = "REGION",
      geog_id = "00000000",
      sector = "Natural Systems"
    ) %>%
    dplyr::select(
      geog_name, geog_class, geog_id, sector,
      inventory_year,
      scenario, total_emissions
    ),

  # potential policy pathways
  mod_scen1 %>%
    filter(!is.na(value_emissions)) %>%
    group_by(inventory_year) %>%
    summarize(total_emissions = sum(value_emissions, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      scenario = "ppp",
      geog_name = "Regional",
      geog_class = "REGION",
      geog_id = "00000000",
      sector = "Natural Systems"
    ) %>%
    dplyr::select(
      geog_name, geog_class, geog_id, sector,
      inventory_year,
      scenario, total_emissions
    ),

  # net zero
  mod_scen2 %>%
    filter(!is.na(value_emissions)) %>%
    group_by(inventory_year) %>%
    summarize(total_emissions = sum(value_emissions, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      scenario = "nz",
      geog_name = "Regional",
      geog_class = "REGION",
      geog_id = "00000000",
      sector = "Natural Systems"
    ) %>%
    dplyr::select(
      geog_name, geog_class, geog_id, sector,
      inventory_year,
      scenario, total_emissions
    )
)

#
# write_rds(regional_ns_forecast,
#           "_meta/data/regional_ns_forecast.RDS")

### numbers for CCAP document
# sector wide 2030/2050 scenario to BAU comparisons

ns_2030 <- regional_ns_forecast %>%
  filter(inventory_year == 2030)

bau2030 <- ns_2030 %>%
  filter(scenario == "bau") %>%
  pull(total_emissions)

ppp2030 <- ns_2030 %>%
  filter(scenario == "ppp") %>%
  pull(total_emissions) -
  bau2030


ppp2030
ppp2030 / bau2030

# 2050

ns_2050 <- regional_ns_forecast %>%
  filter(inventory_year == 2050)

bau2050 <- ns_2050 %>%
  filter(scenario == "bau") %>%
  pull(total_emissions)

ppp2050 <- ns_2050 %>%
  filter(scenario == "ppp") %>%
  pull(total_emissions) -
  bau2050

nz2050 <- ns_2050 %>%
  filter(scenario == "nz") %>%
  pull(total_emissions) -
  bau2050

ppp2050
nz2050
ppp2050 / bau2050
nz2050 / bau2050
