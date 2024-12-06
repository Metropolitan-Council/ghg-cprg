#### Create CTU temporary inventory graphs for Dec 10 Climate Summit

source("R/_load_pkgs.R")

### directory to save ggplot items in

wd <- "C:/Users/WilfahPA/OneDrive - Metropolitan Council/CPRG/Climate summit graphics/"

### county graphs

cprg_colors <- source("R/cprg_colors.R")

ctu_emissions <- readRDS("_meta/data/ctu_emissions.RDS") %>% 
  filter(emissions_year == 2021) %>% 
  mutate(category = if_else(category == "Natural Gas",
                            str_to_sentence(paste(sector, category)),
                            category))

#city selector - looks for high proportions of individual sectors

city_selector <- ctu_emissions %>% 
  group_by(ctu_name, sector) %>% 
  summarize(value_emissions = sum(emissions_metric_tons_co2e)) %>% 
  ungroup() %>% group_by(ctu_name) %>% 
  # don't use sequestration in calculating total emissions
  mutate(city_emissions = sum(if_else(sector == "Nature", 0, value_emissions)),
         sector_proportion = value_emissions/city_emissions)%>%
  # Filter for cities with at least 6 of the 8 sectors
  filter(n_distinct(sector) >= 6)

ag_city <- city_selector %>% 
  filter(sector == "Agriculture") %>% 
  arrange(desc(sector_proportion))
# Watertown, Belle Plaine, Greenfield, Independence, Waconia

ind_city <- city_selector %>% 
  filter(sector == "Industrial", sector_proportion < 0.5) %>% 
  arrange(desc(sector_proportion))
# Oak Park Heights, Waconia, Anoka, Cottage Grove, Norwood Young America

res_city <- city_selector %>% 
  filter(sector == "Residential") %>% 
  arrange(desc(sector_proportion))
#North Oaks, Shorewood, Circle Pines, Mound, Mahtomedi

comm_city <- city_selector %>% 
  filter(sector == "Commercial") %>% 
  arrange(desc(sector_proportion))
#Osseo, Independence, Minneapolis, Hopkins, Hanover, Stillwater

elec_city <- city_selector %>% 
  filter(sector == "Electricity") %>% 
  arrange(desc(sector_proportion))
# South Saint Paul, Jackson, Bayport, Hanover, Saint Anthony, Spring Lake Park

transport_city <- city_selector %>% 
  filter(sector == "Transportation", city_emissions > 100000) %>% 
  arrange(desc(sector_proportion))
# Columbus, Lino Lakes, East Bethel, Brooklyn Center, Inver Grove Heights, Richfield

nature_city <- city_selector %>% 
  filter(sector == "Nature", city_emissions > 100000) %>% 
  arrange(sector_proportion)
# Columbus, East Bethel, Ham Lake, Medina, Hugo, Lino Lakes

category_colors_vector <- unlist(category_colors, use.names = TRUE)

for(i in c("Cottage Grove")) {
  
  emissions_subsector_ctu <- ctu_emissions %>% 
    filter(ctu_name == i) %>% 
    group_by(emissions_year, sector, category) %>% 
    summarize(MT_CO2e = sum(emissions_metric_tons_co2e)) %>% 
    mutate(sector = factor(sector, levels = c("Electricity", "Transportation", "Residential", "Commercial", "Industrial", "Waste", "Agriculture", "Nature")))
  
  subsector_comparison <- ggplot(
    emissions_subsector_ctu %>%
      mutate(
        category = factor(category, levels = emissions_subsector_ctu %>%
                            filter(emissions_year == 2021) %>%
                            arrange(sector, desc(MT_CO2e)) %>%
                            pull(category) %>%
                            unique())
      ) %>%
      filter(emissions_year == 2021),
    aes(x = sector, y = MT_CO2e / 1000000, fill = category)
  ) +
    geom_bar(stat = 'identity', position = 'stack') +
    labs(fill = "Subsector") +
    scale_fill_manual(values = category_colors_vector) +
    theme_minimal() +
    xlab("") +
    ylab(expression(paste("Million metric tons of ", CO[2], "e"))) +
    theme(
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(size = 14),
      text = element_text(size = 20, family = "sans")
    )
  
  
  print(subsector_comparison)
}

### functionize it 

plot_city_emissions <- function(city, plot_title, file_path) {
  
  # Filter and preprocess data for the specified city
  emissions_subsector_ctu <- ctu_emissions %>%
    filter(ctu_name == city) %>%
    group_by(emissions_year, sector, category) %>%
    summarize(MT_CO2e = sum(emissions_metric_tons_co2e), .groups = "drop") %>%
    mutate(sector = factor(sector, levels = c(
      "Electricity", "Transportation", "Residential", 
      "Commercial", "Industrial", "Waste", 
      "Agriculture", "Nature"
    )))
  
  # Create the plot
  subsector_comparison <- ggplot(
    emissions_subsector_ctu %>%
      mutate(
        category = factor(category, levels = emissions_subsector_ctu %>%
                            filter(emissions_year == 2021) %>%
                            arrange(sector, desc(MT_CO2e)) %>%
                            pull(category) %>%
                            unique())
      ) %>%
      filter(emissions_year == 2021),
    aes(x = sector, y = MT_CO2e / 1e6, fill = category)
  ) +
    geom_bar(stat = "identity", position = "stack") +
    labs(fill = "Subsector") +
    ggtitle(plot_title) +
    scale_fill_manual(values = category_colors_vector) +
    theme_minimal() +
    xlab("") +
    ylab(expression(paste("Million metric tons of ", CO[2], "e"))) +
    theme(
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(size = 14),
      text = element_text(size = 20, family = "sans")
    )
  
  # Save the plot
  ggsave(
    paste0(wd,file_path,".png"),
    plot = subsector_comparison,
    width = 14,
    height = 7,
    units = "in",
    dpi = 400
  )
}

# produce some interesting community profiles

plot_city_emissions("Richfield",
                    "Urban Transportation Profile",
                    file_path = "urban_transport")

plot_city_emissions("Lino Lakes",
                    "Suburban Edge Transportation Profile",
                    file_path = "suburban_transport")

plot_city_emissions("Minneapolis",
                    "Urban Commercial Profile",
                    file_path = "urban_commercial")

plot_city_emissions("Mahtomedi",
                    "Suburban Residential Profile",
                    file_path = "suburban_residential")

plot_city_emissions("Cottage Grove",
                    "Suburban Industrial Profile",
                    file_path = "suburban_industrial")

plot_city_emissions("Belle Plaine",
                    "Rural Agricultural Profile",
                    file_path = "rural_agriculture")

plot_city_emissions("East Bethel",
                    "Rural Natural Systems Profile",
                    file_path = "rural_natural")
