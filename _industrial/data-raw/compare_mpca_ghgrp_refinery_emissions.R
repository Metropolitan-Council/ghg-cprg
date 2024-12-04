flight_emissions <- readRDS(file.path(here::here(), "_industrial/data/flight_industrial_point_sources_ctu.RDS"))
subpart_c_emissions <- readRDS(file.path(here::here(), "_industrial/data/fuel_combustion_emissions.RDS"))
mpca_emissions <- readRDS(file.path(here::here(), "_industrial/data/mpca_fuel_emissions.RDS")) %>% ungroup()

fh_mpca <- mpca_emissions %>% filter(grepl("Flint Hills", source_name), ctu_name == "Rosemount") %>% 
  group_by(fuel_category, inventory_year) %>% 
  summarize(value_emissions = sum(value_emissions)) %>% 
  mutate(source = "MPCA",
         fuel_category = if_else(fuel_category == "Other Fuels - Gaseous",
                                 "Other",
                                 fuel_category
         ))

fh_ghgrp <- subpart_c_emissions %>% filter(grepl("Flint Hills", facility_name)) %>% 
  group_by(general_fuel_type, reporting_year) %>% 
  summarize(value_emissions = sum(values_emissions)) %>% 
  mutate(source = "GHGRP") %>% 
  rename(inventory_year = reporting_year, fuel_category = general_fuel_type)

fh_flight <- flight_emissions %>% filter(grepl("Flint Hills", facility_name))

# Combine the two datasets
fh_combined <- bind_rows(fh_mpca %>% filter(!grepl("Coal", fuel_category)), fh_ghgrp)

# Create the bar chart
ggplot(fh_combined, aes(x = as.factor(inventory_year), y = value_emissions, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~fuel_category) +
  labs(
    title = "Comparison of Emissions Across Fuel Categories",
    x = "Inventory Year",
    y = "Value Emissions",
    fill = "Fuel Category"
  ) +
  theme_minimal()
