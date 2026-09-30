# Compare prior WI county energy emissions (2005/2021 point estimates) with the
# new temporal series (03_compile_wi_electricity_activity.R,
# 03_compile_wi_natgas_activity.R). Read-only; produces a ggplot object.

source("R/_load_pkgs.R")

read_energy <- function(file) read_rds(here("_energy", "data", file))

old <- bind_rows(
  read_energy("wisconsin_county_ElecEmissions.RDS") %>%
    ungroup() %>%
    transmute(emissions_year = year, county_name, sector = "Electricity",
              emissions_metric_tons_co2e),
  read_energy("wisconsin_county_GasEmissions.RDS") %>%
    ungroup() %>%
    transmute(emissions_year = year, county_name, sector = "Natural gas",
              emissions_metric_tons_co2e)
) %>%
  mutate(version = "Prior estimate")

new <- bind_rows(
  read_energy("WI_county_elec_emissions.RDS"),
  read_energy("WI_county_natgas_emissions.RDS")
) %>%
  transmute(emissions_year, county_name, sector, emissions_metric_tons_co2e,
            version = "Current pipeline")

# Anchor-year differences
old %>%
  inner_join(new, by = c("emissions_year", "county_name", "sector"),
             suffix = c("_old", "_new")) %>%
  mutate(pct_change = 100 * (emissions_metric_tons_co2e_new /
                               emissions_metric_tons_co2e_old - 1)) %>%
  select(sector, county_name, emissions_year,
         emissions_metric_tons_co2e_old, emissions_metric_tons_co2e_new, pct_change) %>%
  arrange(sector, county_name, emissions_year) %>%
  print()

wi_energy_comparison_plot <- ggplot(
  new,
  aes(emissions_year, emissions_metric_tons_co2e / 1000)
) +
  geom_line(aes(color = version)) +
  geom_point(data = old, aes(color = version), size = 3) +
  facet_grid(sector ~ county_name, scales = "free_y") +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  labs(
    x = NULL, y = "Thousand metric tons CO2e", color = NULL,
    title = "Wisconsin county energy emissions: prior vs. current"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

wi_energy_comparison_plot
