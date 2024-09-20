# compare county proportions relative to statewide totals
# for VMT, emissions, and population

source("R/_load_pkgs.R")

# read in pre-processed proportions
dot_vmt_county_proportions <- readRDS("_transportation/data/dot_vmt_county_proportions.RDS")
cprg_county_proportions <- readRDS("_meta/data/cprg_county_proportions.RDS")


county_vmt_pop_emissions <- cprg_county_proportions %>%
  left_join(dot_vmt_county_proportions,
    by = c("geoid", "population_year" = "vmt_year",
      "county_name",
      "state_name" = "state"
    )
  )  %>%
  mutate(
    vmt_per_capita = annual_vmt / county_population,
  ) %>%
  filter(population_year  >= 2005)


county_prop_long <- county_vmt_pop_emissions %>%
  select(
    geoid, county_name, population_year, county_proportion_annual_vmt,
    county_proportion_of_state_pop
  ) %>%
  pivot_longer(4:5,
    names_to = "proportion_base",
    values_to = "value"
  ) %>%
  mutate(proportion_base = case_when(
    proportion_base == "county_proportion_annual_vmt" ~ "VMT",
    proportion_base == "county_proportion_of_state_pop" ~ "population"
  ))





county_prop_long %>%
  group_by(county_name) %>%
  plot_ly(
    type = "scatter",
    mode = "lines+markers",
    x = ~population_year,
    y = ~value,
    symbol = ~proportion_base,
    color = ~county_name
  )
