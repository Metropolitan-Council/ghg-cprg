# calculate emissions from WTE and onsite burning using IPCC equations and MPCA data
source("R/_load_pkgs.R")
if (!exists("mpca_score")) {
  mpca_score <- readRDS("_waste/data-raw/solid_waste/mpca_score_allyrs.RDS")
}

# assign factors
fcc <- .4 # fraction of carbon content in MSW, IPCC default
ffc <- .4 # fraction of fossil carbon in MSW, IPCC default
co2_factor <- fcc * ffc * 44 / 12 # atomic weight of CO2:C
co2_efficiency_wte <- .95 # efficiency of combustion for incineration, IPCC default
co2_efficiency_onsite <- .71 # efficiency of combustion for onsite burning, GHG Protocol default (IPCC does not provide one)
n2o_emissions_factor_wte <- 50 # aggregate emissions factor for incineration, g N2O/metric tons waste, GHG Protocol default
n2o_emissions_factor_onsite <- 150 # aggregate emissions factor for open burning, g N2O/metric tons waste, GHG Protocol default

incin_factors <- tibble(
  source = c("WTE", "Onsite"),
  co2 = co2_factor * c(co2_efficiency_wte, co2_efficiency_onsite),
  n2o = c(n2o_emissions_factor_wte, n2o_emissions_factor_onsite) *
    units::as_units("gram") %>%
    units::set_units("metric_ton") %>% 
    as.numeric()
)

incineration_emissions <- mpca_score %>%
  filter(source %in% c("Waste to energy", "Onsite")) %>%
  left_join(incin_factors, by = join_by(source)) %>%
  mutate(
    "Metric tons CO2" = value_activity * co2,
    "Metric tons N2O" = value_activity * n2o
  ) %>%
  pivot_longer(
    c("Metric tons CO2", "Metric tons N2O"),
    names_to = "units_emissions",
    values_to = "value_emissions"
  ) %>%
  select(
    -c(state_total, co2, n2o)
  )

# write meta
incineration_emissions_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "geoid", class(incineration_emissions$geoid), "5-digit FIPS code",
    "source", class(incineration_emissions$source), "Subcategory-specific source (e.g., Landfill)",
    "inventory_year", class(incineration_emissions$inventory_year), "Emissions estimation year",
    "value_activity", class(incineration_emissions$value_activity), "Activity data value (from SCORE)",
    "units_activity", class(incineration_emissions$units_activity), "Activity data units",
    "value_emissions", class(incineration_emissions$value_emissions), "Emissions value",
    "units_emissions", class(incineration_emissions$units_emissions), "Emissions units",
  )

# save RDS
saveRDS(incineration_emissions, "_waste/data/incineration_MN_allyrs.RDS")
saveRDS(incineration_emissions_meta, "_waste/data/incineration_MN_allyrs_meta.RDS")
