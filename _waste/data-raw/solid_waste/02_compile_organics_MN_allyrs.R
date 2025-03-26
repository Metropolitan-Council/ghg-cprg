# calculate emissions from aerobic composting using IPCC equations and MPCA data.
source("R/_load_pkgs.R")
if (!exists("mpca_score")) {
  mpca_score <- readRDS("_waste/data-raw/solid_waste/mpca_score_allyrs.RDS")
}

ch4_factor_compost <- 10 / 1000 # aggregate emissions factor for aerobic composting, 10 metric tons CH4/thousand metric tons waste, IPCC default
ch4_factor_ad <- 2 / 1000 # aggregate emissions factor for anaerobic digestion, 2 metric tons CH4/thousand metric tons waste, IPCC default
# if we were incorporating methane recovered, that would be added as a column to the dataframe

n2o_factor_compost <- 0.6 / 1000 # aggregate emissions factor for aerobic composting, 0.6 metric tons N2O/thousand metric tons waste, IPCC default
# N2O emissions from anaerobic digestion are assumed negligible

organics_emissions <- mpca_score %>%
  filter(source == "Organics") %>%
  mutate(
    "Metric tons CH4" = value_activity * ch4_factor_compost,
    "Metric tons N2O" = value_activity * n2o_factor_compost
  ) %>%
  pivot_longer(
    c("Metric tons CH4", "Metric tons N2O"),
    names_to = "units_emissions",
    values_to = "value_emissions"
  )

# write meta
organics_emissions_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "geoid", class(organics_emissions$geoid), "5-digit FIPS code",
    "source", class(organics_emissions$source), "Subcategory-specific source (e.g., Landfill)",
    "inventory_year", class(organics_emissions$inventory_year), "Emissions estimation year",
    "value_activity", class(organics_emissions$value_activity), "Activity data value (from SCORE)",
    "units_activity", class(organics_emissions$units_activity), "Activity data units",
    "value_emissions", class(organics_emissions$value_emissions), "Emissions value",
    "units_emissions", class(organics_emissions$units_emissions), "Emissions units",
  )

# save RDS
saveRDS(organics_emissions, "_waste/data/organics_MN_allyrs.RDS")
saveRDS(organics_emissions_meta, "_waste/data/organics_MN_allyrs_meta.RDS")
