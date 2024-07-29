# Calculate landfill emissions using IPCC methane commitment model
# Data from MPCA Score
source("R/_load_pkgs.R")
if (!exists("mpca_score")) {
  mpca_score <- readRDS("_waste/data-raw/solid_waste/mpca_score_allyrs.RDS")
}
mpca_waste_composition <- readRDS(file.path(here::here(), "_waste/data-raw/solid_waste/mpca_waste_composition.RDS"))
# methane_recovery_mn <- readRDS("_waste/data-raw/epa_mn_methane_recovery.RDS")

## Methane Commitment model ----

# variables: see documentation for sources ----

# methane correction factor (MCF)
# assuming landfills managed well, semi-aerobic (see GHG Protocol)
mcf <- 0.5
# fraction of degradable organic carbon degraded (DOC_f)
doc_f <- 0.6
# fraction of methane in landfill gas (F)
f <- 0.5
# oxidation factor (OX)
ox <- 0.1 # for well-managed landfills


# Calculate DOC using IPCC equation (see documentation)
# waste composition from MPCA report https://www.pca.state.mn.us/sites/default/files/w-sw1-60.pdf
# cleaned in _waste/data-raw/clean_tabula_tables.R

ipcc_doc_factors <- tibble(
  Category = c("Paper", "Textiles", "Organics (Non-Food)", "Organics (Food)", "Wood"),
  Factor = c(0.4, 0.4, 0.17, 0.15, 0.3)
)

doc_sum <- mpca_waste_composition %>%
  inner_join(ipcc_doc_factors, by = join_by(Category)) %>%
  mutate(doc_content = Mean * Factor) %>%
  summarize(doc_total = sum(doc_content), degradable_fraction = sum(Mean))

doc <- doc_sum$doc_total

# methane generation potential
l_0 <- mcf * doc * doc_f * f * 16 / 12

# landfill data from MPCA SCORE report. Transformed to metric tons in mn_read_mpca_score ----
# landfill_data <- mpca_score %>%
#   filter(Method == "Landfill") %>%
#   select(
#     County,
#     Year,
#     `Metric Tons`,
#     Method
#   )

# rec (emissions recovered through flaring/landfill gas to energy) to be added later
# join both lines of f_rec by county and year
# calculate sum (flaring + lfgte = rec)

# calculate emissions = ((Tons * l_0) - rec) * (1-ox)

# rec not included for this version; using emissions = (Tons * l_0) * (1-ox)
landfill_emissions <- mpca_score %>%
  filter(source == "Landfill") %>% 
  # left_join(methane_recovery_mn, by = join_by(County, Year, Method)) %>%
  mutate(
    value_emissions = (value_activity * l_0) * (1 - ox),
    units_emissions = "Tonnes CH4"
  ) %>%
  select(
    -state_total
  )

# write meta
landfill_emissions_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "geoid", class(landfill_emissions$geoid), "5-digit FIPS code",
    "source", class(landfill_emissions$source), "Subcategory-specific source (e.g., Landfill)",
    "inventory_year", class(landfill_emissions$inventory_year), "Emissions estimation year",
    "value_activity", class(landfill_emissions$value_activity), "Activity data value (from SCORE)",
    "units_activity", class(landfill_emissions$units_activity), "Activity data units",
    "value_emissions", class(landfill_emissions$value_emissions), "Emissions value",
    "units_emissions", class(landfill_emissions$units_emissions), "Emissions units",
  )

# save RDS
saveRDS(landfill_emissions, "_waste/data/landfill_MN_allyrs.RDS")
saveRDS(landfill_emissions_meta, "_waste/data/landfill_MN_allyrs_meta.RDS")
