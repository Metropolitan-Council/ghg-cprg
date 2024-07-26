# Calculate landfill emissions using IPCC methane commitment model
# Data from MPCA Score
source("R/_load_pkgs.R")
if (!exists("score_data")) {
  score_data <- readRDS("_waste/data-raw/mpca_score_allyrs.RDS")
}
mpca_waste_composition <- readRDS(file.path(here::here(), "_waste/data-raw/mpca_waste_composition.RDS"))
# methane_recovery_mn <- readRDS("_waste/data/methane_recovery_mn.RDS")

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

l_0 <- mcf * doc * doc_f * f * 16 / 12

# landfill data from MPCA SCORE report. Transformed to metric tons in mn_read_score_data ----
landfill_data <- score_data %>%
  filter(Method == "Landfill") %>%
  select(
    County,
    Year,
    `Metric Tons`,
    Method
  )

# rec (emissions recovered through flaring/landfill gas to energy) to be added later
# join both lines of f_rec by county and year
# calculate sum (flaring + lfgte = rec)

# calculate emissions = ((Tons * l_0) - rec) * (1-ox)

# rec not included for this version; using emissions = (Tons * l_0) * (1-ox)
landfill_emissions <- landfill_data %>%
  # left_join(methane_recovery_mn, by = join_by(County, Year, Method)) %>%
  mutate(
    total_ch4 = (`Metric Tons` * l_0) * (1 - ox)
  ) %>%
  select(
    County,
    Method,
    Year,
    total_ch4
  )

# write meta
landfill_emissions_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "County", class(landfill_emissions$County), "Name of county",
    "Method", class(landfill_emissions$Method), "Subcategory-specific source (e.g., Landfill)",
    "Year", class(landfill_emissions$Year), "Emissions estimation year",
    "total_ch4", class(landfill_emissions$total_ch4), "Annual total landfill emissions, in metric tons CH~4~, attributed to the given county"
  )

# save RDS
saveRDS(landfill_emissions, "_waste/data/mn_landfill_emissions.RDS")
saveRDS(landfill_emissions_meta, "_waste/data/mn_landfill_emissions_meta.RDS")
