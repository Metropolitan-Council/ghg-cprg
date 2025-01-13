source("R/_load_pkgs.R")

# use MPCA SCORE data for 1991-2021 ----
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
score_summary <- read_csv(file.path(here::here(), "_waste/data-raw/solid_waste/score_summary.csv"))
mpca_waste_composition <- readRDS(file.path(here::here(), "_waste/data-raw/solid_waste/mpca_waste_composition.RDS"))
cprg_population_2005 <- readRDS(file.path(here::here(), "_meta/data/cprg_population_2005.RDS"))
# filter to only counties in 9-county MN region, for years between 2005 and 2021

mt_conversion_factor <- 0.90718474
# waste_per_cap = 0.23 # estimated MN per cap value for 1990
#
# historical_est <- cprg_population_2005 %>%
#   filter(STATEFP == 27) %>%
#   mutate(
#     value_activity = population*waste_per_cap*mt_conversion_factor,
#     source = "Landfill",
#     units_activity = "metric tons MSW"
#   ) %>%
#   slice(rep(1:n(), each = 41)) %>%
#   cbind(inventory_year = 1950:1990) %>%
#   select(
#     geoid = GEOID,
#     source,
#     inventory_year,
#     value_activity,
#     units_activity
#   )

mpca_score_long <- score_summary %>%
  # group_by(Year, Method) %>%
  # mutate(state_total = sum(Tons) * mt_conversion_factor) %>%
  filter(
    County %in% cprg_county$county_name
  ) %>%
  mutate(
    value_activity = Tons * mt_conversion_factor, # convert short tons to metric tons (for consistency with IPCC values)
    units_activity = "metric tons MSW"
  ) %>%
  left_join(cprg_county, by = join_by(County == county_name)) %>%
  mutate(Method = ifelse(Method == "WTE", "Waste to energy", Method)) %>%
  select(
    geoid,
    source = Method,
    inventory_year = Year,
    value_activity,
    units_activity
    # state_total
  ) %>%
  ungroup() %>%
  filter(
    source == "Landfill"
  )
# %>%
#   bind_rows(
#     historical_est
#   )

# calculate l0 ----
# methane correction factor (MCF)
# assuming landfills managed well, semi-aerobic (see GHG Protocol)
mcf <- 0.5
# fraction of degradable organic carbon degraded (DOC_f)
doc_f <- 0.6
# fraction of methane in landfill gas (F)
f <- 0.5

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

# first order decay ----
k <- 0.05
# default methane generation rate from IPCC
fod_calc <- mpca_score_long %>%
  mutate(
    msw_l0 = value_activity * l_0,
    ddoc_accumulated = 0,
    value_emissions = 0
  )

fod_county_list <- list()

for (i in 1:9) {
  county <- cprg_county$geoid[i]
  fod_county <- fod_calc %>%
    filter(geoid == county)
  for (r in 1:31) {
    # ddoc accumulated
    if (r == 1) {
      fod_county$ddoc_accumulated[r] <- fod_county$msw_l0[r]
    } else {
      fod_county$ddoc_accumulated[r] <- (fod_county$ddoc_accumulated[r - 1] * exp(-k)
        + fod_county$msw_l0[r])
    }
    # ddoc decomposed (in mmt? ch4)
    if (r == 1) {
      fod_county$value_emissions[r] <- 0
    } else {
      fod_county$value_emissions[r] <- fod_county$ddoc_accumulated[r - 1] * (1 - exp(-k))
    }
  }
  fod_county_list[[i]] <- fod_county
}

fod_emissions <- bind_rows(fod_county_list)

# for final numbers: multiply by 1-ox
ox <- 0.1
if (!exists("gwp")) {
  source(file.path(here::here(), "R/global_warming_potential.R"))
}

fod_emissions <- fod_emissions %>%
  mutate(
    value_emissions_fod = value_emissions * (1 - ox) * gwp$ch4,
    units_emissions = "Metric tonnes CO2e"
  ) %>%
  select(
    geoid,
    inventory_year,
    value_emissions_fod
  )

# graph compare ----

solid_waste <- readRDS(file.path(here::here(), "_waste/data/final_solid_waste_allyrs.RDS"))

waste_compare <- solid_waste %>%
  filter(
    source == "Landfill"
  ) %>%
  left_join(
    fod_emissions,
    by = join_by(geoid, inventory_year)
  )

ggplot(waste_compare, aes(x = inventory_year, y = value_emissions, color = geoid)) +
  geom_line(linetype = 2) +
  geom_line(aes(y = value_emissions_fod))
