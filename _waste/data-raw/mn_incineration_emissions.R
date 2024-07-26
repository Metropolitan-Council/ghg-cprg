# calculate emissions from WTE and onsite burning using IPCC equations and MPCA data
source("R/_load_pkgs.R")
if (!exists("score_data")) {
  score_data <- readRDS("_waste/data-raw/mpca_score_allyrs.RDS")
}

# assign factors
fcc <- .4 # fraction of carbon content in MSW, IPCC default
ffc <- .4 # fraction of fossil carbon in MSW, IPCC default
co2_factor <- fcc * ffc * 44 / 12
co2_efficiency_wte <- .95 # efficiency of combustion for incineration, IPCC default
co2_efficiency_onsite <- .71 # efficiency of combustion for onsite burning, GHG Protocol default (IPCC does not provide one)
n2o_emissions_factor_wte <- 50 # aggregate emissions factor for incineration, g N2O/metric tons waste, GHG Protocol default
n2o_emissions_factor_onsite <- 150 # aggregate emissions factor for open burning, g N2O/metric tons waste, GHG Protocol default

incin_factors <- tibble(
  Method = c("WTE", "Onsite"),
  co2 = co2_factor * c(co2_efficiency_wte, co2_efficiency_onsite),
  n2o = 10^(-6) * c(n2o_emissions_factor_wte, n2o_emissions_factor_onsite)
)

incineration_emissions <- score_data %>%
  filter(Method %in% c("WTE", "Onsite")) %>%
  select(
    County,
    Method,
    Year,
    `Metric Tons`
  ) %>%
  left_join(incin_factors, by = join_by(Method)) %>%
  mutate(
    total_co2 = `Metric Tons` * co2,
    total_n2o = `Metric Tons` * n2o
  ) %>%
  select(
    County,
    Method,
    Year,
    total_co2,
    total_n2o
  )

# write meta
incineration_emissions_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "County", class(incineration_emissions$County), "Name of county",
    "Method", class(incineration_emissions$Method), "Subcategory-specific source (e.g., Landfill)",
    "Year", class(incineration_emissions$Year), "Emissions estimation year",
    "total_co2", class(incineration_emissions$total_co2), 
    "Annual total emissions, in metric tons CO~2~, attributed to the given county",
    "total_n2o", class(incineration_emissions$total_n2o), 
    "Annual total emissions, in metric tons N~2~O, attributed to the given county"
  )

# save RDS
saveRDS(incineration_emissions, "_waste/data/mn_incineration_emissions.RDS")
saveRDS(incineration_emissions_meta, "_waste/data/mn_incineration_emissions_meta.RDS")
