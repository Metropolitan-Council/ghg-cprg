# Import and clean MPCA waste allocation data
source("R/_load_pkgs.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
## MPCA SCORE ----
# Summary data collected from https://public.tableau.com/app/profile/mpca.data.services/viz/SCOREOverview/1991-2021SCORE

score_summary <- read_csv(file.path(here::here(), "_waste/data-raw/score_summary.csv"))

# to do: replace mn_counties with general-use list

# filter to only counties in 9-county MN region, for the year 2021

score_filtered_2021 <- score_summary %>%
  filter(
    County %in% cprg_county$NAME,
    Year == "2021"
  ) %>%
  select(
    County,
    "Management Category" = "Mangement Method",
    Method,
    Year,
    Tons
  )

# add score metadata
mpca_score_2021_meta <- tribble(
  ~Column, ~Class, ~Description,
  "County", class(score_filtered_2021$County), "MN county of waste origin",
  "Management Category", class(score_filtered_2021$`Management Category`), "Waste category
  (either M__ Municipal Solid Waste or Combined Recycling and Organics)",
  "Method", class(score_filtered_2021$Method), "Waste disposal method",
  "Year", class(score_filtered_2021$Year), "MPCA SCORE data collection year (this version of the dataset only includes 2021)",
  "Tons", class(score_filtered_2021$Tons), "Tons of waste collected"
)

saveRDS(score_filtered_2021, paste0("_waste/data/mpca_score_2021.RDS"))
saveRDS(mpca_score_2021_meta, paste0("_waste/data/mpca_score_2021_meta.RDS"))

## Emissions Factors ----

epa_ghg_factor_hub <- readRDS("_meta/data/epa_ghg_factor_hub.RDS")
## Emissions ----

# join emissions factors to score data
# landfill = Mixed MSW: Landfilled
# msw compost = Mixed MSW: Composted (NA)
# onsite = Mixed MSW: Landfilled
# organics = Mixed Organics: Composted
# recycling = Mixed Recyclables: Recyclesd
# WTE = Mixed MSW: Combusted
# MSW Compost removed because it is empty - remember to test this

waste_factors <- epa_ghg_factor_hub$waste

score_final_2021 <- score_filtered_2021 %>%
  rowwise() %>%
  mutate( # emissions factor in metric tons co2/short tons waste
    emissions_factor =
      case_when(
        Method == "Landfill" ~ as.numeric(filter(waste_factors, Material == "Mixed MSW", name == "Landfilled") %>%
          magrittr::extract2("value")), # 0.52
        Method == "MSW Compost" ~ as.numeric(filter(waste_factors, Material == "Mixed MSW", name == "Composted") %>%
          magrittr::extract2("value")), # NA
        Method == "Onsite" ~ as.numeric(filter(waste_factors, Material == "Mixed MSW", name == "Landfilled") %>%
          magrittr::extract2("value")), # 0.52
        Method == "Organics" ~ as.numeric(filter(waste_factors, Material == "Mixed Organics", name == "Composted") %>%
          magrittr::extract2("value")), # 0.17
        Method == "Recycling" ~ as.numeric(filter(waste_factors, Material == "Mixed Recyclables", name == "Recycled") %>%
          magrittr::extract2("value")), # 0.09
        Method == "WTE" ~ as.numeric(filter(waste_factors, Material == "Mixed MSW", name == "Combusted") %>%
          magrittr::extract2("value")), # 0.43
      ),
    # emissions in metric tons co2e
    emissions_metric_tons_co2e = Tons * emissions_factor
  ) %>%
  filter(!Method == "MSW Compost") # removing rows filled with 0s and NAs

mn_emissions_2021_meta <- tribble(
  ~Column, ~Class, ~Description,
  "County", class(score_final_2021$County), "MN county of waste origin",
  "Management Category", class(score_final_2021$`Management Category`), "Waste category
  (either M__ Municipal Solid Waste or Combined Recycling and Organics)",
  "Method", class(score_final_2021$Method), "Waste disposal method",
  "Year", class(score_final_2021$Year), "MPCA SCORE data collection year",
  "Tons", class(score_final_2021$Tons), "Tons of waste collected",
  "emissions_factor", class(score_final_2021$emissions_factor), "Appropriate emissions factor from EPA",
  "emissions_metric_tons_co2e", class(score_final_2021$emissions_metric_tons_co2e),
  "Calculated emissions in metric tons CO2e"
)

# export
saveRDS(score_final_2021, paste0("_waste/data/mn_emissions_2021.RDS"))
saveRDS(mn_emissions_2021_meta, paste0("_waste/data/mn_emissions_2021_meta.RDS"))
