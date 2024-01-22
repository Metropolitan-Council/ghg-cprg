# Import and clean MPCA waste allocation data


source("R/_load_pkgs.R")
library(readr)

## MPCA SCORE ----
# Summary data collected from https://public.tableau.com/app/profile/mpca.data.services/viz/SCOREOverview/1991-2021SCORE

score_summary <- read_csv("_waste/data-raw/score_summary.csv")

# to do: replace mn_counties with general-use list
mn_counties <- c(
  "Anoka",
  "Carver",
  "Chisago",
  "Dakota",
  "Hennepin",
  "Ramsey",
  "Scott",
  "Sherburne",
  "Washington"
)

# filter to only counties in 9-county MN region, for the year 2021

score_filtered <- score_summary %>%
  filter(
    County %in% mn_counties,
    Year == "2021"
  ) %>%
  select(County,
    "Management Category" = "Mangement Method",
    Method,
    Year,
    Tons
  )

# add score metadata
mpca_score_meta <- tribble(
  ~Column, ~Class, ~Description,
  "County", class(score_filtered$County), "MN county of waste origin",
  "Management Category", class(score_filtered$`Management Category`), "Waste category
  (either M__ Municipal Solid Waste or Combined Recycling and Organics)",
  "Method", class(score_filtered$Method), "Waste disposal method",
  "Year", class(score_filtered$Year), "MPCA SCORE data collection year",
  "Tons", class(score_filtered$Tons), "Tons of waste collected"
)

saveRDS(score_filtered, paste0("_waste/data/mpca_score.RDS"))
saveRDS(mpca_score_meta, paste0("_waste/data/mpca_score_meta.RDS"))

## Emissions Factors ----

# emissions factors from https://www.epa.gov/climateleadership/ghg-emission-factors-hub
# cleaning to fix formatting
emissions_factors <- readxl::read_xlsx("_waste/data-raw/ghg-emission-factors-hub-2021.xlsx")

emissions_factors_cleaned <- emissions_factors %>%
  select(2:8) %>%
  slice(397:457) %>%
  row_to_names(row_number = 1) %>%
  rename(
    Recycled = RecycledA,
    Landfilled = LandfilledB,
    Combusted = CombustedC,
    Composted = CompostedD
  )

# metadata

saveRDS(emissions_factors_cleaned, paste0("_waste/data-raw/waste_emissions_factors.RDS"))

## Emissions ----

# join emissions factors to score data
# landfill = Mixed MSW: Landfilled
# msw compost = Mixed MSW: Composted (NA)
# onsite = Mixed MSW: Landfilled
# organics = Mixed Organics: Composted
# recycling = Mixed Recyclables: Recyclesd
# WTE = Mixed MSW: Combusted
# MSW Compost removed because it is empty - remember to test this

score_final <- score_filtered %>%
  mutate( # emissions factor in metric tons co2/short tons waste
    emissions_factor =
      case_when(
        Method == "Landfill" ~ as.numeric(filter(emissions_factors_cleaned, Material == "Mixed MSW") %>%
          select(Landfilled)),
        Method == "MSW Compost" ~ as.numeric(filter(emissions_factors_cleaned, Material == "Mixed MSW") %>%
          select(Composted)),
        Method == "Onsite" ~ as.numeric(filter(emissions_factors_cleaned, Material == "Mixed MSW") %>%
          select(Landfilled)),
        Method == "Organics" ~ as.numeric(filter(emissions_factors_cleaned, Material == "Mixed Organics") %>%
          select(Composted)),
        Method == "Recycling" ~ as.numeric(filter(emissions_factors_cleaned, Material == "Mixed Recyclables") %>%
          select(Recycled)),
        Method == "WTE" ~ as.numeric(filter(emissions_factors_cleaned, Material == "Mixed MSW") %>%
          select(Combusted))
      ),
    # emissions in metric tons co2e
    emissions_metric_tons_co2e = Tons * emissions_factor
  ) %>%
  filter(!Method == "MSW Compost") # removing rows filled with 0s and NAs


# export
saveRDS(score_final, paste0("_waste/data/waste_emissions.RDS"))
