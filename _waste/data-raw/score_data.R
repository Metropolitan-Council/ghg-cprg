# Import and clean MPCA waste allocation data


source("R/_load_pkgs.R")
library(readr)

score_summary <- read_csv("_waste/data-raw/score_summary.csv")

# replace with general-use list
mn_counties <- c("Anoka", 
                 "Carver",
                 "Chisago",
                 "Dakota",
                 "Hennepin",
                 "Ramsey",
                 "Scott",
                 "Sherburne",
                 "Washington")

score_filtered <- score_summary %>%
  filter(County %in% mn_counties,
         Year == "2021") %>%
  select(County,
         "Management Category" ="Mangement Method",
         Method,
         Year,
         Tons)

# append emissions factors
emissions_factors <- readxl::read_xlsx("_waste/data-raw/ghg-emission-factors-hub-2021.xlsx")

emissions_factors_cleaned <- emissions_factors %>%
  select(2:8)%>%
  slice(397:457)%>%
  row_to_names(row_number = 1)%>%
  rename(Recycled = RecycledA,
         Landfilled = LandfilledB,
         Combusted = CombustedC,
         Composted = CompostedD)

# export
saveRDS(score_filtered, paste0("_waste/data-raw/mpca_score_waste.RDS"))
saveRDS(emissions_factors_cleaned, paste0("_waste/data-raw/waste_emissions_factors.RDS"))

