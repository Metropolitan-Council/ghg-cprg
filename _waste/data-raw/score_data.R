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

# join emissions factors to score data
# landfill = Mixed MSW: Landfilled
# msw compost = Mixed MSW: Composted (NA)
# onsite = Mixed MSW: Landfilled
# organics = Mixed Organics: Composted
# recycling = Mixed Recyclables: Recyclesd
# WTE = Mixed MSW: Combusted
score_final <- score_filtered %>%
  mutate( # emissions factor in metric tons co2/short tons waste
    emissions_factor =
           case_when(Method == "Landfill" ~ as.numeric(filter(emissions_factors_cleaned, Material == "Mixed MSW") %>% 
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
saveRDS(score_final, paste0("_waste/data-raw/mpca_score_waste.RDS"))

