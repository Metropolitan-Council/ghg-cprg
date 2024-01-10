# Import and clean MPCA waste allocation data


source("R/_load_pkgs.R")
library(readr)

score_summary <- read_csv("_waste/data-raw/score_summary.csv")

score_filtered <- score_summary %>%
  filter(Region == "Metropolitan Area",
         Year == "2021") %>%
  select(County,
         "Management Category" ="Mangement Method",
         Method,
         Year,
         Tons)

# append emissions factors
