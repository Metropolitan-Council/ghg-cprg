# Import and clean MPCA waste allocation data
source("R/_load_pkgs.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
## MPCA SCORE ----
# Summary data collected from https://public.tableau.com/app/profile/mpca.data.services/viz/SCOREOverview/1991-2021SCORE

score_summary <- read_csv(file.path(here::here(), "_waste/data-raw/score_summary.csv"))

# to do: replace mn_counties with general-use list

# filter to only counties in 9-county MN region, for the year 2021

score_filtered <- score_summary %>%
  filter(
    County %in% cprg_county$NAME,
    Year %in% 2005:2021
  ) %>%
  select(
    County,
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

## Methane Commitment model ----

# emissions = MSW * L_0 * (1-f_rec) * (1-OX)

# variables:
# assume all variables from GHGP unless otherwise noted

# methane generation potential (L_0) = MCF * DOC * DOC_f * F * 16/12 ----

# methane correction factor (MCF)
# assuming landfills managed well, semi-aerobic (see GHG Protocol)
mcf = 0.5

# degradable organic carbon (DOC)
# talk to Liz about how to use the spreadsheet in the pdf.
# use equation from IPCC not GHGP

# fraction of degradable organic carbon degraded (DOC_f)
doc_f = 0.6

# fraction of methane in landfill gas (F)
f = 0.5

l = mcf * doc * doc_f * f * 16/12

# other variables ----

# MSW - yearly
# fraction of methane recovered (f_rec) - yearly

# oxidation factor (OX)
ox = 0.1 # for well-managed landfills

# function ----

calc_landfill_emissions <- function(County, Year, Tons, f_rec){
  mcf = 0.5
  # calc doc here
  # placeholder value:
  doc = 0.5 #PLACEHOLDER
  doc_f = 0.6
  f = 0.5
  l_0 = mcf * doc * doc_f * f * 16/12
  ox = 0.1
  msw = Tons * 0.90718474 # conversion factor tons to metric tons
  
  emissions = msw * l_0 * (1-f_rec) * (1-ox)
  return(tibble(ch4_emissions = emissions, year = Year, county = County))
}

# pull in and calculate f_rec

landfill_data <- score_filtered %>% 
  filter(Method == "Landfill") %>% 
  select(County,
         Year, 
         Tons)
# join f_rec by county and year

ch4_yearly <- purrr::pmap(landfill_data, calc_landfill_emissions)
ch4_emissions <- bind_rows(ch4_yearly)
