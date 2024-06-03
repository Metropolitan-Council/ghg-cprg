# Import necessary info
source("R/_load_pkgs.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
score_data <- readRDS("_waste/data/mpca_score.RDS")

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

# pull in and calculate f_rec ----

# Source: EPA State Inventory Solid Waste tool, methane flaring data 

# flaring_natl_data <- read_csv(file.path(here::here(), "_waste/data-raw/solid-waste-module-flaring.csv"))%>%  
#   row_to_names(row_number = 1) 
# flaring_natl_data <- data.frame(t(flaring_natl_data)) %>%  
#   row_to_names(row_number = 1) 


flaring_data <- readxl::read_xlsx("_waste/data-raw/solid_waste_flaring.xlsx",
                                  range = "A2:AG54") %>% 
  rename(State = 1) %>% 
  filter(State == "MN") 

  
flaring_data <- data.frame(t(flaring_data)) %>% 
  rownames_to_column("Year") %>% 
  rename("LFGTE MMT CH4" = t.flaring_data.)

flaring_data <- flaring_data[-1,]

# need to:
# check units (mmt or mt?)
# multiply by population percentages to scale down

landfill_data <- score_data %>% 
  filter(Method == "Landfill") %>% 
  select(County,
         Year, 
         Tons)
# join f_rec by county and year

ch4_yearly <- purrr::pmap(landfill_data, calc_landfill_emissions)
ch4_emissions <- bind_rows(ch4_yearly)
