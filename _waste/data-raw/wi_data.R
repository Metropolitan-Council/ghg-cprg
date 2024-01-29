# allocate WI state emissions by county population

source("R/_load_pkgs.R")
# library(rvest)
# library(janitor)
# library(tibble)
library(dplyr)

# from https://widnr.widen.net/view/pdf/o9xmpot5x7/AM610.pdf?t.download=true
# WI GHG Emissions Inventory from the DNR, 2018 data
wi_total_emissions <- 2.2 * 10^6 # in mtco2e
#
# wi_pop_read <- read_html("https://www.census.gov/quickfacts/fact/table/piercecountywisconsin,stcroixcountywisconsin,WI/POP010220")
# wi_pop_vector <- wi_pop_read %>% html_elements("td") %>% html_text2()
# wi_pop <- split(wi_pop_vector, ceiling(seq_along(wi_pop_vector)/4)) %>%
#   data.frame() %>%
#   janitor::row_to_names(1) %>%
#   dplyr::select("Population, Census, April 1, 2020")
# wi_pop$County <- c("Pierce", "St. Croix", "Wisconsin Total")
#
# wi_emissions_allocated <- wi_pop %>%
#   dplyr::rename(Population = "Population, Census, April 1, 2020") %>%
#   dplyr::mutate(Population = as.integer(Population))%>%
#   dplyr::mutate(percent_pop = Population/wi_emissions_allocated$Population[3])

# Unfortunately, the above method renders the data impossible to convert into numeric.

# population values from 2020 census
County <- c("Pierce", "St. Croix", "Wisconsin total")
Population <- c(42212, 93536, 5893718)
wi_pop <- data.frame(County, Population)
wi_emissions <- wi_pop %>%
  dplyr::mutate(
    percent_pop = Population / wi_pop$Population[3],
    emissions_metric_tons_co2e = percent_pop * wi_total_emissions
  )

wi_emissions_meta <- tribble(
  ~Column, ~Class, ~Description,
  "County", class(wi_emissions$County), "WI county of waste origin, including state total",
  "Population", class(wi_emissions$Population), "Population of WI county (2020)",
  "percent_pop", class(wi_emissions$percent_pop), "Percent of WI population in county (2020)",
  "emissions_metric_tons_co2e", class(wi_emissions$emissions_metric_tons_co2e),
  "Total waste emissions allocated to county based on 2018 totals"
)

saveRDS(wi_emissions, paste0("_waste/data/wi_emissions.RDS"))
saveRDS(wi_emissions_meta, paste0("_waste/data/wi_emissions_meta.RDS"))

# to do:
# put both datasets in same format
# (wi method = Landfill, include WTE in landfill)
# combine to final emissions dataset
