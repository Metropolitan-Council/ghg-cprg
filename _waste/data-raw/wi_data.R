# allocate WI state emissions by county population

source("R/_load_pkgs.R")

# from https://widnr.widen.net/view/pdf/o9xmpot5x7/AM610.pdf?t.download=true
# WI GHG Emissions Inventory from the DNR, 2018 data
# wi_total_emissions <- 2.2 * 10^6 # in mtco2e
# 0.1 from waste combustion
# 2.1 from landfills (accounts for flaring and landfill gas to energy)

cprg_county_proportions <- readRDS("_meta/data/cprg_county_proportions.RDS")

names <- c(Method = "X", `2005` = "X2005", `2018` = "X2018")
wi_inventory <- read.csv(file.path(here::here(), "_waste/data-raw/tabula-wi_inventory_2005_2018.csv")) %>% 
  rename(all_of(names)) %>% 
  filter(
    Method %in% c("Landfills", "Waste Combustion")
  ) %>% 
  pivot_longer(
    cols = !Method,
    names_to = "Year",
    values_to = "emissions_mmt_co2e"
  ) %>% 
  mutate(
    Year = as.numeric(Year),
    emissions_mmt_co2e = as.numeric(emissions_mmt_co2e)
  ) %>% 
  #add 2021 with values == 2018
  rbind(
    tibble(
      Method = c("Landfills", "Waste Combustion"), 
      Year = c(2021, 2021), 
      emissions_mmt_co2e = c(2.1, 0.1)
      )
    ) %>%  
  complete(Method, Year = 2005:2021) %>% 
  group_by(Method) %>% 
  mutate(
    emissions_mmt_co2e = zoo::na.approx(emissions_mmt_co2e, na.rm = FALSE)
  )


wi_pop <- cprg_county_proportions %>%
  filter(
    STATE == "Wisconsin",
    year %in% 2005:2021
  )

wi_emissions <- wi_pop %>%
  # making year numeric. come back to this when we have finalized num v double
  mutate(year = as.numeric(year)) %>% 
  left_join(
    wi_inventory, 
    by = join_by(year == Year), 
    relationship = "many-to-many"
    ) %>% 
  mutate(
    emissions_metric_tons_co2e = emissions_mmt_co2e * county_proportion_of_state_pop * 10^6
  ) %>% 
  select(
    name,
    year,
    Method,
    emissions_metric_tons_co2e
  ) %>% 
  mutate(
    Method = case_when(
      Method == "Landfills" ~ "Landfill",
      Method == "Waste Combustion" ~ "Waste to energy" # this should be incineration.
      # wait for discussion with MPCA to change
    )
  )
  
  # rowwise() %>%
  # dplyr::mutate(
  #   emissions_metric_tons_co2e = county_proportion_of_state_pop * wi_total_emissions
  # ) %>%
  # left_join(cprg_county, by = c("GEOID", "STATE")) %>%
  # select(NAME = name, county_population, county_proportion_of_state_pop, emissions_metric_tons_co2e)

wi_emissions_meta <- tribble(
  ~Column, ~Class, ~Description,
  "name", class(wi_emissions$name), "WI county of waste origin",
  "year", class(wi_emissions$year), "Emissions estimation year",
  "Method", class(wi_emissions$Method), "Solid waste subcategory (e.g., Landfill)",
  "emissions_metric_tons_co2e", class(wi_emissions$emissions_metric_tons_co2e),
  "Total waste emissions allocated to county based on state inventory totals"
)

saveRDS(wi_emissions, paste0("_waste/data/wi_emissions.RDS"))
saveRDS(wi_emissions_meta, paste0("_waste/data/wi_emissions_meta.RDS"))
