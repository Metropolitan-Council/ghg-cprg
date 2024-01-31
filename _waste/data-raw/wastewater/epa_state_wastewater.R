source("R/_load_pkgs.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")

mn_mpca <- readxl::read_xlsx('_waste/data-raw/wastewater/mpca-mn-wastewater.xlsx')
mn_state_est <- as.numeric(mn_mpca[mn_mpca$Sector == 'Grand Total','2020'])

mn_epa <- readr::read_csv('_waste/data-raw/wastewater/epa-mn-wastewater.csv')
mn_epa_est <- as.numeric(mn_epa[12,33]) * 10^6
mn_epa_ch4_est <- as.numeric(mn_epa[5,33]) * 10^6

wi_epa <- readr::read_csv('_waste/data-raw/wastewater/epa-wi-wastewater.csv')
wi_epa_est <- as.numeric(wi_epa[12,33]) * 10^6
wi_state_est <- 600000 # taken from WI state inventory document (https://widnr.widen.net/view/pdf/o9xmpot5x7/AM610.pdf?t.download=true)

wi_2020 <- tidycensus::get_decennial('county',state = 'WI',variables = 'P1_001N',year = 2020) %>% 
  mutate(pop_percent = value/sum(value)) %>% 
  filter(GEOID %in% cprg_county$GEOID) %>% 
  mutate(epa_wastewater_co2e_metric_tons = pop_percent * wi_epa_est,
         state_wastewater = pop_percent * wi_state_est)

mn_2020 <- tidycensus::get_decennial('county',state = 'MN',variables = 'P1_001N',year = 2020) %>% 
  mutate(pop_percent = value/sum(value)) %>% 
  filter(GEOID %in% cprg_county$GEOID) %>% 
  mutate(epa_wastewater_co2e_metric_tons = pop_percent * mn_epa_est,
         state_wastewater = pop_percent * mn_state_est)

ww_epa <- rows_append(wi_2020 %>% dplyr::select(GEOID, NAME, epa_wastewater_co2e_metric_tons),
                      mn_2020 %>% dplyr::select(GEOID, NAME, epa_wastewater_co2e_metric_tons))

saveRDS(ww_epa, "_waste/data-raw/wastewater/epa_wastewater.RDS")

#compare MN numbers to met council estimates

metc_wastewater <- readRDS("_waste/data-raw/wastewater/metc_wastewater.RDS") 


cty_wastewater <- metc_wastewater %>% 
  group_by(year,COUNTY_NAM,class) %>% 
  summarize(co2e = sum(emissions_metric_tons_co2e))

comp_2020 <- cty_wastewater %>% 
  filter(year == 2020) %>% 
  left_join(., mn_2020 %>% 
              )
